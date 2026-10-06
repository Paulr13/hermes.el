;;; hermes-prompts.el --- Minibuffer handlers for approval/clarify/secret/sudo -*- lexical-binding: t; -*-

;; Author: Giovanni Crisalfi
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Watches the persistent state's `pending' slot.  When it becomes non-nil,
;; schedules a minibuffer interaction (via `run-at-time' 0 so the renderer
;; finishes its work first), then answers the gateway's server→client
;; request with a response frame (`hermes-rpc-respond') and clears the
;; pending slot.  The gateway sends every prompt as a JSON-RPC request
;; (`approval'/`clarify'/`sudo'/`secret' — see `hermes-events-server-requests');
;; there are no `*.respond' RPCs anymore.  `request.cancel' events clear a
;; matching card via the reducer.

;;; Code:

(require 'cl-lib)
(require 'hermes-rpc)
(require 'hermes-state)

(defvar-local hermes--pending-active nil
  "Non-nil while a minibuffer interaction is in flight.
Guards against re-entrant prompts when the renderer hook fires again.")

(defun hermes-prompts--session-buffer (sid)
  "Return the live viewer buffer for SID, or nil.
Prefers the org viewer registry; falls back to the comint bench
registry so prompts surface in whichever client is showing SID."
  (let ((buf (or (gethash sid hermes--org-buffers)
                 (and (boundp 'hermes-comint--buffers)
                      (gethash sid hermes-comint--buffers)))))
    (and (buffer-live-p buf) buf)))

(defun hermes-prompts-watch (old new)
  "State-change hook: when pending becomes non-nil, schedule a prompt.
Globally installed by the org viewer mode and the comint bench;
resolves the target buffer via `hermes-prompts--session-buffer' so
the buffer-local re-entry guard `hermes--pending-active' lives with
the buffer that will run the minibuffer interaction."
  (let ((op (and old (hermes-state-pending old)))
        (np (hermes-state-pending new)))
    (when (and np (not (eq op np)))
      (let* ((sid (hermes-state-session-id new))
             (buf (hermes-prompts--session-buffer sid)))
        (when (and buf (not (buffer-local-value 'hermes--pending-active buf)))
          (with-current-buffer buf
            (setq hermes--pending-active t))
          (let ((pending np))
            (run-at-time
             0 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (unwind-protect
                       (hermes--prompts-handle sid pending)
                     (setq hermes--pending-active nil)
                     (hermes-dispatch '(:pending-clear) sid))))))))))))

(defun hermes--prompts-handle (sid pending)
  "Run the right minibuffer prompt for PENDING and dispatch the response."
  (let* ((kind (hermes-pending-kind pending))
         (rid  (hermes-pending-request-id pending))
         (p    (hermes-pending-payload pending)))
    (pcase kind
      ('approval (hermes--prompt-approval sid rid p))
      ('clarify  (hermes--prompt-clarify  rid p))
      ('sudo     (hermes--prompt-sudo     rid))
      ('secret   (hermes--prompt-secret   rid p)))))

(defun hermes--prompts-get (payload key)
  (cond ((hash-table-p payload) (gethash key payload))
        ((null payload) nil)
        (t (plist-get payload key))))

;;;; Approval

(defun hermes--prompt-approval (sid rid payload)
  "Ask the user to allow/deny a tool call, then respond to the request RID.
Canonical choices match the TUI: once, session, always, deny.  SID is
unused (the response frame carries the id, not the session)."
  (let* ((cmd (hermes--prompts-get payload "command"))
         (desc (hermes--prompts-get payload "description"))
         (prompt (format "Approve%s%s? "
                         (if desc (format " (%s)" desc) "")
                         (if cmd (format " [%s]" cmd) "")))
         (choice (condition-case _
                     (read-multiple-choice
                      prompt
                      '((?o "once"    "allow this single invocation")
                        (?s "session" "allow for this session")
                        (?a "always"  "allowlist this pattern permanently")
                        (?n "no"      "deny")))
                   (quit '(?n "no" "deny"))))
         (key (car choice))
         (resp (pcase key
                 (?o "once")
                 (?s "session")
                 (?a "always")
                 (_  "deny"))))
    (hermes-rpc-respond rid (list :choice resp))))

;;;; Clarify

(defun hermes--prompt-choices (payload key)
  "Read KEY from PAYLOAD as a list of choice strings (vector or list)."
  (let ((c (hermes--prompts-get payload key)))
    (cond ((vectorp c) (append c nil))
          ((listp c) c)
          (t nil))))

(defun hermes--prompt-clarify (rid payload)
  "Ask each clarify question of the batch, then respond to request RID.
The frame carries a `questions' list ({qid, question, choices}); the
response frame answers with `{answers: {qid: str}}' in one shot.
Quitting (C-g) cancels the whole request: a response frame without
`answers' is cancel-all per the gateway contract.  The per-question
lock alternative is the `clarify.lock' RPC — the Emacs client doesn't
use it, answers go out atomically."
  (let* ((questions (hermes--prompt-choices payload "questions"))
         (answers   (make-hash-table :test 'equal))
         ;; t when the user quit mid-batch: respond cancel-all below.
         (cancelled (catch 'cancel
                      (dolist (q questions)
                        (let* ((qid (hermes--prompts-get q "qid"))
                               (question (or (hermes--prompts-get q "question") "Clarify:"))
                               (answer
                                (condition-case _
                                    (let ((choices (hermes--prompt-choices q "choices")))
                                      (if choices
                                          (completing-read (concat question " ") choices nil nil)
                                        (read-string (concat question " "))))
                                  (quit (throw 'cancel t)))))
                          (puthash qid answer answers)))
                      (hermes-rpc-respond rid answers)
                      nil)))
    (when cancelled
      ;; Nothing was answered: a frame without `answers' cancels the
      ;; request on the gateway side.
      (hermes-rpc-respond rid (make-hash-table :test 'equal)))))

;;;; Sudo / secret

(defun hermes--prompt-sudo (rid)
  "Read a sudo password and respond to request RID with `{value}'."
  (let ((pw (read-passwd "sudo password: ")))
    (hermes-rpc-respond rid (list :value pw))))

(defun hermes--prompt-secret (rid payload)
  "Read a secret value and respond to request RID with `{value}'."
  (let* ((var (hermes--prompts-get payload "env_var"))
         (hint (or (hermes--prompts-get payload "prompt")
                   (and var (format "Value for %s: " var))
                   "Secret: "))
         (val (read-passwd hint)))
    (hermes-rpc-respond rid (list :value val))))

(provide 'hermes-prompts)
;;; hermes-prompts.el ends here
