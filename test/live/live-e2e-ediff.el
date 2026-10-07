;;; live-e2e-ediff.el --- Live proof: edit-tool diff wires RET to ediff -*- lexical-binding: t; -*-
;; With a real gateway: the agent creates a file then edits it with its
;; file-edit tool.  The committed tool segment must carry an inline_diff,
;; the bench buffer must have that diff text painted with a `keymap'
;; property, and invoking its RET binding (ediff-buffers stubbed) must
;; open buffer A with the OLD content and buffer B visiting the real
;; file.  Also asserts the no-repaint decision (disk unchanged → :ok t).
;; Run: ./test/live/run.sh live-e2e-ediff 500

(require 'hermes-rpc)
(require 'hermes)
(require 'hermes-state)
(require 'hermes-comint)

(setq hermes-rpc-python
      (or (getenv "LIVE_E2E_PYTHON") (getenv "HERMES_DEV_PYTHON")
          (expand-file-name "~/.hermes/venv-current/bin/python")))

(defconst live/ediff-target
  (expand-file-name "live-ediff-target.txt" (temporary-file-directory)))
(defconst live/old-text "alpha")
(defconst live/new-text "beta")
(defconst live/prompt
  (format "Create the file %s containing exactly %s, using your file write/edit tool — NOT the terminal tool. Then edit that same file so it contains exactly %s. Do not print the contents. Reply with just OK."
          live/ediff-target live/old-text live/new-text))

(defvar live/deadline (time-add (current-time) (seconds-to-time 420)))
(defvar live/results nil)
(defvar live/phase 0)
(defvar live/sid nil)
(defvar live/buf nil)
(defvar live/completions 0)
(defvar live/events-seen nil)
(defvar live/tool-names nil)
(defvar live/done-msg "started")

(defconst live/logfile
  (or (getenv "LIVE_E2E_LOG")
      (expand-file-name "live-e2e-ediff.log" temporary-file-directory)))
(ignore-errors (delete-file live/logfile))
(ignore-errors (delete-file live/ediff-target))

(defun live--log (fmt &rest args)
  (let ((line (apply #'format (concat "[" (format-time-string "%H:%M:%S") "] " fmt) args)))
    (append-to-file (concat line "\n") nil live/logfile)))

(defun live--result (k v)
  (setq live/results (cons (cons k v) live/results))
  (live--log "RESULT %s = %S" k v))

(defun live--state () (and live/sid (gethash live/sid hermes--sessions)))

(defun live--submit (text)
  (hermes-rpc-request "prompt.submit" (list :session_id live/sid :text text) #'ignore))

(defun live--install ()
  (hermes--install-hooks)
  (add-hook 'hermes-rpc-event-functions
            (lambda (type sid payload)
              (push type live/events-seen)
              (pcase type
                ("tool.start"
                 (let ((n (plist-get payload :name)))
                   (when (and n (not (member n live/tool-names)))
                     (setq live/tool-names (cons n live/tool-names))
                     (live--log "[event] tool.start %s" n))))
                ("message.complete"
                 (setq live/completions (1+ live/completions))
                 (live--log "[event] message.complete"))
                ("error"
                 (live--log "[event] ERROR sid=%s %S" sid payload))
                ("gateway.start_timeout"
                 (live--log "[event] start_timeout %S" payload))))))

(defun live--inline-diff-tools ()
  "Committed tool segments that carry a non-empty inline diff,
newest message first."
  (when-let ((st (live--state)))
    (let (acc)
      (cl-loop for m across (hermes-state-turns st)
               when (eq (hermes-message-kind m) 'assistant)
               do (cl-loop for s across (hermes-message-segments m)
                           when (and (eq (hermes-segment-type s) 'tool)
                                     (hermes-tool-inline-diff
                                      (hermes-segment-content s)))
                           do (push (hermes-segment-content s) acc)))
      acc)))

(live--install)
(hermes-rpc-start)
(live--log "gateway spawning, creating session")
(condition-case e
    (hermes-comint--create-session
     (lambda (buf)
       (setq live/buf buf
             live/sid (buffer-local-value 'hermes--current-session-id buf))
       (live--log "created sid=%s" live/sid)
       (if live/sid
           (progn (live--submit live/prompt)
                  (setq live/phase 'streaming))
         (setq live/phase 'done live/done-msg "session failed"))))
  (error (live--log "create-session error: %S" e)))

(while (and (not (eq live/phase 'done))
            (time-less-p (current-time) live/deadline))
  (unless (hermes-rpc-live-p)
    (setq live/done-msg (format "gateway died in phase %s" live/phase)
          live/phase 'done))
  (when (and (eq live/phase 'streaming) (>= live/completions 1))
    (if (live--inline-diff-tools)
        (setq live/phase 'done live/done-msg "completed")
      ;; Turn committed but no inline-diff tool yet; wait briefly for a
      ;; second turn (the edit) before giving up at the deadline.
      (when (>= live/completions 2)
        (setq live/phase 'done live/done-msg
              "no inline-diff tool in 2 committed turns"))))
  (unless (eq live/phase 'done)
    (accept-process-output nil 1)))

(when (not (eq live/phase 'done))
  (setq live/done-msg (format "deadline hit in phase %s (completions=%d)"
                              live/phase live/completions)))

;; --- Ground-truth assertions -------------------------------------------
(let* ((tools (live--inline-diff-tools))
       (tool (car tools)))                 ; newest committed inline-diff tool
  (live--result "tool-names-seen" (nreverse live/tool-names))
  (live--result "inline-diff-tools" (and tool (length tools)))
  (when tool
    (let* ((diff (substring-no-properties (hermes-tool-inline-diff tool)))
           (path (hermes-comint--diff-tool-path tool)))
      (live--log "tool=%s diff=%S ctx=%S"
                 (hermes-tool-name tool)
                 (substring-no-properties diff)
                 (let ((c (hermes-tool-context tool)))
                   (and c (substring-no-properties c))))
      (live--result "diff-present" (> (length diff) 0))
      (live--result "path-extracted" (and path (stringp path) (> (length path) 0)))
      (when path
        (live--result "path" (substring-no-properties path))
        (live--result "file-exists" (file-exists-p path))
        ;; Decision: disk content must equal the AFTER side (no repaint).
        (let ((d (hermes-comint--ediff-decision path diff)))
          (live--result "decision-ok" (plist-get d :ok))
          (unless (plist-get d :ok)
            (live--log "decision msg: %s" (plist-get d :msg))))
        ;; Paint: diff text in the bench buffer with a RET keymap.
        (when (buffer-live-p live/buf)
          (with-current-buffer live/buf
            (goto-char (point-min))
            (let ((found (search-forward diff nil t)))
              (live--result "diff-painted" (and found t))
              (when found
                (let* ((km (get-text-property (1- (point)) 'keymap))
                      (cmd (and km (lookup-key km (kbd "RET")))))
                  (live--result "keymap-on-diff" (and km t))
                  (live--result "ret-bound" (and cmd t))
                  (when cmd
                    (let ((opened nil))
                      (cl-letf (((symbol-function 'ediff-buffers)
                                 (lambda (a b) (setq opened (list a b)))))
                        (call-interactively cmd))
                      (if (consp opened)
                          (let ((a (car opened)) (b (cadr opened)))
                            (live--result "ediff-opened" t)
                            (live--result
                             "before-has-old-text"
                             (and (buffer-live-p a)
                                  (string-match-p live/old-text
                                                  (with-current-buffer a
                                                    (buffer-string)))
                                  t))
                            (live--result
                             "b-visits-file"
                             (and (buffer-live-p b)
                                  (equal (buffer-file-name b) path)
                                  t))
                            (live--result
                             "b-has-new-text"
                             (and (buffer-live-p b)
                                  (string-match-p live/new-text
                                                  (with-current-buffer b
                                                    (buffer-string)))
                                  t))
                            (when (buffer-live-p a) (kill-buffer a))
                            (when (buffer-live-p b) (kill-buffer b)))
                        (live--result "ediff-opened" nil)))))))))))))

(live--result "events-types" (nreverse (delete-dups (copy-sequence live/events-seen))))
(live--result "final-phase" (if (eq live/phase 'done) 'done live/phase))
(live--result "sid" live/sid)
(when (buffer-live-p live/buf) (kill-buffer live/buf))
(hermes-rpc-stop)

(let* ((get (lambda (k) (cdr (assoc-string k live/results))))
       (checks (list
                (list "inline-diff-tool-committed" (funcall get "diff-present"))
                (list "path-extracted" (funcall get "path-extracted"))
                (list "file-on-disk" (funcall get "file-exists"))
                (list "decision-ok" (funcall get "decision-ok"))
                (list "diff-painted" (funcall get "diff-painted"))
                (list "keymap-on-diff" (funcall get "keymap-on-diff"))
                (list "ret-bound" (funcall get "ret-bound"))
                (list "ediff-opened" (funcall get "ediff-opened"))
                (list "before-has-old-text" (funcall get "before-has-old-text"))
                (list "b-visits-file" (funcall get "b-visits-file"))
                (list "b-has-new-text" (funcall get "b-has-new-text"))))
       (failed (cl-loop for (k v) in checks unless v collect k)))
  (princ (format "\n=== EDIFF E2E RESULTS (%s) ===\n" live/done-msg))
  (dolist (kv (nreverse live/results))
    (princ (format "%-40s %S\n" (car kv) (cdr kv))))
  (princ (format "\n=== EDIFF E2E: %s ===\n"
                 (if failed
                     (concat "FAIL " (mapconcat #'identity failed " "))
                   "PASS"))))

(princ "\n--- log tail ---\n")
(let ((s (or (ignore-errors (with-temp-buffer (insert-file-contents live/logfile) (buffer-string))) "")))
  (princ (if (> (length s) 3000) (substring s -3000) s)))
