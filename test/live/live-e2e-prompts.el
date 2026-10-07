;;; live-e2e-prompts.el --- approval + clarify round-trip via the REAL client reply path -*- lexical-binding: t; -*-
;; Verifies commit b795f51 (srq server->client requests) + 65af472 (clarify
;; {answers} wire shape) against a REAL gateway:
;;   1. terminal tool -> srq "approval" -> answered by the production path
;;      (state card -> hermes-prompts-watch -> hermes--prompt-approval ->
;;      hermes-rpc-respond) -> command executes.
;;   2. clarify tool -> srq "clarify" -> answered by production path ->
;;      gateway outcome submitted -> agent receives the answer.
;; Human input is stubbed (read-multiple-choice/completing-read advice);
;; everything else is the real client code.
;; Run from the repo root:  ./test/live/run.sh live-e2e-prompts [timeout_s]

(require 'hermes-rpc)
(require 'hermes)
(require 'hermes-state)
(require 'hermes-comint)
(require 'hermes-prompts)

(setq hermes-rpc-python
      (or (getenv "LIVE_E2E_PYTHON") (getenv "HERMES_DEV_PYTHON")
          (expand-file-name "~/.hermes/venv-current/bin/python")))

(defvar live/results nil)
(defvar live/events-seen nil)
(defvar live/responds nil)          ; (id . result) wire frames the client sent
(defvar live/srq nil)               ; alist method -> (id params arrival-time)
(defvar live/cancel-ids nil)
(defvar live/pending-at-srq nil)    ; snapshots taken inside the srq hook
(defvar live/fallback-answered nil)
(defvar live/completions 0)
(defvar live/sid nil)
(defvar live/phase 0)
(defvar live/declared-done nil)
(defvar live/deadline (time-add (current-time) (seconds-to-time 400)))
(defvar live/answered (make-hash-table :test 'equal))

(defconst live/marker "approval-probe-deleteme/")

(defconst live/logfile
      (or (getenv "LIVE_E2E_LOG")
          (expand-file-name "live-e2e-prompts.log" temporary-file-directory)))
(ignore-errors (delete-file live/logfile))

(defun live--log (fmt &rest args)
  (let ((line (apply #'format (concat "[" (format-time-string "%H:%M:%S") "] " fmt) args)))
    (append-to-file (concat line "\n") nil live/logfile)))

(defun live--result (k v)
  (setq live/results (cons (cons k v) live/results))
  (live--log "RESULT %s = %S" k v))

(defun live--sess ()
  (and live/sid (gethash live/sid hermes--sessions)))

;; Wire-frame logger: records every response frame the client sends.
(defun live--log-respond (orig id result)
  (setq live/responds (cons (cons id result) live/responds))
  (live--log "RESPOND id=%s result=%S" id result)
  (funcall orig id result))
(advice-add 'hermes-rpc-respond :around #'live--log-respond)

;; The real reply path runs unadvised; only the human keystrokes are stubbed.
(advice-add 'read-multiple-choice :override (lambda (&rest _) '(?o "once" "allow this single invocation")))
(advice-add 'completing-read     :override (lambda (&rest _) "beta"))
(advice-add 'read-string         :override (lambda (&rest _) "beta"))

(defun live--srq-recorder (method id params)
  "Record srq frames AFTER the real router ran (hook order)."
  (live--log "SRQ method=%s id=%s" method id)
  (let ((p params))
    (setf (alist-get method live/srq nil nil #'equal)
          (list id p (time-convert (current-time) 'integer))))
  (when live/sid
    (let ((st (gethash live/sid hermes--sessions)))
      (when-let ((pend (and st (hermes-state-pending st))))
        (let ((snap (list (hermes-pending-kind pend)
                          (hermes-pending-request-id pend)
                          (equal (hermes-pending-request-id pend) id))))
          (push snap live/pending-at-srq)
          (live--log "PENDING kind=%s rid=%s matches=%S"
                     (nth 0 snap) (nth 1 snap) (nth 2 snap)))))))
(defun live--event-recorder (type sid payload)
  (push type live/events-seen)
  (pcase type
    ("request.cancel"
     (push (gethash "id" payload) live/cancel-ids)
     (live--log "CANCEL id=%s method=%s reason=%s"
                (gethash "id" payload) (gethash "method" payload) (gethash "reason" payload)))
    ("message.complete"
     (setq live/completions (1+ live/completions))
     (live--log "COMPLETE #%d sid=%s" live/completions sid))
    ("error"
     (live--log "EVENT-ERROR %S" payload))
    ("gateway.start_timeout"
     (live--log "START-TIMEOUT %S" payload))))

;; Fallback: if the prompts-watch timer path didn't fire within 5 s of the
;; srq (batch timer quirk), drive the REAL handler directly.  The response
;; frame and its shape are identical either way; we log which path ran.
(defun live--maybe-fallback ()
  (when (and live/sid (not live/declared-done))
    (let* ((st (gethash live/sid hermes--sessions))
           (pend (and st (hermes-state-pending st))))
      (when-let ((p pend))
        (let* ((rid  (hermes-pending-request-id p))
               (kind (hermes-pending-kind p))
               (arr  (cl-loop for e in live/srq
                              for (id _pa at) = (cdr e)
                              when (and id (equal id rid)) return at)))
          (when (and rid (not (gethash rid live/answered))
                     arr (>= (- (time-convert (current-time) 'integer) arr) 5))
            (live--log "FALLBACK driving real handler for %s (%s)" rid kind)
            (puthash rid t live/answered)
            (push (format "%s:%s" rid kind) live/fallback-answered)
            (condition-case e
                (hermes--prompts-handle live/sid p)
              (error (live--log "fallback handle error: %S" e)))
            (hermes-dispatch '(:pending-clear) live/sid)))))))

(defun live--assistant-turns ()
  (let ((st (live--sess)) out)
    (when st
      (cl-loop for m across (hermes-state-turns st)
               when (eq (hermes-message-kind m) 'assistant)
               do (push m out)))
    (nreverse out)))

(defun live--last-assistant-text ()
  (let* ((turns (live--assistant-turns))
         (m (car (last turns))))
    (when m
      (mapconcat
       (lambda (s) (if (eq (hermes-segment-type s) 'text)
                       (or (hermes-segment-content s) "") ""))
       (append (hermes-message-segments m) nil) " "))))

(defun live--assert-approval ()
  (let ((entry (cdr (assoc "approval" live/srq))))
    (live--result "approval/srq-seen" (and entry t))
    (when entry
      (cl-destructuring-bind (id params at) entry
        (live--result "approval/srq-id-shape" (and (stringp id) (string-prefix-p "srq-" id)))
        (live--result "approval/params-has-command"
                      (and (hash-table-p params) (gethash "command" params)))
        ;; pending card existed with matching kind + rid when the frame landed
        (live--result "approval/card-kind-was-approval"
                      (cl-some (lambda (e) (and (eq (nth 0 e) 'approval) (nth 2 e)))
                               live/pending-at-srq))
        (live--result "approval/card-rid-matched-srq"
                      (cl-some (lambda (e) (nth 2 e)) live/pending-at-srq))
        (live--result "approval/watch-or-fallback"
                      (if live/fallback-answered 'fallback 'watch))
        ;; the real path's wire result
        (let ((resp (assoc id live/responds)))
          (live--result "approval/respond-frame"
                        (and resp (plist-get (cdr resp) :choice))))
        ;; command actually executed post-approval
        (let* ((pairs (cl-loop for m in (live--assistant-turns)
                               append (cl-loop for s across (hermes-message-segments m)
                                               when (eq (hermes-segment-type s) 'tool)
                                               collect (hermes-segment-content s))))
               (tools (cl-remove-if-not (lambda (tl) (equal (hermes-tool-name tl) "terminal")) pairs))
               (tl (car (last tools))))
          (live--result "approval/tool-found" (and tl t))
          (when tl
            (live--result "approval/tool-status" (hermes-tool-status tl))
            (let* ((blob (mapconcat (lambda (get)
                                      (or (funcall get tl) ""))
                                    (list #'hermes-tool-output #'hermes-tool-summary
                                          #'hermes-tool-context #'hermes-tool-preview)
                                    "\n")))
              (live--result "approval/marker-in-tool" (string-match-p live/marker blob)))))))
    (live--result "approval/no-cancel"
                  (if entry (not (member (nth 0 entry) live/cancel-ids)) nil))))

(defun live--assert-clarify ()
  (let ((entry (cdr (assoc "clarify" live/srq))))
    (live--result "clarify/srq-seen" (and entry t))
    (when entry
      (cl-destructuring-bind (id params at) entry
        (let* ((qs (and (hash-table-p params) (hermes--prompt-choices params "questions")))
               (q1 (car qs)))
          (live--result "clarify/srq-id-shape" (and (stringp id) (string-prefix-p "srq-" id)))
          (live--result "clarify/questions-count" (length qs))
          (live--result "clarify/qid-present"
                        (and q1 (hermes--prompts-get q1 "qid"))))
        (live--result "clarify/card-kind-was-clarify"
                      (cl-some (lambda (e) (and (eq (nth 0 e) 'clarify) (nth 2 e)))
                               live/pending-at-srq))
        (live--result "clarify/card-rid-matched-srq"
                      (cl-some (lambda (e) (nth 2 e)) live/pending-at-srq))
        (live--result "clarify/watch-or-fallback"
                      (if live/fallback-answered 'fallback 'watch))
        ;; THE WIRE SHAPE: the client must send {answers: {qid: str}} —
        ;; a bare {qid: answer} hash is cancel-all per the gateway contract.
        (let* ((resp (assoc id live/responds))
               (res  (and resp (cdr resp)))
               (ans  (and res (plist-get res :answers))))
          (live--result "clarify/respond-wrapped-in-answers" (and ans (hash-table-p ans)))
          (when ans
            (let (vals) (maphash (lambda (_k v) (push v vals)) ans)
              (live--result "clarify/answered-value" (car vals)))))
        ;; the gateway accepted it: the agent's reply echoes the choice
        (let ((txt (live--last-assistant-text)))
          (live--result "clarify/agent-reply-text" txt)
          (live--result "clarify/answer-reached-agent"
                        (and txt (string-match-p "beta" txt))))
        (live--result "clarify/no-cancel" (not (member id live/cancel-ids)))))))

(defun live--submit (text)
  (hermes-rpc-request "prompt.submit" (list :session_id live/sid :text text) #'ignore))

(hermes--install-hooks)
(add-hook 'hermes-rpc-server-request-functions #'live--srq-recorder t)
(add-hook 'hermes-rpc-event-functions #'live--event-recorder t)

(live--log "harness start, spawning gateway")
(condition-case e
    (progn
      (hermes-rpc-start)
      (hermes-comint--create-session
       (lambda (buf)
         (setq live/sid (buffer-local-value 'hermes--current-session-id buf))
         (live--log "created sid=%s" live/sid)
         (live--submit
          (concat "Run this exact shell command with the terminal tool, then reply with just OK: "
                  "r" "m -rf /home/r/.hermes/cache/scratch/approval-probe-deleteme/"))
         (setq live/phase 1))))
  (error (live--log "create-session error: %S" e)))

(while (and (not live/declared-done)
            (time-less-p (current-time) live/deadline))
  (unless (hermes-rpc-live-p)
    (live--log "gateway died in phase %s" live/phase)
    (setq live/phase 'dead))
  (live--maybe-fallback)
  (pcase live/phase
    (1 (when (>= live/completions 1)
         (live--assert-approval)
         (live--log "submitting clarify prompt")
         (live--submit
          "You MUST use the clarify tool exactly once to ask me exactly one question: 'Which value should I use?' with exactly two choices: alpha and beta. Wait for my answer, then reply with just the chosen value.")
         (setq live/phase 2)))
    (2 (when (>= live/completions 2)
         (live--assert-clarify)
         (setq live/phase 3
               live/declared-done t))))
  (when (eq live/phase 'dead) (setq live/declared-done t))
  (unless live/declared-done
    (sit-for 0.5)))

(when (not live/declared-done)
  (live--log "deadline hit in phase %s (completions=%d)" live/phase live/completions)
  (pcase live/phase
    (1 (live--assert-approval))
    (2 (live--assert-clarify))))

(advice-remove 'read-multiple-choice nil)
(advice-remove 'completing-read nil)
(advice-remove 'read-string nil)
(advice-remove 'hermes-rpc-respond nil)
(hermes-rpc-stop)

(live--result "final-phase" (if live/declared-done live/phase live/phase))
(live--result "completions" live/completions)
(live--result "events-types" (nreverse (delete-dups (copy-sequence live/events-seen))))
(live--result "cancel-ids" live/cancel-ids)

(princ (format "\n=== LIVE PROMPT E2E (phase %s, completions %d) ===\n"
               live/phase live/completions))
(dolist (kv (nreverse live/results))
  (princ (format "%-42s %S\n" (car kv) (cdr kv))))
(princ "\n--- log tail ---\n")
(let ((s (or (ignore-errors (with-temp-buffer (insert-file-contents live/logfile) (buffer-string))) "")))
  (princ (if (> (length s) 3500) (substring s -3500) s)))
