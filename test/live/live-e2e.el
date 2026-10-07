;;; live-e2e.el --- Live-gateway e2e: terminal tool + subagent round-trip -*- lexical-binding: t; -*-
;; Exercises: fa20a72 (full bash command in DONE heading),
;;            e76efe7 (per-subagent detail buffers, spawn-goal preservation),
;; plus logs events for manual review. Run from the repo root:  ./test/live/run.sh live-e2e [timeout_s]

(require 'hermes-rpc)
(require 'hermes)
(require 'hermes-state)
(require 'hermes-comint)
(require 'hermes-subagents)

(setq hermes-rpc-python
      (or (getenv "LIVE_E2E_PYTHON") (getenv "HERMES_DEV_PYTHON")
          (expand-file-name "~/.hermes/venv-current/bin/python")))

(defconst live/marker "E2E_FULL_COMMAND_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA_90CHARS_TAIL_OK")
(defvar live/deadline (time-add (current-time) (seconds-to-time 360)))
(defvar live/results nil)
(defvar live/phase 0)
(defvar live/completions 0)
(defvar live/sid nil)
(defvar live/detail-renders 0)
(defvar live/detail-opened nil)
(defvar live/events-seen nil)
(defvar live/done-msg "started")
(defvar live/subagent-completed nil)

(defconst live/prompt1
  (concat "Run this exact shell command with the terminal tool, then reply with just OK: "
          "echo " live/marker))
(defconst live/prompt2
  "You MUST use the delegate_task tool exactly once to spawn exactly one subagent whose goal is exactly: use the terminal tool to run this command: echo hippopotamus42 — then reply with just the command's output. Wait for its result, then tell me what it printed.")

(defconst live/logfile
      (or (getenv "LIVE_E2E_LOG")
          (expand-file-name "live-e2e.log" temporary-file-directory)))
(ignore-errors (delete-file live/logfile))

(defun live--log (fmt &rest args)
  (let ((line (apply #'format (concat "[" (format-time-string "%H:%M:%S") "] " fmt) args)))
    (append-to-file (concat line "\n") nil live/logfile)))

(defun live--result (k v)
  (setq live/results (cons (cons k v) live/results))
  (live--log "RESULT %s = %S" k v))

(defun live--sess ()
  (and live/sid (gethash live/sid hermes--sessions)))

;; Count live detail-buffer re-renders (proves live updates)
(defun live--count-render (orig id sa)
  (when (get-buffer (hermes-subagents--detail-buffer-name id))
    (setq live/detail-renders (1+ live/detail-renders)))
  (funcall orig id sa))
(advice-add 'hermes-subagents--render-detail :around #'live--count-render)

(defun live--install ()
  (hermes--install-hooks)
  (add-hook 'hermes-rpc-event-functions
            (lambda (type sid payload)
              (push type live/events-seen)
              (pcase type
                ("session.info"
                 (when-let ((m (and (hash-table-p payload) (gethash "model" payload))))
                   (live--result "model" m)))
                ("approval.request"
                 (let ((rid (gethash "request_id" payload)))
                   (live--log "[approval] auto-approve once: %s" (or (gethash "command" payload) "?"))
                   (hermes--request "approval.respond"
                                    (list :session_id sid :request_id rid :choice "once"))))
                ("subagent.spawn_requested"
                 (let ((id (gethash "subagent_id" payload)))
                   (live--log "[subagent] spawn_requested id=%s goal=%S"
                              id (gethash "goal" payload))
                   (condition-case e
                       (hermes-subagents-detail-open id)
                     (error (live--log "[subagent] detail-open error: %S" e)))))
                ("subagent.start"
                 (live--log "[subagent] start id=%s goal=%S"
                            (gethash "subagent_id" payload) (gethash "goal" payload)))
                ((or "subagent.progress" "subagent.tool" "subagent.thinking")
                 (unless live/detail-opened
                   (setq live/detail-opened t)
                   (condition-case e
                       (hermes-subagents-detail-open (gethash "subagent_id" payload))
                     (error (live--log "[subagent] %s detail-open error: %S" type e)))))
                ("subagent.complete"
                 (live--log "[subagent] complete id=%s status=%s summary=%S"
                            (gethash "subagent_id" payload) (gethash "status" payload)
                            (gethash "summary" payload))
                 (setq live/subagent-completed t))
                ("message.complete"
                 (setq live/completions (1+ live/completions)
                       live/phase (pcase live/completions (1 2) (2 3) (_ live/phase)))
                 (live--log "[event] message.complete #%d -> phase %s"
                            live/completions live/phase))
                ("error"
                 (live--log "[event] ERROR %S" payload))
                ("gateway.start_timeout"
                 (live--log "[event] start_timeout %S" payload))))))

(defun live--tool-segments ()
  "All (msg . tool) pairs for terminal tools in committed turns."
  (let (out)
    (when-let ((st (live--sess)))
      (cl-loop for m across (hermes-state-turns st)
               when (eq (hermes-message-kind m) 'assistant)
               do (cl-loop for s across (hermes-message-segments m)
                           when (eq (hermes-segment-type s) 'tool)
                           do (push (cons m (hermes-segment-content s)) out))))
    (nreverse out)))

(defun live--assert-bash ()
  (let* ((pairs (cl-remove-if-not
                 (lambda (p) (equal (hermes-tool-name (cdr p)) "terminal")) (live--tool-segments)))
         (tool  (cdr (car (last pairs)))))
    (if (not tool)
        (live--result "bash/tool-found" nil)
      (live--result "bash/tool-status" (hermes-tool-status tool))
      (live--result "bash/context-len" (length (or (hermes-tool-context tool) "")))
      (let* ((fmt     (hermes-tool-format-bash tool))
             (summary (plist-get fmt :summary)))
        (live--result "bash/summary" summary)
        (live--result "bash/full-command-in-heading"
                      (and (string-match-p "E2E_FULL_COMMAND_" summary)
                           (string-match-p "_OK\\'" (string-trim-right summary))))
        (live--result "bash/marker-verbatim"
                      (and (string-match-p (regexp-quote live/marker) summary)
                           (not (string-match-p "…" summary))))))))

(defun live--one-subagent (sa)
  (live--result "subagent/id" (hermes-subagent-id sa))
  (live--result "subagent/status" (hermes-subagent-status sa))
  (live--result "subagent/goal" (hermes-subagent-goal sa))
  (live--result "subagent/goal-preserved"
                (and (hermes-subagent-goal sa)
                     (string-match-p "hippopotamus" (hermes-subagent-goal sa))))
  (live--result "subagent/summary-set"
                (and (hermes-subagent-summary sa)
                     (> (length (hermes-subagent-summary sa)) 0)))
  (live--result "subagent/tools-seen" (length (hermes-subagent-tools sa)))
  (live--result "subagent/notes-seen" (length (hermes-subagent-notes sa)))
  (let* ((id (hermes-subagent-id sa))
         (db (and id (get-buffer (hermes-subagents--detail-buffer-name id)))))
    (if (not db)
        (live--result "subagent/detail-buffer" nil)
      (let ((txt (with-current-buffer db (buffer-string))))
        (live--result "subagent/detail-has-goal" (string-match-p "hippopotamus" txt))
        (live--result "subagent/detail-has-status"
                      (string-match-p (upcase (format "%s" (hermes-subagent-status sa))) txt))))))

(defun live--assert-subagents ()
  (let* ((st     (live--sess))
         (turns  (and st (hermes-state-turns st)))
         ;; Committed turns carry subagents after message.complete clears
         ;; the live stream (hermes-state.el:1148).
         (sas    (and turns (cl-loop for m across turns
                                     when (hermes-message-subagents m)
                                     append (append (hermes-message-subagents m) nil))))
         (stream (and st (hermes-state-stream st)))
         (ssas   (and stream (hermes-stream-subagents stream))))
    (live--result "subagent/stream-post-commit" (and ssas (length ssas)))
    (if (or (not sas) (= (length sas) 0))
        (live--result "subagent/any" nil)
      (live--result "subagent/count" (length sas))
      (dolist (sa sas)
        (live--one-subagent sa))
      (let ((ov (get-buffer hermes-subagents-buffer-name)))
        (if (not ov)
            (live--result "subagent/overview-buffer" nil)
          (live--result "subagent/overview-mentions-goal"
                        (string-match-p "hippopotamus"
                                        (with-current-buffer ov (buffer-string))))))
      (live--result "subagent/detail-live-renders" live/detail-renders))))

(defun live--submit (text)
  (hermes-rpc-request "prompt.submit"
                      (list :session_id live/sid :text text) #'ignore))

(live--install)
(hermes-rpc-start)
(live--log "gateway spawning, opening comint session")
(condition-case e
    (hermes-comint--create-session
     (lambda (buf)
       (setq live/sid (buffer-local-value 'hermes--current-session-id buf))
       (live--log "created sid=%s" live/sid)
       (live--submit live/prompt1)
       (setq live/phase 1)))
  (error (live--log "create-session error: %S" e)))

(while (and (not (eq live/phase 'done))
            (time-less-p (current-time) live/deadline))
  (unless (hermes-rpc-live-p)
    (setq live/done-msg (format "gateway died in phase %s" live/phase)
          live/phase 'done))
  (unless (eq live/phase 'done)
    (pcase live/phase
      (2 (live--assert-bash)
         (setq live/phase 10)
         (live--log "submitting subagent prompt")
         (live--submit live/prompt2))
      (3 (when live/subagent-completed
           (setq live/phase 4)))
      (4 (live--assert-subagents)
         (setq live/phase 'done
               live/done-msg "completed")))
    (when (not (eq live/phase 'done))
      (accept-process-output nil 1))))

(when (not (eq live/phase 'done))
  (setq live/done-msg (format "deadline hit in phase %s" live/phase)))

(when (and (not (eq live/phase 'done)) (>= live/phase 3))
  (live--assert-subagents)
  (setq live/phase 'done
        live/done-msg (format "phase-%s-deadline-assert" live/phase)))

(live--result "final-phase" (if (eq live/phase 'done) 'done live/phase))
(live--result "events-types" (nreverse (delete-dups (copy-sequence live/events-seen))))
(hermes-rpc-stop)

(princ (format "\n=== LIVE E2E RESULTS (%s) ===\n" live/done-msg))
(dolist (kv (nreverse live/results))
  (princ (format "%-40s %S\n" (car kv) (cdr kv))))
(princ "\n--- log tail ---\n")
(let ((s (or (ignore-errors (with-temp-buffer (insert-file-contents live/logfile) (buffer-string))) "")))
  (princ (if (> (length s) 4000) (substring s -4000) s)))