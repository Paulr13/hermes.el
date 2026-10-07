;;; live-e2e-multisession.el --- Side-by-side multi-session streaming proof -*- lexical-binding: t; -*-
;; Two comint sessions off ONE gateway, long turns submitted near-simultaneously.
;; Asserts: distinct sids, both streams in-flight at the same moment (overlap
;; sampled), per-sid message.complete routing, no marker cross-contamination in
;; buffers. Run from the repo root:  ./test/live/run.sh live-e2e-multisession [timeout_s]

(require 'hermes-rpc)
(require 'hermes)
(require 'hermes-state)
(require 'hermes-comint)

(setq hermes-rpc-python
      (or (getenv "LIVE_E2E_PYTHON") (getenv "HERMES_DEV_PYTHON")
          (expand-file-name "~/.hermes/venv-current/bin/python")))

(defconst live/marker-a "MULTISESS_SHARK_AAA_42")
(defconst live/marker-b "MULTISESS_MARLIN_BBB_99")
(defvar live/deadline (time-add (current-time) (seconds-to-time 420)))
(defvar live/results nil)
(defvar live/phase 0)
(defvar live/sid-a nil)
(defvar live/sid-b nil)
(defvar live/buf-a nil)
(defvar live/buf-b nil)
(defvar live/completions-a 0)
(defvar live/completions-b 0)
(defvar live/both-streaming nil)
(defvar live/samples-both 0)
(defvar live/samples-only-a 0)
(defvar live/samples-only-b 0)
(defvar live/samples-none 0)
(defvar live/events-seen nil)
(defvar live/done-msg "started")

(defconst live/prompt-a
  (concat "Run this exact shell command with the terminal tool, then reply with just OK: "
          "sleep 6 && echo " live/marker-a))
(defconst live/prompt-b
  (concat "Run this exact shell command with the terminal tool, then reply with just OK: "
          "sleep 6 && echo " live/marker-b))

(defconst live/logfile
      (or (getenv "LIVE_E2E_LOG")
          (expand-file-name "live-e2e-multisession.log" temporary-file-directory)))
(ignore-errors (delete-file live/logfile))

(defun live--log (fmt &rest args)
  (let ((line (apply #'format (concat "[" (format-time-string "%H:%M:%S") "] " fmt) args)))
    (append-to-file (concat line "\n") nil live/logfile)))

(defun live--result (k v)
  (setq live/results (cons (cons k v) live/results))
  (live--log "RESULT %s = %S" k v))

(defun live--state (sid) (and sid (gethash sid hermes--sessions)))

(defun live--submit (sid text)
  (hermes-rpc-request "prompt.submit" (list :session_id sid :text text) #'ignore))

(defun live--install ()
  (hermes--install-hooks)
  (add-hook 'hermes-rpc-event-functions
            (lambda (type sid payload)
              (push type live/events-seen)
              (pcase type
                ("message.complete"
                 (cond ((equal sid live/sid-a)
                        (setq live/completions-a (1+ live/completions-a)))
                       ((equal sid live/sid-b)
                        (setq live/completions-b (1+ live/completions-b))))
                 (live--log "[event] message.complete sid=%s" sid))
                ("error"
                 (live--log "[event] ERROR sid=%s %S" sid payload))
                ("gateway.start_timeout"
                 (live--log "[event] start_timeout %S" payload))))))

(defun live--create-b-then-go ()
  (hermes-comint--create-session
   (lambda (buf)
     (setq live/buf-b buf
           live/sid-b (buffer-local-value 'hermes--current-session-id buf))
     (live--log "created sid-b=%s" live/sid-b)
     (if (or (not live/sid-b) (equal live/sid-a live/sid-b))
         (progn (live--result "sids-distinct" nil)
                (setq live/phase 'done live/done-msg "sid collision"))
       (live--submit live/sid-b live/prompt-b)
       (live--submit live/sid-a live/prompt-a)
       (live--log "both prompts submitted")
       (setq live/phase 'streaming)))))

(defun live--tool-segments (sid)
  "All (msg . tool) pairs for terminal tools in sid's committed turns."
  (let (out)
    (when-let ((st (live--state sid)))
      (cl-loop for m across (hermes-state-turns st)
               when (eq (hermes-message-kind m) 'assistant)
               do (cl-loop for s across (hermes-message-segments m)
                           when (eq (hermes-segment-type s) 'tool)
                           do (push (cons m (hermes-segment-content s)) out))))
    (nreverse out)))

(defun live--assert-one (sid marker other tag)
  "Assert sid's committed turns + buffer carry MARKER and never OTHER."
  (let* ((pairs (cl-remove-if-not
                 (lambda (p) (equal (hermes-tool-name (cdr p)) "terminal"))
                 (live--tool-segments sid)))
         (tool (cdr (car (last pairs)))))
    (if (not tool)
        (live--result (concat tag "/tool-found") nil)
      (live--result (concat tag "/tool-status") (hermes-tool-status tool))
      (live--result (concat tag "/cmd-has-marker")
                    (and (string-match-p (regexp-quote marker)
                                         (or (hermes-tool-context tool) ""))
                         (not (string-match-p (regexp-quote other)
                                              (or (hermes-tool-context tool) "")))))
      (let* ((fmt (hermes-tool-format-bash tool))
             (summary (plist-get fmt :summary)))
        (live--result (concat tag "/summary-has-marker")
                      (string-match-p (regexp-quote marker) summary)))))
  (let* ((turns (when-let ((st (live--state sid))) (hermes-state-turns st)))
         (texts (and turns (cl-loop for m across turns
                                    concat (or (hermes--message-text m) "")))))
    (live--result (concat tag "/turns-count") (and turns (length turns)))
    (live--result (concat tag "/text-has-marker") (and texts (string-match-p (regexp-quote marker) texts)))
    (live--result (concat tag "/text-has-other") (and texts (string-match-p (regexp-quote other) texts))))
  ;; Buffer projection: the visible bench content.
  (let ((buf (if (string= tag "a") live/buf-a live/buf-b)))
    (if (not (buffer-live-p buf))
        (live--result (concat tag "/buffer-live") nil)
      (let ((txt (with-current-buffer buf (buffer-string))))
        (live--result (concat tag "/buffer-has-marker") (string-match-p (regexp-quote marker) txt))
        (live--result (concat tag "/buffer-has-other") (string-match-p (regexp-quote other) txt))))))

(live--install)
(hermes-rpc-start)
(live--log "gateway spawning, creating session A then B")
(condition-case e
    (hermes-comint--create-session
     (lambda (buf)
       (setq live/buf-a buf
             live/sid-a (buffer-local-value 'hermes--current-session-id buf))
       (live--log "created sid-a=%s" live/sid-a)
       (if live/sid-a (live--create-b-then-go)
         (setq live/phase 'done live/done-msg "session A failed"))))
  (error (live--log "create-session error: %S" e)))

(while (and (not (eq live/phase 'done))
            (time-less-p (current-time) live/deadline))
  (unless (hermes-rpc-live-p)
    (setq live/done-msg (format "gateway died in phase %s" live/phase)
          live/phase 'done))
  (unless (eq live/phase 'done)
    (when (eq live/phase 'streaming)
      ;; Overlap sampling: both states' live streams non-nil at once.
      (let ((sa (and (live--state live/sid-a) (hermes-state-stream (live--state live/sid-a))))
            (sb (and (live--state live/sid-b) (hermes-state-stream (live--state live/sid-b)))))
        (cond ((and sa sb) (setq live/both-streaming t)
               (setq live/samples-both (1+ live/samples-both)))
              (sa (setq live/samples-only-a (1+ live/samples-only-a)))
              (sb (setq live/samples-only-b (1+ live/samples-only-b)))
              (t (setq live/samples-none (1+ live/samples-none)))))
      (when (and (>= live/completions-a 1) (>= live/completions-b 1))
        (live--assert-one live/sid-a live/marker-a live/marker-b "a")
        (live--assert-one live/sid-b live/marker-b live/marker-a "b")
        (live--result "sessions-in-hash" (hash-table-count hermes--sessions))
        (setq live/phase 'done live/done-msg "completed")))
    (when (not (eq live/phase 'done))
      (accept-process-output nil 1))))

(when (not (eq live/phase 'done))
  (setq live/done-msg (format "deadline hit in phase %s (A done=%d B done=%d)"
                              live/phase live/completions-a live/completions-b)))

(live--result "final-phase" (if (eq live/phase 'done) 'done live/phase))
(live--result "sid-a" live/sid-a)
(live--result "sid-b" live/sid-b)
(live--result "sids-distinct" (and live/sid-a live/sid-b (not (equal live/sid-a live/sid-b))))
(live--result "completions-a" live/completions-a)
(live--result "completions-b" live/completions-b)
(live--result "overlap-observed" live/both-streaming)
(live--result "samples-both" live/samples-both)
(live--result "samples-only-a" live/samples-only-a)
(live--result "samples-only-b" live/samples-only-b)
(live--result "samples-none" live/samples-none)
(live--result "events-types" (nreverse (delete-dups (copy-sequence live/events-seen))))
(hermes-rpc-stop)

(princ (format "\n=== MULTISESSION E2E RESULTS (%s) ===\n" live/done-msg))
(dolist (kv (nreverse live/results))
  (princ (format "%-40s %S\n" (car kv) (cdr kv))))
(princ "\n--- log tail ---\n")
(let ((s (or (ignore-errors (with-temp-buffer (insert-file-contents live/logfile) (buffer-string))) "")))
  (princ (if (> (length s) 3000) (substring s -3000) s)))
