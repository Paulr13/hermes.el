;;; live-e2e-tool-stream.el --- Prove gateway tool streaming ground truth -*- lexical-binding: t; -*-
;; Ground-truth check for the dead `tool.progress' preview removal: with a real
;; gateway a 12s `sleep' terminal tool must show its COMMAND in the bench
;; mid-run (painted at tool.start), and NOTHING else mid-run — no
;; `tool.progress' frame exists on the wire and no output may appear before
;; `tool.complete'.  At completion the stdout (from the result envelope) must
;; be visible in the bench.  Run: ./test/live/run.sh live-e2e-tool-stream

(require 'hermes-rpc)
(require 'hermes)
(require 'hermes-state)
(require 'hermes-comint)

(setq hermes-rpc-python
      (or (getenv "LIVE_E2E_PYTHON") (getenv "HERMES_DEV_PYTHON")
          (expand-file-name "~/.hermes/venv-current/bin/python")))

(defconst live/marker-cmd "sleep 12")
;; Output token: $((65536+1234)) evaluates to 66770 — a string that never
;; appears inside the command literal, so mid-run absence is unambiguous.
(defconst live/marker-out "66770")
(defconst live/command "sleep 12 && echo $((65536+1234))")
(defconst live/prompt
  (concat "Run this exact shell command with the terminal tool, then reply with just OK: "
          live/command))

(defvar live/deadline (time-add (current-time) (seconds-to-time 300)))
(defvar live/results nil)
(defvar live/phase 0)
(defvar live/sid nil)
(defvar live/buf nil)
(defvar live/completions 0)
(defvar live/events-seen nil)
(defvar live/mid-samples 0)
(defvar live/mid-running 0)
(defvar live/mid-output-nil 0)
(defvar live/mid-buf-out 0)
(defvar live/mid-buf-cmd 0)
(defvar live/done-msg "started")

(defconst live/logfile
  (or (getenv "LIVE_E2E_LOG")
      (expand-file-name "live-e2e-tool-stream.log" temporary-file-directory)))
(ignore-errors (delete-file live/logfile))

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
                ("message.complete"
                 (setq live/completions (1+ live/completions))
                 (live--log "[event] message.complete"))
                ("error"
                 (live--log "[event] ERROR sid=%s %S" sid payload))
                ("gateway.start_timeout"
                 (live--log "[event] start_timeout %S" payload))))))

(defun live--stream-terminal-tool ()
  "The terminal tool segment in the live stream, or nil."
  (when-let ((st (live--state))
             (stream (hermes-state-stream st)))
    (cl-loop for s across (hermes-stream-segments stream)
             when (and (eq (hermes-segment-type s) 'tool)
                       (equal (hermes-tool-name (hermes-segment-content s)) "terminal"))
             return (hermes-segment-content s))))

(defun live--committed-terminal-tool ()
  "The terminal tool segment from committed turns, or nil."
  (when-let ((st (live--state)))
    (cl-loop for m across (hermes-state-turns st)
             when (eq (hermes-message-kind m) 'assistant)
             thereis (cl-loop for s across (hermes-message-segments m)
                              when (and (eq (hermes-segment-type s) 'tool)
                                        (equal (hermes-tool-name (hermes-segment-content s)) "terminal"))
                              return (hermes-segment-content s)))))

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
  (unless (eq live/phase 'done)
    (when (eq live/phase 'streaming)
      (setq live/mid-samples (1+ live/mid-samples))
      (let ((tool (live--stream-terminal-tool)))
        (when (and tool (memq (hermes-tool-status tool) '(generating running)))
          (setq live/mid-running (1+ live/mid-running))
          (when (null (hermes-tool-output tool))
            (setq live/mid-output-nil (1+ live/mid-output-nil)))
          (when (buffer-live-p live/buf)
            (let ((txt (with-current-buffer live/buf (buffer-string))))
              (when (string-match-p (regexp-quote live/marker-out) txt)
                (setq live/mid-buf-out (1+ live/mid-buf-out)))
              (when (string-match-p (regexp-quote live/marker-cmd) txt)
                (setq live/mid-buf-cmd (1+ live/mid-buf-cmd))))))))
    (when (>= live/completions 1)
      (setq live/phase 'done live/done-msg "completed"))
    (when (not (eq live/phase 'done))
      (accept-process-output nil 1))))

(when (not (eq live/phase 'done))
  (setq live/done-msg (format "deadline hit in phase %s (completions=%d)"
                              live/phase live/completions)))

;; Final assertions from committed state + buffer.
(let ((tool (live--committed-terminal-tool)))
  (live--result "tool-found" (and tool t))
  (when tool
    (live--result "final-status" (hermes-tool-status tool))
    (live--result "final-ctx-has-cmd"
                  (and (string-match-p (regexp-quote live/marker-cmd)
                                       (or (hermes-tool-context tool) "")) t))
    (live--result "final-output-has-out"
                  (and (hermes-tool-output tool)
                       (string-match-p (regexp-quote live/marker-out)
                                       (hermes-tool-output tool)) t))))
(when (buffer-live-p live/buf)
  (let ((txt (with-current-buffer live/buf (buffer-string))))
    (live--result "buffer-has-out" (and (string-match-p (regexp-quote live/marker-out) txt) t))
    (live--result "buffer-has-cmd" (and (string-match-p (regexp-quote live/marker-cmd) txt) t))))
(live--result "tool-progress-events"
              (cl-count "tool.progress" live/events-seen :test #'equal))
(live--result "events-types" (nreverse (delete-dups (copy-sequence live/events-seen))))
(live--result "mid-samples" live/mid-samples)
(live--result "mid-running" live/mid-running)
(live--result "mid-output-nil" live/mid-output-nil)
(live--result "mid-buf-out" live/mid-buf-out)
(live--result "mid-buf-cmd" live/mid-buf-cmd)
(live--result "final-phase" (if (eq live/phase 'done) 'done live/phase))
(live--result "sid" live/sid)
(hermes-rpc-stop)

;; Verdict: state-level mid-run assertions only — batch has no frames, so
;; mid-run buffer paints are visibility-gated (informational, not verdict).
(let* ((get (lambda (k) (cdr (assoc-string k live/results))))
       (checks (list
                (list "tool-found" (funcall get "tool-found"))
                (list "final-ctx-has-cmd" (funcall get "final-ctx-has-cmd"))
                (list "final-output-has-out" (funcall get "final-output-has-out"))
                (list "buffer-has-out" (funcall get "buffer-has-out"))
                (list "no-tool-progress-frames" (eq 0 (funcall get "tool-progress-events")))
                (list "sampled-mid-run" (> (or (funcall get "mid-running") 0) 0))
                (list "no-output-mid-run"
                      (= (or (funcall get "mid-output-nil") 0)
                         (or (funcall get "mid-running") -1)))))
       (failed (cl-loop for (k v) in checks unless v collect k)))
  (princ (format "\n=== TOOL-STREAM E2E RESULTS (%s) ===\n" live/done-msg))
  (dolist (kv (nreverse live/results))
    (princ (format "%-40s %S\n" (car kv) (cdr kv))))
  (princ (format "\n=== TOOL-STREAM E2E: %s ===\n"
                 (if failed
                     (concat "FAIL " (mapconcat #'identity failed " "))
                   "PASS"))))

(princ "\n--- log tail ---\n")
(let ((s (or (ignore-errors (with-temp-buffer (insert-file-contents live/logfile) (buffer-string))) "")))
  (princ (if (> (length s) 3000) (substring s -3000) s)))
