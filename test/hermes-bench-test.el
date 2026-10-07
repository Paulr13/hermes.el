;;; hermes-bench-test.el --- ERT tests for comint-backed bench -*- lexical-binding: t; -*-

(require 'ert)
(require 'hermes)
(require 'hermes-comint)
(require 'hermes-input)
(require 'hermes-state)
(load (expand-file-name "hermes-test-helpers.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name))))

(defvar hermes-bench-test--counter 0)

(defun hermes-bench-test--make-parent ()
  "Return (PARENT . SID) for a fresh org viewer + registered session."
  (let* ((sid (format "bench-test-%d" (cl-incf hermes-bench-test--counter)))
         (buf (generate-new-buffer (format " *hermes-bench-test-%s*" sid))))
    (with-current-buffer buf
      (org-mode)
      (hermes--ensure-container)
      (hermes-org-minor-mode 1)
      (hermes--register-session
       sid
       (make-hermes-state :session-id sid :connection 'connected)
       (copy-marker (point-min) nil)))
    (cons buf sid)))

(defmacro hermes-bench-test--with-pair (parent-var bench-var &rest body)
  "Bind PARENT-VAR and BENCH-VAR to a fresh hermes/bench pair around BODY."
  (declare (indent 2))
  `(let* ((pair (hermes-bench-test--make-parent))
          (,parent-var (car pair))
          (sid (cdr pair))
          (,bench-var (hermes-bench-ensure sid)))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p ,bench-var) (kill-buffer ,bench-var))
       (when (buffer-live-p ,parent-var) (kill-buffer ,parent-var)))))

(ert-deftest hermes-bench-test/ensure-uses-comint-mode ()
  "`hermes-bench-ensure' creates a `hermes-comint-mode' buffer."
  (hermes-bench-test--with-pair _parent bench
    (with-current-buffer bench
      (should (derived-mode-p 'hermes-comint-mode))
      (should hermes-comint--bench-p))))

(ert-deftest hermes-bench-test/ensure-buffer-name ()
  "Bench buffer is named `*hermes-bench:<sid>*'."
  (hermes-bench-test--with-pair _parent bench
    (should (equal (buffer-name bench)
                   (format "*hermes-bench:%s*" sid)))))

(ert-deftest hermes-bench-test/ensure-registers-in-registry ()
  "Bench is registered in `hermes--bench-buffers' under its sid."
  (hermes-bench-test--with-pair _parent bench
    (should (eq bench (gethash sid hermes--bench-buffers)))))

(ert-deftest hermes-bench-test/ensure-returns-existing-on-repeat ()
  "Repeat `hermes-bench-ensure' returns the same buffer."
  (hermes-bench-test--with-pair _parent bench
    (should (eq bench (hermes-bench-ensure sid)))))

(ert-deftest hermes-bench-test/active-p-from-org-viewer ()
  (hermes-bench-test--with-pair parent bench
    (should (eq bench (hermes-bench-active-p parent)))
    (with-current-buffer parent
      (should (eq bench (hermes-bench-active-p))))))

(ert-deftest hermes-bench-test/active-p-from-bench-buffer ()
  (hermes-bench-test--with-pair _parent bench
    (should (eq bench (hermes-bench-active-p bench)))
    (with-current-buffer bench
      (should (eq bench (hermes-bench-active-p))))))

(ert-deftest hermes-bench-test/active-p-by-sid ()
  (hermes-bench-test--with-pair _parent bench
    (should (eq bench (hermes-bench-active-p sid)))))

(ert-deftest hermes-bench-test/active-p-nil-for-unrelated ()
  (let ((unrelated (generate-new-buffer " *hermes-bench-test-unrelated*")))
    (unwind-protect
        (should-not (hermes-bench-active-p unrelated))
      (kill-buffer unrelated))))

(ert-deftest hermes-bench-test/kill-on-last-viewer-detach ()
  "Killing the only viewer kills the paired bench."
  (let* ((pair (hermes-bench-test--make-parent))
         (parent (car pair))
         (sid (cdr pair))
         (bench (hermes-bench-ensure sid)))
    (should (buffer-live-p bench))
    (kill-buffer parent)
    (should-not (buffer-live-p bench))
    (should-not (gethash sid hermes--bench-buffers))))

(ert-deftest hermes-bench-test/hide-deletes-and-removes ()
  "`hermes-bench-hide' kills the buffer and clears the registry entry."
  (let* ((pair (hermes-bench-test--make-parent))
         (parent (car pair))
         (sid (cdr pair))
         (bench (hermes-bench-ensure sid)))
    (unwind-protect
        (progn
          (hermes-bench-hide sid)
          (should-not (buffer-live-p bench))
          (should-not (gethash sid hermes--bench-buffers)))
      (when (buffer-live-p parent) (kill-buffer parent)))))

;;;; Slash-command CAPF in the bench

(defun hermes-bench-test--seed-catalog (parent)
  "Install a fake slash catalog with `/clear' on PARENT's session."
  (with-current-buffer parent
    (let ((h (make-hash-table :test 'equal)))
      (puthash "pairs"
               (vector (vector "/clear" "Clear conversation history"))
               h)
      (setf (hermes-state-slash-catalog (hermes-test--cur)) h))))

(defun hermes-bench-test--type-input (bench text)
  "Insert TEXT into BENCH's input area, point at end."
  (with-current-buffer bench
    (let* ((p (marker-position hermes-comint--prompt-start))
           (input-start (+ p (length hermes-comint--prompt-string)))
           (inhibit-read-only t))
      (goto-char input-start)
      (delete-region input-start (point-max))
      (insert text))))

(ert-deftest hermes-bench-test/capf-hook-installed ()
  "Bench mode installs the completion dispatch on the bench buffer."
  (hermes-bench-test--with-pair _parent bench
    (with-current-buffer bench
      (should (memq #'hermes-comint--complete-at-point
                    completion-at-point-functions)))))

(ert-deftest hermes-bench-test/capf-finds-slash-after-prompt ()
  "CAPF triggers when `/' immediately follows the bench prompt."
  (hermes-bench-test--with-pair parent bench
    (hermes-bench-test--seed-catalog parent)
    (hermes-bench-test--type-input bench "/cle")
    (with-current-buffer bench
      (let* ((result (hermes-comint-bench--slash-complete))
             (p (marker-position hermes-comint--prompt-start))
             (input-start (+ p (length hermes-comint--prompt-string))))
        (should result)
        (should (= input-start (nth 0 result)))
        (should (= (point) (nth 1 result)))
        (should (member "/clear" (nth 2 result)))))))

(ert-deftest hermes-bench-test/capf-nil-before-slash ()
  "CAPF returns nil when input does not start with `/'."
  (hermes-bench-test--with-pair parent bench
    (hermes-bench-test--seed-catalog parent)
    (hermes-bench-test--type-input bench "hello")
    (with-current-buffer bench
      (should-not (hermes-comint-bench--slash-complete)))))

(ert-deftest hermes-bench-test/capf-in-conversation-buffer ()
  "Slash CAPF works in a plain conversation buffer (no bench flag).
Regression 2026-10-07: the CAPF was bench-only, so typing \"/\" in a
`*hermes:<sid>*' conversation buffer fell through to comint's file
completion and the popup offered directories instead of commands."
  (let* ((pair (hermes-bench-test--make-parent))
         (parent (car pair))
         (sid (cdr pair)))
    (unwind-protect
        (with-temp-buffer
          (hermes-comint-mode)
          (setq-local hermes--current-session-id sid)
          (should-not hermes-comint--bench-p)
          ;; The mode installs the TUI-style dispatcher as the only CAPF.
          (should (equal (list #'hermes-comint--complete-at-point)
                         completion-at-point-functions))
          (hermes-bench-test--seed-catalog parent)
          (hermes-bench-test--type-input (current-buffer) "/cle")
          (let* ((result (hermes-comint-bench--slash-complete))
                 (input-start (hermes-comint--input-start-pos)))
            (should result)
            (should (= input-start (nth 0 result)))
            (should (= (point) (nth 1 result)))
            (should (member "/clear" (nth 2 result)))))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(ert-deftest hermes-bench-test/dispatch-file-fallback-after-text ()
  "Text before a \"/\" dispatches to file completion, not the menu.
TUI parity: `check /ho' is a path — the slash is not the first input
character, so the dispatcher falls through to comint file completion."
  (let* ((pair (hermes-bench-test--make-parent))
         (parent (car pair))
         (sid (cdr pair)))
    (unwind-protect
        (with-temp-buffer
          (hermes-comint-mode)
          (setq-local hermes--current-session-id sid)
          (hermes-bench-test--seed-catalog parent)
          (hermes-bench-test--type-input (current-buffer) "check /ho")
          (let* ((result (hermes-comint--complete-at-point))
                 (menu   (hermes-comint-bench--slash-complete)))
            (should result)              ; file completion answered
            ;; File completion's candidates are a function table, not a
            ;; list — distinguish by asking the menu CAPF in the same spot.
            (should-not menu)            ; no command menu here
            (should-not (equal result menu))))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(ert-deftest hermes-bench-test/dispatch-file-fallback-path-after-command ()
  "A path argument after a slash command gets file completion.
`/dir ~/pro': the menu owns the leading \"/dir\", but point sits in
\"~/pro\" — path-ish input falls back to file completion."
  (let* ((pair (hermes-bench-test--make-parent))
         (parent (car pair))
         (sid (cdr pair)))
    (unwind-protect
        (with-temp-buffer
          (hermes-comint-mode)
          (setq-local hermes--current-session-id sid)
          (hermes-bench-test--type-input (current-buffer) "/dir ~/pro")
          (let* ((result (hermes-comint--complete-at-point))
                 (menu   (hermes-comint-bench--slash-complete)))
            (should result)
            (should-not menu)
            (should-not (equal result menu))))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(ert-deftest hermes-bench-test/dispatch-nil-on-prose ()
  "Plain prose dispatches to nothing — no menu, no file completion."
  (let* ((pair (hermes-bench-test--make-parent))
         (parent (car pair))
         (sid (cdr pair)))
    (unwind-protect
        (with-temp-buffer
          (hermes-comint-mode)
          (setq-local hermes--current-session-id sid)
          (hermes-bench-test--type-input (current-buffer) "hello")
          (should-not (hermes-comint--complete-at-point)))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(ert-deftest hermes-bench-test/capf-nil-before-slash-conversation ()
  "No slash menu when conversation input does not start with `/'."
  (let* ((pair (hermes-bench-test--make-parent))
         (parent (car pair))
         (sid (cdr pair)))
    (unwind-protect
        (with-temp-buffer
          (hermes-comint-mode)
          (setq-local hermes--current-session-id sid)
          (hermes-bench-test--type-input (current-buffer) "hello")
          (should-not (hermes-comint-bench--slash-complete)))
      (when (buffer-live-p parent) (kill-buffer parent)))))

;;;; Bench mode reducer no-ops

(ert-deftest hermes-bench-test/load-from-state-no-op ()
  "In bench mode, `hermes-comint--load-from-state' does not insert turns."
  (let* ((sid (format "bench-load-%d" (cl-incf hermes-bench-test--counter)))
         (msg (make-hermes-message
               :kind 'user
               :segments (vector (make-hermes-segment
                                  :type 'text :content "Should not appear"))
               :timestamp (current-time)))
         (state (make-hermes-state :session-id sid :turns (vector msg))))
    (puthash sid state hermes--sessions)
    (unwind-protect
        (let ((bench (hermes-bench-ensure sid)))
          (with-current-buffer bench
            (hermes-comint--load-from-state state)
            (should (= (marker-position hermes-comint--output-end)
                       1))
            (should-not (string-match-p
                         "Should not appear"
                         (buffer-substring-no-properties
                          (point-min) (point-max))))))
      (let ((b (gethash sid hermes--bench-buffers)))
        (when (buffer-live-p b) (kill-buffer b)))
      (remhash sid hermes--sessions))))

(ert-deftest hermes-bench-test/stream-commit-clears-ephemeral ()
  "Bench-mode `--stream-commit' wipes the ephemeral region and does not advance output-end."
  (let* ((sid (format "bench-commit-%d" (cl-incf hermes-bench-test--counter)))
         (state (make-hermes-state :session-id sid)))
    (puthash sid state hermes--sessions)
    (unwind-protect
        (let ((bench (hermes-bench-ensure sid)))
          (with-current-buffer bench
            (setq hermes-comint--current-user-prompt "hi")
            (setq hermes-comint--steer-messages '("steered"))
            (setq hermes-comint--stream-active t)
            ;; Insert dummy ephemeral content.
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char (marker-position hermes-comint--output-end))
                (insert "stream text\n")))
            (hermes-comint--stream-commit state)
            (should (null hermes-comint--steer-messages))
            (should (null hermes-comint--status-message))
            (should (= (marker-position hermes-comint--output-end) 1))
            (should-not (string-match-p
                         "stream text"
                         (buffer-substring-no-properties
                          (point-min) (point-max))))))
      (let ((b (gethash sid hermes--bench-buffers)))
        (when (buffer-live-p b) (kill-buffer b)))
      (remhash sid hermes--sessions))))

(provide 'hermes-bench-test)
;;; hermes-bench-test.el ends here
