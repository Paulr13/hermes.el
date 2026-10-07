;;; hermes-comint-test.el --- ERT tests for the comint viewer -*- lexical-binding: t; -*-

(require 'ert)
(require 'hermes-state)
(require 'hermes-comint)
(require 'hermes)
(load (expand-file-name "hermes-test-helpers.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name))))

(defvar hermes-comint-test--counter 0)

(defun hermes-comint-test--fresh-sid ()
  (format "comint-test-%d" (cl-incf hermes-comint-test--counter)))

(defun hermes-comint-test--make-buffer (sid &optional state)
  "Create a hermes-comint-mode buffer for SID, registering STATE if given.
Returns the buffer."
  (when state (puthash sid state hermes--sessions))
  (let* ((name (format "*hermes-comint-test:%s*" sid))
         (buf  (get-buffer-create name)))
    (with-current-buffer buf
      (hermes-comint-mode)
      (setq-local hermes--current-session-id sid)
      (puthash sid buf hermes-comint--buffers)
      (when state (hermes-comint--load-from-state state)))
    buf))

(defmacro hermes-comint-test--with-buffer (buf-var sid-var state &rest body)
  "Bind BUF-VAR and SID-VAR around a fresh comint buffer for STATE."
  (declare (indent 3))
  `(let* ((,sid-var (hermes-comint-test--fresh-sid))
          (,buf-var (hermes-comint-test--make-buffer ,sid-var ,state)))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p ,buf-var) (kill-buffer ,buf-var))
       (remhash ,sid-var hermes--sessions)
       (remhash ,sid-var hermes-comint--buffers))))

(defun hermes-comint-test--committed-text ()
  "Return committed-output substring [point-min, output-end)."
  (buffer-substring-no-properties
   (point-min) (marker-position hermes-comint--output-end)))

;;;; Buffer setup

(ert-deftest hermes-comint-test/setup-establishes-markers ()
  "After mode activation, output-end and prompt-start are well-formed."
  (hermes-comint-test--with-buffer buf sid (make-hermes-state :session-id sid)
    (with-current-buffer buf
      (should (markerp hermes-comint--output-end))
      (should (markerp hermes-comint--prompt-start))
      (should (= 1 (marker-position hermes-comint--output-end)))
      ;; Pending region is empty initially: output-end == prompt-start.
      (should (= (marker-position hermes-comint--output-end)
                 (marker-position hermes-comint--prompt-start)))
      ;; Prompt is at point-max area.
      (should (string-prefix-p hermes-comint--prompt-string
                               (buffer-substring-no-properties
                                (marker-position hermes-comint--prompt-start)
                                (point-max)))))))

;;;; Turn insertion — kinds and segments

(ert-deftest hermes-comint-test/insert-user-turn ()
  "User turn renders heading + body in the committed region."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (msg (make-hermes-message
               :kind 'user
               :segments (vector (make-hermes-segment
                                  :type 'text :content "Hello world"))
               :timestamp (current-time)))
         (state (make-hermes-state
                 :session-id sid
                 :turns (vector msg))))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        (let ((committed (hermes-comint-test--committed-text)))
          (should (string-match-p "User" committed))
          (should (string-match-p "Hello world" committed)))))))

(ert-deftest hermes-comint-test/insert-assistant-turn-with-all-segments ()
  "Assistant turn renders reasoning, text, and tool blocks in natural order."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (tool (make-hermes-tool :id "t1" :name "write_file"
                                 :status 'complete :summary "wrote foo"))
         (msg (make-hermes-message
               :kind 'assistant
               :segments (vector
                          (make-hermes-segment
                           :type 'reasoning :content "thinking step" :id "r1")
                          (make-hermes-segment
                           :type 'text :content "Here is the answer." :id "t1")
                          (make-hermes-segment
                           :type 'tool :content tool :id "tool1"))
               :timestamp (current-time)))
         (state (make-hermes-state :session-id sid :turns (vector msg))))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        (let ((c (hermes-comint-test--committed-text)))
          (should (string-match-p "Assistant" c))
          (should (string-match-p "thinking step" c))
          (should (string-match-p "Here is the answer" c))
          (should (string-match-p "write_file" c))
          (should (string-match-p "wrote foo" c))
          (should (< (string-match-p "thinking step" c)
                     (string-match-p "Here is the answer" c)))
          (should (< (string-match-p "Here is the answer" c)
                     (string-match-p "write_file" c))))))))

(ert-deftest hermes-comint-test/insert-system-turn ()
  (let* ((sid (hermes-comint-test--fresh-sid))
         (msg (make-hermes-message
               :kind 'system
               :segments (vector (make-hermes-segment
                                  :type 'text :content "system note"))
               :timestamp (current-time)))
         (state (make-hermes-state :session-id sid :turns (vector msg))))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        (let ((c (hermes-comint-test--committed-text)))
          (should (string-match-p "System" c))
          (should (string-match-p "system note" c)))))))

;;;; Append-only refresh

(ert-deftest hermes-comint-test/append-only-on-new-turn ()
  "A second refresh appends just the new turn, not a full rebuild."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (m1 (make-hermes-message
              :kind 'user
              :segments (vector (make-hermes-segment :type 'text :content "one"))
              :timestamp (current-time)))
         (s1 (make-hermes-state :session-id sid :turns (vector m1))))
    (hermes-comint-test--with-buffer buf sid s1
      (with-current-buffer buf
        (let* ((after-first (hermes-comint-test--committed-text))
               (m2 (make-hermes-message
                    :kind 'user
                    :segments (vector (make-hermes-segment
                                       :type 'text :content "two"))
                    :timestamp (current-time)))
               (s2 (make-hermes-state :session-id sid
                                      :turns (vector m1 m2))))
          (hermes-comint--append-new-turns s2)
          (let ((after-second (hermes-comint-test--committed-text)))
            (should (string-prefix-p after-first after-second))
            (should (string-match-p "two" after-second))))))))

;;;; Streaming lifecycle

(ert-deftest hermes-comint-test/stream-begin-paints-pending-region ()
  "Stream begin inserts in-flight content into [output-end, prompt-start)."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (initial (make-hermes-state :session-id sid))
         (stream (make-hermes-stream
                  :segments (vector (make-hermes-segment
                                     :type 'text :content "streaming…")))))
    (hermes-comint-test--with-buffer buf sid initial
      (with-current-buffer buf
        (let ((new (make-hermes-state :session-id sid :stream stream)))
          (hermes-comint--stream-begin new)
          (should hermes-comint--stream-active)
          (let ((pending (buffer-substring-no-properties
                          (marker-position hermes-comint--output-end)
                          (marker-position hermes-comint--prompt-start))))
            (should (string-match-p "streaming" pending))))))))

(ert-deftest hermes-comint-test/stream-commit-seals-into-committed ()
  "Stream commit promotes the in-flight turn into the committed region."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (initial (make-hermes-state :session-id sid))
         (stream (make-hermes-stream
                  :segments (vector (make-hermes-segment
                                     :type 'text :content "in flight")))))
    (hermes-comint-test--with-buffer buf sid initial
      (with-current-buffer buf
        (hermes-comint--stream-begin
         (make-hermes-state :session-id sid :stream stream))
        ;; Reducer would push the final message into turns before clearing
        ;; the stream — simulate that.
        (let* ((final (make-hermes-message
                       :kind 'assistant
                       :segments (vector (make-hermes-segment
                                          :type 'text :content "final answer"))
                       :timestamp (current-time)))
               (committed (make-hermes-state :session-id sid
                                             :turns (vector final))))
          (hermes-comint--stream-commit committed)
          (should-not hermes-comint--stream-active)
          ;; Pending region is empty: output-end caught up to prompt-start.
          (should (= (marker-position hermes-comint--output-end)
                     (marker-position hermes-comint--prompt-start)))
          (let ((c (hermes-comint-test--committed-text)))
            (should (string-match-p "final answer" c))))))))

(ert-deftest hermes-comint-test/full-send-cycle-no-duplicate-assistant ()
  "Full hook-driven dispatch — user submit → stream begin → delta → complete.
Reproduces the dup-rendering bug seen when sending from the comint buffer."
  (let* ((sid     (hermes-comint-test--fresh-sid))
         (initial (make-hermes-state :session-id sid))
         (user    (make-hermes-message
                   :kind 'user
                   :segments (vector (make-hermes-segment
                                      :type 'text :content "hi"))
                   :timestamp (current-time)))
         (stream0 (make-hermes-stream :segments []))
         (stream1 (make-hermes-stream
                   :segments (vector (make-hermes-segment
                                      :type 'text :content "Hey."))))
         (final   (make-hermes-message
                   :kind 'assistant
                   :segments (vector (make-hermes-segment
                                      :type 'text :content "Hey."))
                   :timestamp (current-time)))
         (s1 (make-hermes-state :session-id sid :turns (vector user)))
         (s2 (make-hermes-state :session-id sid :turns (vector user)
                                :stream stream0))
         (s3 (make-hermes-state :session-id sid :turns (vector user)
                                :stream stream1))
         (s4 (make-hermes-state :session-id sid
                                :turns (vector user final))))
    (hermes-comint-test--with-buffer buf sid initial
      (with-current-buffer buf
        (let ((hermes--current-session-id sid))
          ;; user-submit
          (puthash sid s1 hermes--sessions)
          (hermes-comint--refresh initial s1)
          ;; message.start
          (puthash sid s2 hermes--sessions)
          (hermes-comint--refresh s1 s2)
          ;; message.delta
          (puthash sid s3 hermes--sessions)
          (hermes-comint--refresh s2 s3)
          ;; message.complete
          (puthash sid s4 hermes--sessions)
          (hermes-comint--refresh s3 s4))
        (let* ((c (buffer-substring-no-properties (point-min) (point-max)))
               (count (cl-count-if
                       (lambda (s) (string-match-p "Assistant" s))
                       (split-string c "\n"))))
          (should (= 1 count)))))))

(ert-deftest hermes-comint-test/reentrant-pending-clear-no-duplicate ()
  "Inner re-entrant firing before outer `message.complete' does not dup the assistant.
Reproduces the observed bug: another subscriber dispatches an
event (e.g. `:pending-turns-clear') from inside the hook chain for
`message.complete'.  The hook then fires recursively in B→C order
*before* the outer A→B firing reaches the comint subscriber.  With the
live-state projection, both invocations converge to the same buffer."
  (let* ((sid     (hermes-comint-test--fresh-sid))
         (initial (make-hermes-state :session-id sid))
         (user    (make-hermes-message
                   :kind 'user
                   :segments (vector (make-hermes-segment
                                      :type 'text :content "hi"))
                   :timestamp (current-time)))
         (stream0 (make-hermes-stream :segments []))
         (final   (make-hermes-message
                   :kind 'assistant
                   :segments (vector (make-hermes-segment
                                      :type 'reasoning :content "match the energy")
                                     (make-hermes-segment
                                      :type 'text :content "Hey."))
                   :timestamp (current-time)))
         ;; A: turns=[user], stream=inflight.   (before message.complete)
         ;; B: turns=[user, assistant], stream=nil.   (after message.complete)
         ;; C: turns=[user, assistant], stream=nil.   (after :pending-turns-clear)
         (sA (make-hermes-state :session-id sid :turns (vector user)
                                :stream stream0))
         (sB (make-hermes-state :session-id sid :turns (vector user final)))
         (sC (make-hermes-state :session-id sid :turns (vector user final))))
    (hermes-comint-test--with-buffer buf sid initial
      (with-current-buffer buf
        (let ((hermes--current-session-id sid))
          ;; Get the buffer into the streaming lifecycle (mirrors what
          ;; message.start did).
          (puthash sid sA hermes--sessions)
          (hermes-comint--refresh initial sA)
          (should hermes-comint--stream-active)
          ;; Outer dispatch reduces A→B and writes B to the slot.
          (puthash sid sB hermes--sessions)
          ;; Now another subscriber runs first, dispatches its inner
          ;; event (B→C), the inner hook fires synchronously, and the
          ;; comint subscriber sees the inner firing BEFORE the outer
          ;; one reaches it.
          (puthash sid sC hermes--sessions)
          (hermes-comint--refresh sB sC)    ; inner firing (B → C)
          (hermes-comint--refresh sA sB))   ; outer firing resumes (A → B)
        ;; Exactly one assistant heading in the buffer.
        (let* ((c (buffer-substring-no-properties (point-min) (point-max)))
               (n (cl-count-if (lambda (s) (string-match-p "Assistant" s))
                               (split-string c "\n"))))
          (should (= 1 n)))
        ;; Pending region empty: commit ran exactly once and snapshot is current.
        (should-not hermes-comint--stream-active)
        (should (= (marker-position hermes-comint--output-end)
                   (marker-position hermes-comint--prompt-start)))))))

;;;; Header line

(ert-deftest hermes-comint-test/header-line-bg-running ()
  (let* ((bt (make-hermes-bg-task :task-id "1" :prompt "p"
                                  :status 'running :created-at "now"))
         (state (make-hermes-state :bg-tasks (vector bt))))
    (should (string-match-p "bg: 1 running"
                            (hermes-comint--format-header-line state)))))

(ert-deftest hermes-comint-test/header-line-bg-complete ()
  (let* ((bt (make-hermes-bg-task :task-id "7" :prompt "p"
                                  :status 'complete :created-at "now"))
         (state (make-hermes-state :bg-tasks (vector bt))))
    (should (string-match-p "bg #7 complete"
                            (hermes-comint--format-header-line state)))))

(ert-deftest hermes-comint-test/header-line-nil-on-empty-state ()
  (should-not (hermes-comint--format-header-line
               (make-hermes-state))))

;;;; Prompt area — text in/out, read-only invariants

(ert-deftest hermes-comint-test/prompt-text-roundtrip ()
  "Typing after prompt prefix is readable + clearable."
  (hermes-comint-test--with-buffer buf sid (make-hermes-state :session-id sid)
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "user typed this")
      (should (equal "user typed this" (hermes-comint--prompt-text)))
      (hermes-comint--clear-prompt)
      (should (equal "" (hermes-comint--prompt-text))))))

(ert-deftest hermes-comint-test/committed-region-is-read-only ()
  "Inserted committed turns carry read-only property."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (msg (make-hermes-message
               :kind 'user
               :segments (vector (make-hermes-segment :type 'text :content "x"))
               :timestamp (current-time)))
         (state (make-hermes-state :session-id sid :turns (vector msg))))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        (let ((mid (/ (+ (point-min)
                         (marker-position hermes-comint--output-end))
                      2)))
          (should (get-text-property mid 'read-only)))))))

;;;; Open + registry round-trip

(ert-deftest hermes-comint-test/open-registers-and-loads-state ()
  "hermes-comint--open creates a buffer, registers it, loads turns."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (msg (make-hermes-message
               :kind 'user
               :segments (vector (make-hermes-segment
                                  :type 'text :content "hi from open"))
               :timestamp (current-time)))
         (state (make-hermes-state :session-id sid :turns (vector msg))))
    (puthash sid state hermes--sessions)
    (unwind-protect
        (let ((buf (save-window-excursion (hermes-comint--open sid))))
          (should (buffer-live-p buf))
          (should (eq buf (gethash sid hermes-comint--buffers)))
          (with-current-buffer buf
            (should (derived-mode-p 'hermes-comint-mode))
            (should (equal sid hermes--current-session-id))
            (should (string-match-p "hi from open"
                                    (hermes-comint-test--committed-text))))
          (kill-buffer buf))
      (remhash sid hermes--sessions)
      (remhash sid hermes-comint--buffers))))

(ert-deftest hermes-comint-test/detach-removes-from-registry ()
  "Killing the buffer removes the registry entry."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (state (make-hermes-state :session-id sid)))
    (puthash sid state hermes--sessions)
    (unwind-protect
        (let ((buf (save-window-excursion (hermes-comint--open sid))))
          (should (gethash sid hermes-comint--buffers))
          (kill-buffer buf)
          (should-not (gethash sid hermes-comint--buffers)))
      (remhash sid hermes--sessions)
      (remhash sid hermes-comint--buffers))))

;;;; Mode-line formatter

(ert-deftest hermes-comint-test/mode-line-nil-on-empty ()
  "Empty state returns the empty string."
  (should (equal "" (hermes-comint--format-mode-line nil nil))))

(ert-deftest hermes-comint-test/mode-line-basic ()
  "Connection dot and session info appear in the formatted string."
  (let* ((sid "abc12345xyz")
         (state (make-hermes-state :session-id sid :connection 'connected)))
    (let ((s (hermes-comint--format-mode-line state sid)))
      (should (string-match-p "●" s))
      (should (string-match-p "session abc12345 ready" s)))))

(ert-deftest hermes-comint-test/mode-line-model ()
  "Model name (from session-info) appears in the formatted string."
  (let* ((sid "s1")
         (info (let ((h (make-hash-table :test 'equal)))
                 (puthash "model" "claude-opus-4-7" h) h))
         (state (make-hermes-state :session-id sid
                                   :connection 'connected
                                   :session-info info)))
    (should (string-match-p "claude-opus-4-7"
                            (hermes-comint--format-mode-line state sid)))))

(ert-deftest hermes-comint-test/mode-line-streaming-status ()
  "Streaming text from session-scoped UI state appears in the mode-line."
  (hermes-test--reset-global-state)
  (let* ((sid "s2")
         (state (make-hermes-state :session-id sid :connection 'connected))
         (ui    (make-hermes-ui-state :status-text "Thinking…")))
    (puthash sid ui hermes--ui-states)
    (should (string-match-p "Thinking…"
                            (hermes-comint--format-mode-line state sid)))))

(ert-deftest hermes-comint-test/mode-line-usage ()
  "Context readout (TUI-style) appears when the usage hash has it;
plain token counters alone show nothing."
  (let* ((sid "s3")
         (usage (let ((h (make-hash-table :test 'equal)))
                  (puthash "context_used" 62200 h)
                  (puthash "context_max" 512000 h)
                  (puthash "context_percent" 12.16 h)
                  (puthash "context_estimated" t h)
                  (puthash "cache_hit_pct" 85 h) h))
         (state (make-hermes-state :session-id sid
                                   :connection 'connected
                                   :usage usage)))
    (should (string-match-p (regexp-quote "~62.2K/512K │ [█░░░░░░░░░] ~12% │ ◎ 85%")
                            (hermes-comint--format-mode-line state sid)))
    ;; Tokens-only snapshot (no context accounting) renders no readout.
    (let* ((tokens (let ((h (make-hash-table :test 'equal)))
                     (puthash "tokens_sent" 100 h)
                     (puthash "tokens_received" 250 h) h))
           (st2 (make-hermes-state :session-id sid
                                   :connection 'connected
                                   :usage tokens)))
      (should-not (string-match-p "tokens"
                                  (hermes-comint--format-mode-line st2 sid))))))

(ert-deftest hermes-comint-test/format-token-count ()
  "Token counts format TUI-style with K suffixes."
  (should (equal "622" (hermes-comint--format-token-count 622)))
  (should (equal "62.2K" (hermes-comint--format-token-count 62200)))
  (should (equal "512K" (hermes-comint--format-token-count 512000)))
  (should (equal "1.5K" (hermes-comint--format-token-count 1533))))

(ert-deftest hermes-comint-test/mode-line-queue ()
  "Queue length appears in the formatted string."
  (let* ((sid "s4")
         (state (make-hermes-state :session-id sid
                                   :connection 'connected
                                   :queue '("a" "b" "c"))))
    (should (string-match-p "queue: 3"
                             (hermes-comint--format-mode-line state sid)))))

;;;; Image insertion

(ert-deftest hermes-comint-test/image-fallback-terminal ()
  "On terminal, image segment produces [image: name] placeholder before text."
  (let* ((msg (make-hermes-message
               :kind 'user
               :segments (vector
                          (make-hermes-segment
                           :type 'image
                           :content (list :path "/tmp/test.png" :name "test.png"))
                          (make-hermes-segment
                           :type 'text :content "Hello"))
               :timestamp (current-time))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) nil)))
        (hermes-comint--insert-user-body msg))
      (let ((text (buffer-string)))
        (should (string-match-p "\\[image:" text))
        (should (string-match-p "test\\.png" text))
        (should (string-match-p "Hello" text))
        (should (< (string-match-p "\\[image:" text)
                   (string-match-p "Hello" text)))))))

;;;; TAB folding in the read-only history

(defun hermes-comint-test--fold-overlay ()
  "Return the first hermes-fold overlay in the current buffer."
  (catch 'found
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'hermes-fold) (throw 'found o)))))

(ert-deftest hermes-comint-test/tab-on-tool-heading-folds-body ()
  "TAB on a tool status line folds the tool body; TAB again unfolds."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (msg (make-hermes-message
               :kind 'assistant
               :segments (vector
                          (make-hermes-segment :type 'text :content "Prose before")
                          (make-hermes-segment :type 'tool
                                               :content (make-hermes-tool
                                                         :id "t1" :name "Uniquetool"
                                                         :status 'complete
                                                         :output "secret body text")))
               :timestamp (current-time)))
         (state (make-hermes-state :session-id sid :turns (vector msg))))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        (should (string-match-p "Uniquetool" (hermes-comint-test--committed-text)))
        (goto-char (point-min))
        (search-forward "DONE Uniquetool")
        (beginning-of-line)
        (hermes-comint-tab)
        (let ((o (hermes-comint-test--fold-overlay)))
          (should o)
          (should (> (overlay-end o) (overlay-start o))))
        (should (invisible-p (line-beginning-position 2)))
        (hermes-comint-tab)
        (should-not (hermes-comint-test--fold-overlay))
        (should-not (invisible-p (line-beginning-position 2)))))))

(ert-deftest hermes-comint-test/tab-on-turn-heading-folds-whole-turn ()
  "TAB on a turn heading folds the whole turn; the next turn stays visible."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (a-msg (make-hermes-message
                 :kind 'assistant
                 :segments (vector
                            (make-hermes-segment :type 'text :content "Assistant answer")
                            (make-hermes-segment :type 'tool
                                                 :content (make-hermes-tool
                                                           :id "t1" :name "Uniquetool"
                                                           :status 'complete
                                                           :output "tool output")))
                 :timestamp (current-time)))
         (u-msg (make-hermes-message
                 :kind 'user
                 :segments (vector (make-hermes-segment
                                    :type 'text :content "Second question"))
                 :timestamp (current-time)))
         (state (make-hermes-state :session-id sid :turns (vector a-msg u-msg))))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        (goto-char (point-min))
        (search-forward "Assistant")
        (beginning-of-line)
        (hermes-comint-tab)
        (let ((o (hermes-comint-test--fold-overlay)))
          (should o)
          ;; Heading stays visible; everything under it is hidden...
          (should-not (invisible-p (point-min)))
          (should (invisible-p (line-beginning-position 2)))
          ;; ...but the following user turn is not covered.
          (save-excursion
            (search-forward "User")
            (beginning-of-line)
            (should-not (invisible-p (point)))))
        (hermes-comint-tab)
        (should-not (hermes-comint-test--fold-overlay))))))

(ert-deftest hermes-comint-test/tab-on-body-line-creates-no-fold ()
  "TAB on a body line inside the history does not create a fold."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (msg (make-hermes-message
               :kind 'assistant
               :segments (vector (make-hermes-segment
                                  :type 'text :content "Prose before"))
               :timestamp (current-time)))
         (state (make-hermes-state :session-id sid :turns (vector msg))))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        (goto-char (point-min))
        (search-forward "Prose before")
        (beginning-of-line)
        (hermes-comint-tab)
        (should-not (hermes-comint-test--fold-overlay))))))

(ert-deftest hermes-comint-test/tab-at-prompt-completes-no-fold ()
  "TAB at the prompt runs completion and creates no fold."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (state (make-hermes-state :session-id sid)))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        (goto-char (point-max))
        (hermes-comint-tab)
        (should-not (hermes-comint-test--fold-overlay))))))

;;;; Real event chains (gateway-shaped payloads) + prompts wiring

(defun hermes-comint-test--ht (&rest kvs)
  "Build a hash-table payload like the gateway's JSON objects."
  (let ((h (make-hash-table :test 'equal)))
    (while kvs (puthash (pop kvs) (pop kvs) h))
    h))

(defun hermes-comint-test--state-from-events (sid events)
  "Reduce EVENTS through `hermes--reduce' and return a state for SID."
  (let ((s (let ((acc nil))
             (dolist (e events) (setq acc (hermes--reduce acc e)))
             acc)))
    (make-hermes-state :session-id sid
                       :turns (hermes-state-turns s)
                       :stream (hermes-state-stream s))))

(ert-deftest hermes-comint-test/terminal-tool-event-paints-command-and-folds ()
  "Gateway-shaped terminal tool events paint the full command; TAB folds."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (state (hermes-comint-test--state-from-events
                 sid
                 (list (cons "message.start" nil)
                       (cons "tool.generating"
                             (hermes-comint-test--ht "tool_id" "t1" "name" "terminal"))
                       (cons "tool.start"
                             (hermes-comint-test--ht
                              "tool_id" "t1"
                              "context" "{\"command\": \"echo hello && echo world\", \"cwd\": \"/home/r\"}"))
                       (cons "tool.complete"
                             (hermes-comint-test--ht
                              "tool_id" "t1" "output" "hello\nworld\n"
                              "duration_s" 0.2))
                       (cons "message.complete"
                             (hermes-comint-test--ht "text" "All done"))))))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        ;; The full command appears in the body (formatter registered for
        ;; the `terminal' tool name, not just the generic fallback).
        (should (string-match-p "echo hello && echo world"
                                (hermes-comint-test--committed-text)))
        (goto-char (point-min))
        (search-forward "DONE")
        (beginning-of-line)
        (hermes-comint-tab)
        (let ((o (hermes-comint-test--fold-overlay)))
          (should o)
          (should (> (overlay-end o) (overlay-start o))))
        (hermes-comint-tab)
        (should-not (hermes-comint-test--fold-overlay))))))

(ert-deftest hermes-comint-test/tool-start-matching-generates-by-name ()
  "Gateway reality: tool.generating has no call id (segment keyed by
name); tool.start arrives with the provider call id + args and must
still land.  This is the flow behind the empty \"DONE $\" heading."
  (let* ((sid (hermes-comint-test--fresh-sid))
         (state (hermes-comint-test--state-from-events
                 sid
                 (list (cons "message.start" nil)
                       (cons "tool.generating"
                             (hermes-comint-test--ht "name" "terminal"))
                       (cons "tool.start"
                             (hermes-comint-test--ht
                              "tool_id" "call_01a10db8x"
                              "name" "terminal"
                              "context" ""
                              "args" (hermes-comint-test--ht
                                      "command" "echo real && echo flow")))
                       (cons "tool.complete"
                             (hermes-comint-test--ht
                              "tool_id" "call_01a10db8x" "name" "terminal"
                              "output" "real\nflow\n" "duration_s" 0.2))
                       (cons "message.complete"
                             (hermes-comint-test--ht "text" "done"))))))
    (hermes-comint-test--with-buffer buf sid state
      (with-current-buffer buf
        (should (string-match-p "echo real && echo flow"
                                (hermes-comint-test--committed-text)))
        (goto-char (point-min))
        (search-forward "DONE")
        (beginning-of-line)
        (hermes-comint-tab)
        (let ((o (hermes-comint-test--fold-overlay)))
          (should o)
          (should (> (overlay-end o) (overlay-start o))))))))

(ert-deftest hermes-comint-test/prompts-resolve-bench-buffer ()
  "The prompts watcher resolves bench buffers via the comint registry,
so approval / clarify / sudo prompts surface in bench-only sessions."
  (require 'hermes-prompts)
  (let* ((sid (hermes-comint-test--fresh-sid))
         (state (make-hermes-state :session-id sid)))
    (hermes-comint-test--with-buffer buf sid state
      (should (eq buf (hermes-prompts--session-buffer sid))))))

;;;; Edit-tool diff → ediff

(ert-deftest hermes-comint-test/diff-split-edit-pair ()
  (should (equal (hermes-comint--diff-split "- old\n+ new")
                 '("old" . "new"))))

(ert-deftest hermes-comint-test/diff-split-context-headers-and-write ()
  (let* ((unified "diff --git a/f b/f\nindex 1..2\n--- a/f\n+++ b/f\n@@ -1,2 +1,2 @@\n ctx\n-removed\n+added\n\\ No newline at end of file")
         (split (hermes-comint--diff-split unified)))
    (should (equal (car split) "ctx\nremoved"))
    (should (equal (cdr split) "ctx\nadded")))
  ;; Write-tool style: whole file as + lines, empty before.
  (let ((split (hermes-comint--diff-split "+line1\n+line2")))
    (should (equal (car split) ""))
    (should (equal (cdr split) "line1\nline2")))
  ;; Blank lines inside a diff are skipped, not joined as content.
  (should (equal (hermes-comint--diff-split "- a\n\n+ b") '("a" . "b"))))

(ert-deftest hermes-comint-test/ediff-decision-no-path ()
  (let ((d (hermes-comint--ediff-decision nil "- a\n+ b")))
    (should-not (plist-get d :ok))
    (should (string-match-p "no file path" (plist-get d :msg)))))

(ert-deftest hermes-comint-test/ediff-decision-file-gone ()
  (let ((d (hermes-comint--ediff-decision
            "/nonexistent/hermes-ediff-probe.txt" "- a\n+ b")))
    (should-not (plist-get d :ok))
    (should (string-match-p "not on disk" (plist-get d :msg)))))

(ert-deftest hermes-comint-test/ediff-decision-changed ()
  (let* ((tmp (make-temp-file "hermes-ediff-"))
         (d (progn (write-region "current content" nil tmp nil 'quiet)
                   (hermes-comint--ediff-decision tmp "- old\n+ new"))))
    (unwind-protect
        (progn
          (should-not (plist-get d :ok))
          (should (string-match-p "changed since this edit" (plist-get d :msg))))
      (delete-file tmp))))

(ert-deftest hermes-comint-test/ediff-open-against-disk ()
  "Matching file opens ediff: before buffer has old text, B visits the
real file.  Missing/changed files message instead of opening."
  (let* ((tmp (make-temp-file "hermes-ediff-"))
         (opened nil))
    (unwind-protect
        (progn
          (write-region "new" nil tmp nil 'quiet)
          (cl-letf (((symbol-function 'ediff-buffers)
                     (lambda (a b) (setq opened (list a b)))))
            ;; Matching: disk == after.
            (hermes-comint--ediff-open tmp "- old\n+ new")
            (should (consp opened))
            (should (bufferp (car opened)))
            (should (equal "old"
                           (with-current-buffer (car opened)
                             (buffer-string))))
            (should (equal tmp (buffer-file-name (cadr opened))))))
      (when (and (consp opened) (buffer-live-p (car opened)))
        (kill-buffer (car opened)))
      (when (and (consp opened) (buffer-live-p (cadr opened)))
        (kill-buffer (cadr opened)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest hermes-comint-test/ediff-open-fallback-messages ()
  "Changed file → no ediff session opened, fallback message printed."
  (let* ((tmp (make-temp-file "hermes-ediff-"))
         (opened nil))
    (unwind-protect
        (progn
          (write-region "different" nil tmp nil 'quiet)
          (cl-letf (((symbol-function 'ediff-buffers)
                     (lambda (_a _b) (setq opened t)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (when (string-match-p
                              "changed"
                              (apply #'format fmt args))
                         (setq opened 'msg)))))
            (hermes-comint--ediff-open tmp "- old\n+ new")
            (should (eq opened 'msg))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest hermes-comint-test/diff-split-glue-no-oracle ()
  "Without a disk oracle a glued `-old+new' line stays whole."
  (should (equal (hermes-comint--diff-split "-alpha+beta")
                 '("alpha+beta" . ""))))

(ert-deftest hermes-comint-test/diff-split-glue-oracle-resolves ()
  "With a disk oracle the glued line splits at the right boundary."
  (should (equal (hermes-comint--diff-split "-alpha+beta" "beta")
                 '("alpha" . "beta")))
  ;; candidate choice matters: old `a+b' glued with new `ab' difflib-welds
  ;; to `-a+b+ab'; the split must land at the second `+'
  (should (equal (hermes-comint--diff-split "-a+b+ab" "ab")
                 '("a+b" . "ab"))))

(ert-deftest hermes-comint-test/diff-split-clean-ambiguous-keeps-whole ()
  "A clean `-' line whose content contains `+' is never split."
  (should (equal (hermes-comint--diff-split "- a + b\n+ x" "x")
                 '("a + b" . "x"))))

(ert-deftest hermes-comint-test/ediff-decision-glued-resolves ()
  "A difflib-glued wire diff resolves against the real file content."
  (let* ((tmp (make-temp-file "hermes-ediff-")))
    (unwind-protect
        (progn
          (write-region "beta" nil tmp nil 'quiet)
          (let* ((diff (concat "display.diff.review_header\n"
                               "a" tmp " → b" tmp "\n"
                               "@@ -1 +1 @@\n"
                               "-alpha+beta"))
                 (d (hermes-comint--ediff-decision tmp diff)))
            (should (plist-get d :ok))
            (should (equal "alpha" (plist-get d :before)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest hermes-comint-test/diff-tool-path-bare-context ()
  "Bare-path context (gateway `patch' shape) resolves to itself."
  (let ((tool (make-hermes-tool :id "t" :name "patch" :status 'complete
                                :context "/tmp/hermes-bare-target.txt"
                                :inline-diff "-a\n+b")))
    (should (equal "/tmp/hermes-bare-target.txt"
                   (hermes-comint--diff-tool-path tool)))))

(ert-deftest hermes-comint-test/diff-tool-path-arrow-fallback ()
  "No usable context: the path is harvested from the arrow line."
  (let* ((tool (make-hermes-tool :id "t" :name "patch" :status 'complete
                                 :context nil
                                 :inline-diff
                                 (concat "display.diff.review_header\n"
                                         "a//tmp/x.txt → b//tmp/x.txt\n"
                                         "@@ -1 +1 @@\n-a+b"))))
    (should (equal "/tmp/x.txt" (hermes-comint--diff-tool-path tool)))))

(ert-deftest hermes-comint-test/formatter-patch-registered ()
  "Gateway file-tool names reach the edit formatter."
  (should (eq (hermes-tool--lookup "patch") #'hermes-tool-format-edit))
  (should (eq (hermes-tool--lookup "write_file") #'hermes-tool-format-edit)))

(ert-deftest hermes-comint-test/tool-block-wires-diff-ret-keymap ()
  "A complete edit tool with file_path + inline-diff gets a keymap
property on the diff text whose RET opens ediff; tools without a path
get none."
  (let* ((tmp (make-temp-file "hermes-ediff-"))
         (diff "- old\n+ new")
         (tool (make-hermes-tool
                :id "e1" :name "Edit" :status 'complete
                :context (format "{\"file_path\":\"%s\"}" tmp)
                :inline-diff diff)))
    (unwind-protect
        (progn
          (write-region "new" nil tmp nil 'quiet)
          (with-temp-buffer
            (hermes-comint--insert-tool-block tool)
            (goto-char (point-min))
            (should (search-forward diff nil t))
            (let ((km (get-text-property (1- (point)) 'keymap)))
              (should km)
              (should (lookup-key km (kbd "RET")))
              ;; Drive the RET closure with ediff stubbed out.
              (let ((opened nil))
                (cl-letf (((symbol-function 'ediff-buffers)
                           (lambda (a b) (setq opened (list a b)))))
                  (call-interactively (lookup-key km (kbd "RET")))
                  (should (consp opened))
                  (should (equal "old"
                                 (with-current-buffer (car opened)
                                   (buffer-string)))))))
            ;; No path in context → no keymap on the diff.
            (let ((tool2 (make-hermes-tool
                          :id "e2" :name "Edit" :status 'complete
                          :context "{}"
                          :inline-diff diff)))
              (erase-buffer)
              (hermes-comint--insert-tool-block tool2)
              (goto-char (point-min))
              (search-forward diff nil t)
              (should-not (get-text-property (1- (point)) 'keymap)))))
      (ignore-errors (delete-file tmp)))))

(provide 'hermes-comint-test)
;;; hermes-comint-test.el ends here
