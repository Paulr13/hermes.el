;;; hermes-subagents-test.el --- tests for hermes-subagents.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'hermes-state-test)
(require 'hermes-test-helpers)
(require 'hermes-state)
(require 'hermes-subagents)

;;;; Pure formatter

(ert-deftest hermes-subagents-test/format-empty ()
  "Empty or missing subagent vector renders the placeholder."
  (should (string-prefix-p "No subagents"
                           (hermes-subagents--format nil)))
  (should (string-prefix-p "No subagents" (hermes-subagents--format []))))

(ert-deftest hermes-subagents-test/format-running-block ()
  "A running subagent shows status, goal, tools, notes and thinking tail."
  (let* ((s (hermes-test--reduce*
             nil
             (cons "message.start" nil)
             (cons "subagent.spawn_requested"
                   (hermes-test--ht "subagent_id" "sa-1" "goal" "Mirror files to fork"))
             (cons "subagent.start" (hermes-test--ht "subagent_id" "sa-1"))
             (cons "subagent.tool"
                   (hermes-test--ht "subagent_id" "sa-1" "tool_name" "tool_search"
                                    "args" (hermes-test--ht "query" "github push files")))
             (cons "subagent.progress"
                   (hermes-test--ht "subagent_id" "sa-1" "note" "fetching base blobs"))
             (cons "subagent.thinking"
                   (hermes-test--ht "subagent_id" "sa-1" "text" "verified base match"))))
            (stream (hermes-state-stream s))
            (sa (aref (hermes-stream-subagents stream) 0))
            (text (hermes-subagents--format (hermes-stream-subagents stream))))
    (should (eq 'running (hermes-subagent-status sa)))
    (should (string-match-p "⏺ RUNNING · sa-1" text))
    (should (string-match-p "Goal: Mirror files to fork" text))
    (should (string-match-p "├ tool_search(github push files)" text))
    (should (string-match-p "├ fetching base blobs" text))
    (should (string-match-p "⋯ verified base match" text))
    ;; No summary while running.
    (should-not (string-match-p "→ " text))))

(ert-deftest hermes-subagents-test/format-complete-block ()
  "A completed subagent shows the summary and duration, not thinking."
  (let* ((s (hermes-test--reduce*
             nil
             (cons "message.start" nil)
             (cons "subagent.spawn_requested"
                   (hermes-test--ht "subagent_id" "sa-2" "goal" "Do the thing"))
             (cons "subagent.tool"
                   (hermes-test--ht "subagent_id" "sa-2" "tool_name" "push_files"
                                    "args" (hermes-test--ht "path" "/a/b.el")))
             (cons "subagent.complete"
                   (hermes-test--ht "subagent_id" "sa-2" "status" "complete"
                                    "summary" "Mirrored 2 files, hashes verified"
                                    "duration_s" 123))))
            (text (hermes-subagents--format
                   (hermes-stream-subagents (hermes-state-stream s)))))
    (should (string-match-p "✔ COMPLETE · sa-2 · 2m03s" text))
    (should (string-match-p "→ Mirrored 2 files, hashes verified" text))
    (should (string-match-p "├ push_files(/a/b.el)" text))
    ;; Completed subagents stop streaming thinking.
    (should-not (string-match-p "⋯ " text))))

(ert-deftest hermes-subagents-test/format-truncates-args ()
  "Long single-arg tool calls and multi-line goals stay on one line."
  (let* ((long (make-string 200 ?x))
         (s (hermes-test--reduce*
             nil
             (cons "message.start" nil)
             (cons "subagent.spawn_requested"
                   (hermes-test--ht "subagent_id" "sa-3"
                                    "goal" (concat "line1\nline2 " long)))
             (cons "subagent.tool"
                   (hermes-test--ht "subagent_id" "sa-3" "tool_name" "terminal"
                                    "args" (hermes-test--ht "command" long)))))
            (text (hermes-subagents--format
                   (hermes-stream-subagents (hermes-state-stream s)))))
    (dolist (line (split-string text "\n"))
      (should-not (string-match-p "\n" line)))
    (should (string-match-p "line1 line2" text))
    (should (string-match-p "…)$" (car (last (split-string text "\n" t)))))))

;;;; Change detection

(ert-deftest hermes-subagents-test/spawned-p-detects-lifecycle ()
  "spawned-p: count increase and queued→running are spawns; progress isn't."
  (let* ((base (hermes-test--reduce*
                nil
                (cons "message.start" nil)
                (cons "subagent.spawn_requested"
                      (hermes-test--ht "subagent_id" "sa-9" "goal" "G"))))
         (sas0 (hermes-stream-subagents (hermes-state-stream base)))
         (running (hermes-test--reduce*
                   base
                   (cons "subagent.start"
                         (hermes-test--ht "subagent_id" "sa-9"))))
         (sas1 (hermes-stream-subagents (hermes-state-stream running)))
         (tooling (hermes-test--reduce*
                   running
                   (cons "subagent.tool"
                         (hermes-test--ht "subagent_id" "sa-9"
                                          "tool_name" "t" "args" nil))))
         (sas2 (hermes-stream-subagents (hermes-state-stream tooling))))
    (should (hermes-subagents--spawned-p [] sas0))
    (should (hermes-subagents--spawned-p sas0 sas1))
    (should-not (hermes-subagents--spawned-p sas1 sas2))))

(ert-deftest hermes-subagents-test/duration-format ()
  (should (equal "" (hermes-subagents--fmt-duration nil)))
  (should (equal "42s" (hermes-subagents--fmt-duration 42)))
  (should (equal "2m03s" (hermes-subagents--fmt-duration 123)))
  (should (equal "1h05m" (hermes-subagents--fmt-duration 3900))))

;;;; Detail buffer

(ert-deftest hermes-subagents-test/format-detail-shows-everything ()
  "Detail format includes full goal, all tools, notes, thinking, summary."
  (let* ((s (hermes-test--reduce*
             nil
             (cons "message.start" nil)
             (cons "subagent.spawn_requested"
                   (hermes-test--ht "subagent_id" "sa-d" "goal" "Mirror files"))
             (cons "subagent.start" (hermes-test--ht "subagent_id" "sa-d"))
             (cons "subagent.tool"
                   (hermes-test--ht "subagent_id" "sa-d" "tool_name" "push_files"
                                    "args" (hermes-test--ht "path" "/a.el")))
             (cons "subagent.tool"
                   (hermes-test--ht "subagent_id" "sa-d" "tool_name" "terminal"
                                    "args" (hermes-test--ht "command" "sha256sum /a.el")))
             (cons "subagent.progress"
                   (hermes-test--ht "subagent_id" "sa-d" "note" "hashed blobs"))
             (cons "subagent.thinking"
                   (hermes-test--ht "subagent_id" "sa-d" "text" "verified"))))
          (sa (aref (hermes-stream-subagents (hermes-state-stream s)) 0))
            (text (mapconcat #'identity
                             (hermes-subagents--format-detail sa) "\n")))
    (should (string-match-p "ID: sa-d" text))
    (should (string-match-p "Goal: Mirror files" text))
    (should (string-match-p "Tools (2):" text))
    (should (string-match-p "push_files(/a\\.el)" text))
    (should (string-match-p "terminal(sha256sum /a\\.el)" text))
    (should (string-match-p "Progress (1):" text))
    (should (string-match-p "hashed blobs" text))
    (should (string-match-p "verified" text))))

(ert-deftest hermes-subagents-test/detail-buffer-lives-updates ()
  "detail-open creates the buffer; later events update it in place."
  (let* ((s1 (hermes-test--reduce*
              nil
              (cons "message.start" nil)
              (cons "subagent.spawn_requested"
                    (hermes-test--ht "subagent_id" "sa-x" "goal" "G"))
              (cons "subagent.start" (hermes-test--ht "subagent_id" "sa-x"))))
         (state (make-hermes-state :stream (hermes-state-stream s1))))
    (puthash "sess-1" state hermes--sessions)
    (unwind-protect
        (let ((buf (hermes-subagents-detail-open "sa-x")))
          (should (equal "*hermes-subagent:sa-x*" (buffer-name buf)))
          (should (buffer-local-value 'buffer-read-only buf))
          (with-current-buffer buf
            (should (string-match-p "RUNNING" (buffer-string)))
            ;; Simulate a live update: subagent completes.
            (let ((s2 (hermes-test--reduce*
                       s1
                       (cons "subagent.complete"
                             (hermes-test--ht "subagent_id" "sa-x"
                                              "status" "complete"
                                              "summary" "Done"
                                              "duration_s" 42)))))
              (puthash "sess-1"
                       (make-hermes-state :stream (hermes-state-stream s2))
                       hermes--sessions)
              (hermes-subagents--render-details
               (hermes-stream-subagents (hermes-state-stream s2))))
            (should (string-match-p "Summary: Done" (buffer-string)))
            (should (string-match-p "✔ COMPLETE" (buffer-string)))))
      (when (get-buffer "*hermes-subagent:sa-x*")
        (kill-buffer "*hermes-subagent:sa-x*"))
      (remhash "sess-1" hermes--sessions))))

(ert-deftest hermes-subagents-test/detail-open-unknown-id-errors ()
  (should-error (hermes-subagents-detail-open "does-not-exist")))

(ert-deftest hermes-subagents-test/find-scans-sessions ()
  "--find returns (sa . sid) across all sessions."
  (let* ((s (hermes-test--reduce*
             nil
             (cons "message.start" nil)
             (cons "subagent.spawn_requested"
                   (hermes-test--ht "subagent_id" "sa-f" "goal" "G"))))
         (state (make-hermes-state :stream (hermes-state-stream s))))
    (puthash "sess-7" state hermes--sessions)
    (unwind-protect
        (let ((res (hermes-subagents--find "sa-f")))
          (should (equal "sa-f" (hermes-subagent-id (car res))))
          (should (equal "sess-7" (cdr res))))
      (remhash "sess-7" hermes--sessions))))

(ert-deftest hermes-subagents-test/find-falls-back-to-committed-turns ()
  "Post-commit the stream is nil; --find still locates the subagent
via the committed turn's copy."
  (let* ((s (hermes-test--reduce*
             nil
             (cons "message.start" nil)
             (cons "subagent.spawn_requested"
                   (hermes-test--ht "subagent_id" "sa-t" "goal" "Mirror it"))
             (cons "message.complete" nil))))
    (should (null (hermes-state-stream s)))
    (puthash "sess-turns" s hermes--sessions)
    (unwind-protect
        (let ((res (hermes-subagents--find "sa-t")))
          (should res)
          (should (equal "Mirror it" (hermes-subagent-goal (car res))))
          (should (equal "sess-turns" (cdr res)))
          (should (null (hermes-subagents--find "sa-unknown"))))
      (remhash "sess-turns" hermes--sessions))))

(ert-deftest hermes-subagents-test/detail-open-after-commit-renders-late-summary ()
  "Post-commit: detail-open renders from the committed turn, and a late
completion event refreshes the open overview/detail buffers."
  (let* ((s (hermes-test--reduce*
             nil
             (cons "message.start" nil)
             (cons "subagent.spawn_requested"
                   (hermes-test--ht "subagent_id" "sa-l" "goal" "Count hippos"))
             (cons "message.complete" nil)))
            (s2 (hermes-test--reduce*
                 s
                 (cons "subagent.complete"
                       (hermes-test--ht "subagent_id" "sa-l"
                                        "status" "complete"
                                        "summary" "five hippos"))))
            (hermes--current-session-id "sess-late"))
    (puthash "sess-late" s hermes--sessions)
    (unwind-protect
        (progn
          (hermes-subagents-detail-open "sa-l")
          (with-current-buffer (hermes-subagents--detail-buffer-name "sa-l")
            (should (string-match-p "Count hippos" (buffer-string))))
          ;; Overview buffer exists so --render (and the detail refresh
          ;; it triggers) runs on state change.
          (hermes-subagents-open "sess-late")
          (puthash "sess-late" s2 hermes--sessions)
          (hermes-subagents--on-state-change s s2)
          (with-current-buffer (hermes-subagents--detail-buffer-name "sa-l")
            (should (string-match-p "five hippos" (buffer-string))))
          (with-current-buffer hermes-subagents-buffer-name
            (should (string-match-p "COMPLETE" (buffer-string)))))
      (remhash "sess-late" hermes--sessions)
      (ignore-errors (kill-buffer (hermes-subagents--detail-buffer-name "sa-l")))
      (ignore-errors (kill-buffer hermes-subagents-buffer-name))
      (setq hermes-subagents--fingerprint nil))))

(provide 'hermes-subagents-test)
;;; hermes-subagents-test.el ends here
