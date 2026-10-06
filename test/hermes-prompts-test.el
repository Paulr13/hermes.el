;;; hermes-prompts-test.el --- ERT tests for prompt reply paths -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'hermes-prompts)

(defun hermes-prompts-test--ht (&rest kvs)
  "Build a hash-table from KVS, a flat plist-style list of \"key\" \"value\"."
  (let ((h (make-hash-table :test 'equal)))
    (while kvs
      (puthash (pop kvs) (pop kvs) h))
    h))

(defvar hermes-prompts-test--answered nil
  "Captured (ID RESULT) pairs from the stubbed `hermes-rpc-respond'.")

(defmacro hermes-prompts-test--capture-responds (&rest body)
  "Run BODY with `hermes-rpc-respond' stubbed to record (ID RESULT)."
  (declare (indent 0))
  `(let ((hermes-prompts-test--answered nil))
     (cl-letf (((symbol-function 'hermes-rpc-respond)
                (lambda (id result)
                  (push (list id result) hermes-prompts-test--answered))))
       ,@body)))

(defun hermes-prompts-test--last-answer ()
  "Return the most recent captured (ID RESULT)."
  (car hermes-prompts-test--answered))

;;;; Approval

(ert-deftest hermes-prompts-test/approval-responds-with-choice ()
  (hermes-prompts-test--capture-responds
   (cl-letf (((symbol-function 'read-multiple-choice)
              (lambda (&rest _) '(?o "once" "allow this single invocation"))))
     (hermes--prompt-approval "s1" "srq-1"
                              (hermes-prompts-test--ht
                               "command" "ls" "description" "list files")))
   (should (equal '("srq-1" (:choice "once")) (hermes-prompts-test--last-answer)))))

(ert-deftest hermes-prompts-test/approval-session-choice ()
  (hermes-prompts-test--capture-responds
   (cl-letf (((symbol-function 'read-multiple-choice)
              (lambda (&rest _) '(?s "session" "allow for this session"))))
     (hermes--prompt-approval "s1" "srq-1" (hermes-prompts-test--ht)))
   (should (equal '("srq-1" (:choice "session")) (hermes-prompts-test--last-answer)))))

(ert-deftest hermes-prompts-test/approval-quit-denies ()
  (hermes-prompts-test--capture-responds
   (cl-letf (((symbol-function 'read-multiple-choice)
              (lambda (&rest _) (signal 'quit nil))))
     (hermes--prompt-approval "s1" "srq-2" (hermes-prompts-test--ht)))
   (should (equal '("srq-2" (:choice "deny")) (hermes-prompts-test--last-answer)))))

;;;; Clarify (batch)

(ert-deftest hermes-prompts-test/clarify-answers-whole-batch-in-one-frame ()
  (hermes-prompts-test--capture-responds
   (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "b"))
             ((symbol-function 'read-string) (lambda (&rest _) "free answer")))
     (hermes--prompt-clarify
      "srq-4"
      (hermes-prompts-test--ht
       "questions" (vector
                    (hermes-prompts-test--ht "qid" "q1" "question" "Pick:"
                                             "choices" ["a" "b"])
                    (hermes-prompts-test--ht "qid" "q2" "question" "Free:")))))
   (let* ((ans (hermes-prompts-test--last-answer))
          (result (nth 1 ans)))
     (should (equal "srq-4" (nth 0 ans)))
     ;; Batch contract: the result must WRAP the answers in an `answers'
     ;; key — a frame without it is cancel-all per the gateway contract.
     (should (plist-get result :answers))
     (let ((answers (plist-get result :answers)))
       (should (hash-table-p answers))
       (should (equal "b" (gethash "q1" answers)))
       (should (equal "free answer" (gethash "q2" answers)))))))

(ert-deftest hermes-prompts-test/clarify-quit-responds-cancel-all ()
  (hermes-prompts-test--capture-responds
   (cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) (signal 'quit nil))))
     (hermes--prompt-clarify
      "srq-5"
      (hermes-prompts-test--ht
       "questions" (vector (hermes-prompts-test--ht "qid" "q1" "question" "Pick:"
                                                    "choices" ["a" "b"])))))
   (let* ((ans (hermes-prompts-test--last-answer))
          (result (nth 1 ans)))
     (should (equal "srq-5" (nth 0 ans)))
     (should (hash-table-p result))
     (should (= 0 (hash-table-count result)))
     ;; A frame without `answers' is cancel-all.
     (should (null (gethash "answers" result))))))

(ert-deftest hermes-prompts-test/clarify-without-questions-responds-cancel-all ()
  (hermes-prompts-test--capture-responds
   (hermes--prompt-clarify "srq-6" (hermes-prompts-test--ht))
   (let* ((ans (hermes-prompts-test--last-answer))
          (result (nth 1 ans)))
     (should (equal "srq-6" (nth 0 ans)))
     (should (hash-table-p result))
     (should (= 0 (hash-table-count result))))))

;;;; Sudo / secret

(ert-deftest hermes-prompts-test/sudo-responds-with-value ()
  (hermes-prompts-test--capture-responds
   (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "hunter2")))
     (hermes--prompt-sudo "srq-2"))
   (should (equal '("srq-2" (:value "hunter2")) (hermes-prompts-test--last-answer)))))

(ert-deftest hermes-prompts-test/secret-responds-with-value ()
  (hermes-prompts-test--capture-responds
   (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "sk-test")))
     (hermes--prompt-secret "srq-3"
                            (hermes-prompts-test--ht
                             "env_var" "OPENAI_API_KEY"
                             "prompt" "Value for OPENAI_API_KEY: ")))
   (should (equal '("srq-3" (:value "sk-test")) (hermes-prompts-test--last-answer)))))

(provide 'hermes-prompts-test)
;;; hermes-prompts-test.el ends here
