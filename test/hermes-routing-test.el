;;; hermes-routing-test.el --- Tests for server→client request routing -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'hermes)

(defun hermes-routing-test--ht (&rest kvs)
  "Build a hash-table from KVS, a flat plist-style list of \"key\" \"value\"."
  (let ((h (make-hash-table :test 'equal)))
    (while kvs
      (puthash (pop kvs) (pop kvs) h))
    h))

(ert-deftest hermes-routing-test/approval-synthesizes-legacy-event ()
  "The frame's srq id becomes request_id; session_id is stripped."
  (let ((dispatched nil) (ui-dispatched nil))
    (cl-letf (((symbol-function 'hermes-dispatch)
               (lambda (msg &optional sid) (push (list msg sid) dispatched)))
              ((symbol-function 'hermes-ui-dispatch)
               (lambda (msg &optional sid) (push (list msg sid) ui-dispatched))))
      (hermes--route-server-request
       "approval" "srq-9"
       (hermes-routing-test--ht "session_id" "s1" "request_id" "apr-1"
                                "command" "ls"))
      (should (= 1 (length dispatched)))
      (let* ((msg (nth 0 (car dispatched)))
             (sid (nth 1 (car dispatched)))
             (payload (cdr msg)))
        (should (equal "approval.request" (car msg)))
        (should (equal "s1" sid))
        (should (equal "srq-9" (gethash "request_id" payload)))
        (should (equal "ls" (gethash "command" payload)))
        (should (null (gethash "session_id" payload))))
      (should (= 1 (length ui-dispatched))))))

(ert-deftest hermes-routing-test/unknown-method-dispatches-under-its-name ()
  (let ((dispatched nil))
    (cl-letf (((symbol-function 'hermes-dispatch)
               (lambda (msg &optional _sid) (push msg dispatched)))
              ((symbol-function 'hermes-ui-dispatch)
               (lambda (&rest _))))
      (hermes--route-server-request
       "vault.unlock_prompt" "srq-a"
       (hermes-routing-test--ht "session_id" "s1" "backend" "onepassword"
                                "display_name" "1Password"))
      (let* ((msg (car (last dispatched)))
             (payload (cdr msg)))
        (should (equal "vault.unlock_prompt" (car msg)))
        (should (equal "srq-a" (gethash "request_id" payload)))))))

(ert-deftest hermes-routing-test/server-request-without-session-does-not-dispatch ()
  (let ((dispatched nil))
    (cl-letf (((symbol-function 'hermes-dispatch)
               (lambda (&rest _) (push t dispatched)))
              ((symbol-function 'hermes-ui-dispatch)
               (lambda (&rest _))))
      (hermes--route-server-request "approval" "srq-b" (hermes-routing-test--ht))
      (should (null dispatched)))))

(provide 'hermes-routing-test)
;;; hermes-routing-test.el ends here
