;;; hermes-rpc-test.el --- ERT tests for the JSON-RPC transport -*- lexical-binding: t; -*-

(require 'ert)
(require 'hermes-rpc)

(defun hermes-rpc-test--ht (&rest kvs)
  "Build a hash-table from KVS, a flat plist-style list of \"key\" \"value\"."
  (let ((h (make-hash-table :test 'equal)))
    (while kvs
      (puthash (pop kvs) (pop kvs) h))
    h))

;;;; Server→client request dispatch

(ert-deftest hermes-rpc-test/server-request-dispatches-hook ()
  (let ((hermes-rpc-server-request-functions nil)
        (captured nil))
    (add-hook 'hermes-rpc-server-request-functions
              (lambda (method id params)
                (push (list method id params) captured)))
    (hermes-rpc--dispatch-frame
     (hermes-rpc-test--ht "jsonrpc" "2.0"
                          "id" "srq-abc123def456"
                          "method" "approval"
                          "params" (hermes-rpc-test--ht
                                    "session_id" "s1"
                                    "request_id" "a1"
                                    "command" "ls")))
    (should (= 1 (length captured)))
    (should (equal "approval" (nth 0 (car captured))))
    (should (equal "srq-abc123def456" (nth 1 (car captured))))
    (should (equal "ls" (gethash "command" (nth 2 (car captured)))))))

(ert-deftest hermes-rpc-test/response-frame-never-dispatched-as-server-request ()
  (let ((server-captured nil)
        (cb-called nil)
        (hermes-rpc-server-request-functions
         (list (lambda (&rest _) (push t server-captured))))
        (hermes-rpc--pending (make-hash-table :test 'eql)))
    (puthash 7 (lambda (r e) (setq cb-called (list r e))) hermes-rpc--pending)
    (hermes-rpc--dispatch-frame
     (hermes-rpc-test--ht "jsonrpc" "2.0" "id" 7 "result" "x"))
    (should (null server-captured))
    (should (equal '("x" nil) cb-called))
    (should (null (gethash 7 hermes-rpc--pending)))))

(ert-deftest hermes-rpc-test/error-response-routes-to-callback ()
  (let ((hermes-rpc--pending (make-hash-table :test 'eql))
        (captured nil))
    (puthash 3 (lambda (r e) (setq captured (list r e))) hermes-rpc--pending)
    (hermes-rpc--dispatch-frame
     (hermes-rpc-test--ht "jsonrpc" "2.0" "id" 3
                          "error" (hermes-rpc-test--ht "code" 4002 "message" "bad")))
    (should (null (nth 0 captured)))
    (should (= 4002 (gethash "code" (nth 1 captured))))))

(ert-deftest hermes-rpc-test/event-still-dispatches-hook ()
  (let ((hermes-rpc-event-functions nil)
        (captured nil))
    (add-hook 'hermes-rpc-event-functions
              (lambda (type sid payload) (push (list type sid payload) captured)))
    (hermes-rpc--dispatch-frame
     (hermes-rpc-test--ht "method" "event"
                          "params" (hermes-rpc-test--ht
                                    "type" "message.complete"
                                    "session_id" "s1"
                                    "payload" (hermes-rpc-test--ht))))
    (should (equal "message.complete" (nth 0 (car captured))))
    (should (equal "s1" (nth 1 (car captured))))
    (should (equal "message.complete" (car (car captured))))))

(ert-deftest hermes-rpc-test/unrecognised-frame-does-not-error ()
  ;; A frame with neither method nor id only logs; nothing dispatches.
  (let ((server-captured nil)
        (event-captured nil)
        (hermes-rpc-server-request-functions
         (list (lambda (&rest _) (push t server-captured))))
        (hermes-rpc-event-functions
         (list (lambda (&rest _) (push t event-captured)))))
    (hermes-rpc--dispatch-frame (hermes-rpc-test--ht "jsonrpc" "2.0"))
    (should (null server-captured))
    (should (null event-captured))))

;;;; gateway.ready advertises server→client request support

(ert-deftest hermes-rpc-test/ready-advertises-server-requests ()
  (let ((hermes-rpc--state 'starting)
        (hermes-rpc--pending-frames nil)
        (calls nil))
    (cl-letf (((symbol-function 'hermes-rpc-request)
               (lambda (method params &optional _cb)
                 (push (cons method params) calls)))
              ((symbol-function 'hermes-rpc--flush-pending) (lambda ())))
      (hermes-rpc--dispatch-frame
       (hermes-rpc-test--ht "method" "event"
                            "params" (hermes-rpc-test--ht
                                      "type" "gateway.ready"
                                      "session_id" ""
                                      "payload" (hermes-rpc-test--ht "skin" "default"))))
      (should (eq 'ready hermes-rpc--state))
      (should (equal "client.capabilities" (car (car calls))))
      (should (eq t (plist-get (cdr (car calls)) :server_requests))))))

;;;; Response frames

(ert-deftest hermes-rpc-test/response-frame-shape ()
  (should (equal '(:jsonrpc "2.0" :id "srq-1" :result "y")
                 (hermes-rpc--response-frame "srq-1" "y")))
  (should (equal '(:jsonrpc "2.0" :id "srq-2" :error (:code 4404 :message "not shown"))
                 (hermes-rpc--error-frame "srq-2" 4404 "not shown"))))

(ert-deftest hermes-rpc-test/respond-sends-response-frame ()
  (let ((sent nil))
    (cl-letf (((symbol-function 'hermes-rpc-live-p) (lambda () t))
              ((symbol-function 'hermes-rpc--send) (lambda (frame) (push frame sent))))
      (hermes-rpc-respond "srq-x" (list :choice "once"))
      (should (equal '(:jsonrpc "2.0" :id "srq-x" :result (:choice "once"))
                     (car sent)))
      (hermes-rpc-respond-error "srq-y" 4404 "decline")
      (should (equal '(:jsonrpc "2.0" :id "srq-y" :error (:code 4404 :message "decline"))
                     (car sent))))))

(ert-deftest hermes-rpc-test/respond-without-gateway-is-noop ()
  (let ((hermes-rpc--process nil))
    (should (null (hermes-rpc-respond "srq-1" "x")))
    (should (null (hermes-rpc-respond-error "srq-1" 4404 "x")))))

(provide 'hermes-rpc-test)
;;; hermes-rpc-test.el ends here
