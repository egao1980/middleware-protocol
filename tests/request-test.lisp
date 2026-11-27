;;;; Requests and their property bag; visibility to inner and outer layers,
;;;; also for deferred responses (request.lisp, D6).

(in-package #:middleware-protocol/tests)

(deftest property-bag-basics
  (let ((request (make-instance 'request)))
    (ok (equal '(nil nil) (multiple-value-list (request-property request :a))))
    (ok (equal '(:none nil) (multiple-value-list (request-property request :a :none))))
    (ok (eq 1 (setf (request-property request :a) 1)))
    (ok (equal '(1 t) (multiple-value-list (request-property request :a :none))))
    (setf (request-property request :a) 2)
    (ok (eql 2 (request-property request :a)) "setting again replaces")
    (setf (request-property request :b) nil)
    (ok (equal '(nil t) (multiple-value-list (request-property request :b :none)))
        "a NIL value is present")
    (setf (request-property request :c :ignored-default) 3)
    (ok (eql 3 (request-property request :c)))
    (ok (remove-request-property request :a))
    (ok (not (remove-request-property request :a)) "removing an absent key → false")
    (ok (equal '(nil nil) (multiple-value-list (request-property request :a))))))

(deftest property-keys-compare-with-eql
  (let ((request (make-instance 'request)))
    (setf (request-property request 'session) :symbol-key)
    (setf (request-property request 42) :number-key)
    (ok (eq :symbol-key (request-property request 'session)))
    (ok (eq :number-key (request-property request 42)))
    (ok (null (nth-value 1 (request-property request "session"))))
    (ok (null (nth-value 1 (request-property request :session))))))

(deftest request-properties-is-a-fresh-plist
  (let ((request (make-instance 'request :properties (list :a 1 :b 2))))
    (setf (request-property request :c 3) 3)
    (let ((plist (request-properties request)))
      (ok (= 6 (length plist)))
      (ok (eql 1 (getf plist :a)))
      (ok (eql 2 (getf plist :b)))
      (ok (eql 3 (getf plist :c)))
      (setf (getf plist :a) 100)
      (ok (eql 1 (request-property request :a)) "the copy does not write through"))))

(deftest initial-properties-are-copied
  (let* ((initial (list :a 1))
         (request (make-instance 'request :properties initial)))
    (setf (request-property request :a) 2
          (request-property request :b) 3)
    (ok (equal '(:a 1) initial) "the caller's list is not modified")
    (ok (eql 2 (request-property request :a)))))

(deftest non-request-objects-have-no-property-bag
  (ok (equal '(:none nil)
             (multiple-value-list (request-property (make-instance 'request) :a :none))))
  (ok (signals (request-property "a string" :a) 'error))
  (ok (signals (setf (request-property (list :a 1) :a) 2) 'error)))

;;; Visibility: M-CONTEXT sets :FROM-OUTER before the primary and records what
;;; the inner layers left in :FROM-INNER when the response is finished.
(defvar *observed* nil)

(defclass m-context (handler) ())

(defmethod prepare-request ((handler m-context) (request request))
  (setf (request-property request :from-outer) :outer-value)
  (call-next-method))

(defmethod finish-response ((handler m-context) (request request) response)
  (push (request-property request :from-inner :absent) *observed*)
  (call-next-method))

(deftest properties-flow-in-and-out-of-inner-layers
  (let* ((*observed* '())
         (handler (make-handler '(m-context function-handler)
                                :function (lambda (request)
                                            (setf (request-property request :from-inner)
                                                  (list :saw (request-property request :from-outer)))
                                            :done)))
         (request (make-instance 'request)))
    (ok (eq :done (handle handler request)))
    (ok (equal '((:saw :outer-value)) *observed*))
    (ok (eq :outer-value (request-property request :from-outer))
        "the caller's request object carries them")))

(deftest properties-reach-a-deferred-response
  (let* ((*observed* '())
         (handler (make-handler
                   '(m-context function-handler)
                   :function (lambda (request)
                               ;; The body runs later, when the server calls it.
                               (lambda (responder)
                                 (setf (request-property request :from-inner) :set-by-the-body)
                                 (funcall responder (list 200 '() '("body")))))))
         (response (handle handler (make-instance 'request))))
    (ok (functionp response))
    (ok (null *observed*) "nothing finished before the body runs")
    (run-streaming response)
    (ok (equal '(:set-by-the-body) *observed*)
        "FINISH-RESPONSE, run when the head is sent, sees what the body set")))

(deftest properties-reach-an-asynchronous-response
  (multiple-value-bind (promise resolve) (pending-promise)
    (let* ((*observed* '())
           (request (make-instance 'request))
           (handler (make-handler '(m-context function-handler) :function (constantly promise)))
           (response (handle handler request)))
      (ok (null *observed*))
      (setf (request-property request :from-inner) :set-before-resolving)
      (funcall resolve :value)
      (ok (equal '(:resolved :value) (multiple-value-list (settle response))))
      (ok (equal '(:set-before-resolving) *observed*)))))
