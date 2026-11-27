;;;; Handlers: functions, objects, FUNCTION-HANDLER, the default hooks
;;;; (handler.lisp).

(in-package #:middleware-protocol/tests)

(deftest function-is-a-handler
  (ok (equal '(:got :ping) (handle (lambda (request) (list :got request)) :ping)))
  (ok (= 3 (handle #'1+ 2))))

;;; A handler object with state that is not a HANDLER: any class with a
;;; primary method on HANDLE.
(defclass counter-handler ()
  ((count :initform 0 :accessor counter-count)))

(defmethod handle ((handler counter-handler) request)
  (list request (incf (counter-count handler))))

(deftest object-handler-with-state
  (let ((counter (make-instance 'counter-handler)))
    (ok (equal '(:a 1) (handle counter :a)))
    (ok (equal '(:b 2) (handle counter :b)))
    (ok (= 2 (counter-count counter)))))

;;; A terminal that is a HANDLER, with state in a slot.
(defclass greeting-handler (handler)
  ((greeting :initarg :greeting :reader greeting)
   (calls :initform 0 :accessor greeting-calls)))

(defmethod handle ((handler greeting-handler) request)
  (incf (greeting-calls handler))
  (format nil "~A, ~A" (greeting handler) request))

(deftest handler-subclass-terminal
  (let ((handler (make-instance 'greeting-handler :greeting "Hello")))
    (ok (equal "Hello, Ann" (handle handler "Ann")))
    (ok (equal "Hello, Bob" (handle handler "Bob")))
    (ok (= 2 (greeting-calls handler)))))

(deftest function-handler-calls-its-function
  (let* ((seen nil)
         (handler (make-instance 'function-handler
                                 :function (lambda (request)
                                             (setf seen request)
                                             (list :answer request)))))
    (ok (typep handler 'handler))
    (ok (functionp (handler-function handler)))
    (ok (equal '(:answer :q) (handle handler :q)))
    (ok (eq :q seen))))

(defun %test-terminal (request)
  (list :first request))

(deftest function-handler-symbol-is-late-bound
  (let ((handler (make-instance 'function-handler :function '%test-terminal))
        (original (fdefinition '%test-terminal)))
    (unwind-protect
         (progn
           (ok (equal '(:first 1) (handle handler 1)))
           (setf (fdefinition '%test-terminal) (lambda (request) (list :second request)))
           (ok (equal '(:second 2) (handle handler 2))))
      (setf (fdefinition '%test-terminal) original))))

(deftest function-handler-requires-function
  (ok (signals (make-instance 'function-handler) 'error)))

(deftest function-handler-accessor-is-setf-able
  (let ((handler (make-instance 'function-handler :function (constantly :old))))
    (setf (handler-function handler) (constantly :new))
    (ok (eq :new (handle handler :x)))))

(deftest default-hooks-are-identity
  (let ((handler (make-instance 'function-handler :function #'identity))
        (request (list :request))
        (response (list :response)))
    (ok (eq request (prepare-request handler request)))
    (ok (eq response (finish-response handler request response)))))

(deftest immediate-response-returned-unchanged
  (let* ((response (list 200 '(:content-type "text/plain") '("ok")))
         (handler (make-instance 'function-handler :function (constantly response))))
    (ok (eq response (handle handler :r)))))

(deftest handler-without-primary-signals
  (let ((handler (make-handler '(m-outer))))
    (ok (typep handler 'm-outer) "a class of mixins alone can be made")
    (ok (signals (let ((*trace* nil)) (handle handler :r)) 'error)
        "but handling needs a primary method")))
