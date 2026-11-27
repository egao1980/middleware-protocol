;;;; Test support: a trace of layer events, tracing mixins, request classes,
;;;; a fake Clack responder and promise helpers.

(in-package #:middleware-protocol/tests)

;;; Trace

(defvar *trace* nil
  "Events noted by the layers under test, most recent first.")

(defun note (event)
  (push event *trace*)
  event)

(defun call-traced (thunk)
  "→ (values trace result): the events THUNK noted, in order, and its value."
  (let ((*trace* '()))
    (let ((result (funcall thunk)))
      (values (reverse *trace*) result))))

(defmacro traced (&body body)
  `(call-traced (lambda () ,@body)))

;;; Tracing mixins: every method of the protocol notes (EVENT NAME).

(defmacro define-tracing-mixin (name &optional (superclasses '(handler)))
  `(progn
     (defclass ,name ,superclasses ())
     (defmethod handle :around ((handler ,name) request)
       (note (list :enter ',name))
       (multiple-value-prog1 (call-next-method)
         (note (list :exit ',name))))
     (defmethod handle :before ((handler ,name) request)
       (note (list :before ',name)))
     (defmethod handle :after ((handler ,name) request)
       (note (list :after ',name)))
     (defmethod prepare-request ((handler ,name) request)
       (note (list :prepare ',name))
       (call-next-method))
     (defmethod finish-response ((handler ,name) request response)
       (let ((response (call-next-method)))
         (note (list :finish ',name))
         response))))

(define-tracing-mixin m-outer)
(define-tracing-mixin m-middle)
(define-tracing-mixin m-inner)

(defparameter +tracing-mixins+ '(m-outer m-middle m-inner))

(defun tracing-names-in-precedence-order (handler)
  "The tracing mixins of HANDLER's class, in its class precedence list order."
  (remove-if-not (lambda (name) (member name +tracing-mixins+))
                 (mapcar #'class-name
                         (closer-mop:class-precedence-list (class-of handler)))))

(defun events-of (kind trace)
  "The names of the (KIND name) events of TRACE, in order."
  (loop for event in trace
        when (and (consp event) (eq (first event) kind))
          collect (second event)))

(defun primary (response)
  "A terminal function that notes :PRIMARY and answers RESPONSE."
  (lambda (request)
    (declare (ignore request))
    (note :primary)
    response))

;;; Request classes for dispatch

(defclass alpha-request (request) ())
(defclass beta-request (request) ())

;;; A fake Clack server side

(defun run-streaming (response)
  "Call the Clack streaming RESPONSE like a server would.
→ (values head chunks closed writer): the list given to the responder, the body
chunks written through the writer, whether the writer was closed, and the writer
the responder returned (NIL when the head carried a body)."
  (let ((head nil) (chunks '()) (closed nil) (writer nil))
    (funcall response
             (lambda (list)
               (setf head list)
               (if (cddr list)
                   nil
                   (setf writer
                         (lambda (data &key start end close)
                           (declare (ignore start end))
                           (push data chunks)
                           (when close (setf closed t))
                           nil)))))
    (values head (reverse chunks) closed writer)))

(defun streaming-app-response (head &rest chunks)
  "A Clack streaming response that sends HEAD (status headers) then CHUNKS."
  (lambda (responder)
    (let ((writer (funcall responder head)))
      (loop for (chunk . more) on chunks
            do (funcall writer chunk :close (null more))))))

;;; Promises (blackbird runs callbacks when a promise settles, in that thread)

(defun pending-promise ()
  "→ (values promise resolve reject): an unsettled promise and its two functions."
  (let (resolve reject)
    (let ((promise (bb:with-promise (res rej :resolve-fn resolve-fn :reject-fn reject-fn)
                     (setf resolve resolve-fn
                           reject reject-fn))))
      (values promise resolve reject))))

(defun settle (promise)
  "→ (values state value): :PENDING NIL, :RESOLVED and the first value, or
:REJECTED and the condition."
  (let ((state :pending) (value nil))
    (bb:catcher (bb:attach promise
                           (lambda (&rest values)
                             (setf state :resolved
                                   value (first values))))
                (condition (c)
                  (setf state :rejected
                        value c)))
    (values state value)))

(defun resolved-promise (value)
  (multiple-value-bind (promise resolve) (pending-promise)
    (funcall resolve value)
    promise))

;;; Conditions used by the tests

(define-condition test-error (error)
  ((tag :initarg :tag :initform nil :reader test-error-tag)))

(define-condition other-test-error (error) ())
