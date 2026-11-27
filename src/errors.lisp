;;;; Errors: ERROR-HANDLER, HANDLE-ERROR, DECLINE (D9).
;;;;
;;;; By default a condition signalled by an inner layer propagates unchanged:
;;;; outer layers and the caller of HANDLE see the same object, and no layer of
;;;; the protocol handles, wraps or logs it. Mapping conditions to responses is
;;;; the job of one mixin.
;;;;
;;;; ERROR-HANDLER (a mixin, subclass of HANDLER)
;;;;
;;;;   Initargs and accessors:
;;;;   * :ERROR-TYPE, ERROR-HANDLER-TYPE: a type specifier, default ERROR. Only
;;;;     conditions of this type are considered.
;;;;   * :ERROR-FUNCTION, ERROR-HANDLER-FUNCTION: NIL (the default) or a function
;;;;     of two arguments, REQUEST CONDITION, returning the response.
;;;;
;;;;   HANDLE :AROUND on (ERROR-HANDLER T) calls CALL-NEXT-METHOD inside
;;;;   HANDLER-BIND, not HANDLER-CASE: when an inner layer signals a condition of
;;;;   ERROR-TYPE during this method's dynamic extent (inner :AROUND methods, the
;;;;   hooks, the primary, FINISH-RESPONSE of an immediate response):
;;;;   1. (handle-error handler request condition) is called BEFORE unwinding,
;;;;      inside the handler: the signalling frames, their restarts (USE-VALUE,
;;;;      CONTINUE, domain restarts) and the backtrace are all still there. A
;;;;      method may invoke one of those restarts; HANDLE then goes on as that
;;;;      restart decides. REQUEST is the request as this layer received it.
;;;;   2. Around that call the restart DECLINE is established (report: let the
;;;;      condition propagate). Invoking it (the function DECLINE) makes this
;;;;      handler decline: the same condition goes on to outer handlers.
;;;;   3. When HANDLE-ERROR returns, its primary value is this layer's response:
;;;;      the stack unwinds to this method (cleanup forms of inner layers run)
;;;;      and it returns that value. Outer layers see an ordinary response.
;;;;   Conditions not of ERROR-TYPE are not touched (warnings and SIGNALs stay
;;;;   with their own handlers). A condition HANDLE-ERROR itself signals is not
;;;;   handled by the same ERROR-HANDLER. A condition signalled while a deferred
;;;;   response is produced (a Clack streaming body, a promise callback) is
;;;;   outside the dynamic extent and is not handled (a later slice).
;;;;
;;;; HANDLE-ERROR handler request condition → response
;;;;
;;;;   The method on (ERROR-HANDLER T T) calls ERROR-FUNCTION with REQUEST and
;;;;   CONDITION; when ERROR-FUNCTION is NIL it declines (calls DECLINE). Mixins
;;;;   specialize it on their own class and/or on condition classes and reach
;;;;   the default with CALL-NEXT-METHOD.
;;;;
;;;; DECLINE &optional condition
;;;;
;;;;   Invokes the most recent DECLINE restart (associated with CONDITION, as
;;;;   FIND-RESTART selects); with none active, signals CONTROL-ERROR, as
;;;;   CLHS MUFFLE-WARNING does. DECLINE is also the restart's name.

(in-package #:middleware-protocol)

(defclass error-handler (handler)
  ((%error-type :initarg :error-type :initform 'error :accessor error-handler-type)
   (%error-function :initarg :error-function :initform nil :accessor error-handler-function))
  (:documentation "Mixin turning conditions of inner layers into responses (file header)."))

(defgeneric handle-error (handler request condition)
  (:documentation "The response for CONDITION, signalled while HANDLER handled REQUEST (file
header). Called before unwinding; may call DECLINE."))

(defun decline (&optional condition)
  "Invoke the DECLINE restart: the ERROR-HANDLER lets CONDITION propagate (file header)."
  (let ((restart (find-restart 'decline condition)))
    (if restart
        (invoke-restart restart)
        (error 'control-error))))

(defmethod handle-error ((handler error-handler) request condition)
  "ERROR-FUNCTION's response for CONDITION, or decline when there is none (file header)."
  (let ((function (error-handler-function handler)))
    (if function
        (funcall function request condition)
        (decline condition))))

(defmethod handle :around ((handler error-handler) request)
  "Map conditions of ERROR-TYPE from the inner layers to a response, deciding before
unwinding (file header)."
  (let ((type (error-handler-type handler)))
    (block handled
      (handler-bind
          ((condition
             (lambda (condition)
               ;; HANDLER-BIND does not evaluate its type, and TYPE is a slot
               ;; value (maybe compound): filter when the condition is signalled.
               (when (typep condition type)
                 (restart-case
                     ;; The only unwind: inner cleanup forms run after the decision.
                     (return-from handled
                       (values (handle-error handler request condition)))
                   (decline ()
                     :report "Decline: let the condition propagate to outer handlers."
                     ;; Associated with CONDITION: DECLINE of another condition
                     ;; does not see this restart.
                     :test (lambda (other) (or (null other) (eq other condition)))
                     ;; The handler returns normally: the signal goes on outward.
                     nil))))))
        (call-next-method)))))
