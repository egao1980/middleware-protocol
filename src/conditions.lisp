;;;; Conditions (CLHS 9). Given in the contract: not stubbed.
;;;;
;;;; MIDDLEWARE-ERROR (ERROR) is the root of the protocol's own conditions.
;;;; Conditions signalled by handlers and middleware are NOT wrapped in it:
;;;; an inner condition reaches outer layers and the caller of HANDLE as the
;;;; same object (errors.lisp).
;;;;
;;;; INVALID-MIXINS (MIDDLEWARE-ERROR): HANDLER-CLASS / MAKE-HANDLER were given a
;;;; mixin list they cannot build a class from (compose.lisp). Slots: MIXINS (the
;;;; list as given), REASON (a string saying which rule it breaks). The report
;;;; names both.
;;;;
;;;; The restart DECLINE belongs to ERROR-HANDLER (errors.lisp).

(in-package #:middleware-protocol)

(define-condition middleware-error (error)
  ()
  (:documentation "Root of the conditions middleware-protocol itself signals."))

(define-condition invalid-mixins (middleware-error)
  ((mixins :initarg :mixins :initform nil :reader invalid-mixins-mixins)
   (reason :initarg :reason :initform "" :reader invalid-mixins-reason))
  (:report (lambda (condition stream)
             (format stream "Cannot build a handler class from the mixins ~S: ~A"
                     (invalid-mixins-mixins condition)
                     (invalid-mixins-reason condition))))
  (:documentation "HANDLER-CLASS or MAKE-HANDLER got a mixin list they cannot build a class from."))
