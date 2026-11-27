;;;; middleware-protocol: generic handler middleware for cl-stack.
;;;;
;;;; One generic function, HANDLE handler request → response, with standard
;;;; method combination. A middleware is a MIXIN CLASS whose methods on HANDLE
;;;; are :AROUND (control: short-circuit, rewrite the request, map errors,
;;;; transform the response) or :BEFORE / :AFTER (observation only). The class
;;;; precedence list orders the layers: the leftmost mixin's :AROUND runs
;;;; outermost. There is no chain object, no NEXT argument and no runtime
;;;; ADD-METHOD; MAKE-HANDLER composes mixins chosen at run time into an
;;;; anonymous class. Design and locked decisions D1 to D11:
;;;; cl-stack/docs/capabilities/middleware.md.
;;;;
;;;; Files (each header is the specification of what it defines):
;;;;   conditions.lisp  MIDDLEWARE-ERROR, INVALID-MIXINS
;;;;   response.lisp    MAP-RESPONSE (immediate responses; adapters add deferred shapes)
;;;;   handler.lisp     HANDLE, HANDLER, the hooks PREPARE-REQUEST / FINISH-RESPONSE,
;;;;                    FUNCTION-HANDLER
;;;;   compose.lisp     HANDLER-CLASS, MAKE-HANDLER
;;;;   request.lisp     REQUEST and its property bag
;;;;   timing.lisp      TIMING-HANDLER
;;;;   errors.lisp      ERROR-HANDLER, HANDLE-ERROR, DECLINE
;;;;   clack.lisp       system middleware-protocol/clack, package MIDDLEWARE-PROTOCOL/CLACK
;;;;   blackbird.lisp   system middleware-protocol/blackbird (MAP-RESPONSE on promises)

(defpackage #:middleware-protocol
  (:use #:cl)
  (:nicknames #:stack-middleware)
  (:documentation "Generic handler middleware for cl-stack: one HANDLE generic function,
middleware as mixin classes with :around / :before / :after methods ordered by
the class precedence list, MAKE-HANDLER for run-time composition, request
property bags, MAP-RESPONSE for immediate and deferred responses, and the
standard TIMING-HANDLER and ERROR-HANDLER mixins.")
  (:export
   ;; handlers (handler.lisp)
   #:handle
   #:handler
   #:prepare-request
   #:finish-response
   #:function-handler
   #:handler-function
   ;; run-time composition (compose.lisp)
   #:handler-class
   #:make-handler
   ;; requests (request.lisp)
   #:request
   #:request-property
   #:remove-request-property
   #:request-properties
   ;; responses (response.lisp)
   #:map-response
   ;; standard mixins (timing.lisp, errors.lisp)
   #:timing-handler
   #:timing-handler-report
   #:timing-handler-clock
   #:error-handler
   #:error-handler-type
   #:error-handler-function
   #:handle-error
   #:decline
   ;; conditions (conditions.lisp)
   #:middleware-error
   #:invalid-mixins
   #:invalid-mixins-mixins
   #:invalid-mixins-reason))
