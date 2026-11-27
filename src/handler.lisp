;;;; Handlers: HANDLE, HANDLER, the hooks, FUNCTION-HANDLER (D1, D2, D8).
;;;;
;;;; HANDLE handler request → response
;;;;
;;;;   The one generic function of the protocol, standard method combination.
;;;;   REQUEST and RESPONSE may be of any type: adapters name the concrete
;;;;   classes (middleware-protocol/clack: CLACK-REQUEST and Clack responses).
;;;;
;;;;   * A FUNCTION is a handler: (handle fn request) is (funcall fn request). No
;;;;     hooks run for it; it cannot carry mixins (wrap it in FUNCTION-HANDLER).
;;;;   * Any object whose class has a primary method on HANDLE is a handler.
;;;;
;;;; HANDLER
;;;;
;;;;   The protocol class of handlers that take part in method combination:
;;;;   every middleware mixin and every terminal (FUNCTION-HANDLER, CLACK-HANDLER)
;;;;   inherits it. No slots. A MIDDLEWARE is a mixin class (a subclass of
;;;;   HANDLER) with :AROUND methods on HANDLE (control: short-circuit, rewrite
;;;;   the request, map errors, transform the response) or :BEFORE / :AFTER
;;;;   methods (observation only), and/or methods on the two hooks. A TERMINAL is
;;;;   a class with a primary method on HANDLE. A handler is an instance of a
;;;;   class that lists mixins before a terminal:
;;;;     (defclass app (error-handler timing-handler function-handler) ())
;;;;   or the same built at run time with MAKE-HANDLER (compose.lisp).
;;;;
;;;;   Configuration lives in slots with initargs (D4). Slot names are symbols of
;;;;   the mixin's own package (never shared names, which would merge slots of
;;;;   two mixins); initargs are keywords named after the mixin
;;;;   (:TIMING-REPORT), so one MAKE-HANDLER call configures every mixin of the
;;;;   class without clashes. A class has one instance of each mixin: a
;;;;   middleware that needs several configurations takes a list in one slot.
;;;;
;;;; The base method: HANDLE :AROUND on (HANDLER T)
;;;;
;;;;   HANDLER comes after every mixin in a handler's class precedence list, so
;;;;   this is the INNERMOST :AROUND method. It
;;;;     1. calls (prepare-request handler request) → REQUEST*;
;;;;     2. calls (call-next-method handler request*): the :BEFORE methods, the
;;;;        primary method and the :AFTER methods all see REQUEST*;
;;;;     3. returns (map-response response
;;;;                  (lambda (r) (finish-response handler request* r))),
;;;;        so FINISH-RESPONSE runs now for an immediate response and when a
;;;;        deferred one is produced (response.lisp).
;;;;   A handler whose class has no applicable primary method signals an ERROR
;;;;   when handled (CL's NO-PRIMARY-METHOD / NO-APPLICABLE-METHOD).
;;;;
;;;; Order (locked; tests assert it from the class precedence list)
;;;;
;;;;   For (defclass h (m1 m2 terminal) ()), with :AROUND methods and hooks on M1
;;;;   and M2 and the hooks written as below:
;;;;     m1 :around enter → m2 :around enter → prepare-request m1 → m2 →
;;;;     :before m1 → m2 → primary → :after m2 → m1 →
;;;;     finish-response m2 → m1 → m2 :around exit → m1 :around exit.
;;;;   The leftmost mixin is the outermost layer (D2). A mixin that must sit
;;;;   outside another says so by inheriting from it: then it precedes it in
;;;;   every class precedence list.
;;;;
;;;;   * Short-circuit: an :AROUND method that returns without calling
;;;;     CALL-NEXT-METHOD answers the request. Inner :AROUND methods, the base
;;;;     method and the primary method do not run, so NO mixin's hooks run, the
;;;;     outer mixins' included: hooks belong to requests that reach the
;;;;     primary. Outer :AROUND methods see its value as the response, exactly
;;;;     like an inner response, so a layer that must see every response (timing,
;;;;     security headers) is an :AROUND that maps the response.
;;;;   * Rewriting: (call-next-method handler new-request). CLOS requires the new
;;;;     arguments to select the same applicable methods as the original ones, so
;;;;     NEW-REQUEST (and the value of PREPARE-REQUEST) is normally of the same
;;;;     class as REQUEST.
;;;;   * Dispatch on the request: a method specialized on a request class does
;;;;     not apply to requests of other classes, so for them that layer is a
;;;;     pass-through (D5). There is no rule for unknown request types.
;;;;
;;;; PREPARE-REQUEST handler request → request
;;;;
;;;;   Called by the base method before the primary. The method on (HANDLER T)
;;;;   returns REQUEST. Mixins specialize it on their class and chain with
;;;;   CALL-NEXT-METHOD; the leftmost mixin's method is the most specific and
;;;;   runs first. Written as "do the work, then (call-next-method handler
;;;;   request*)" (or "set request properties, then (call-next-method)"), outer
;;;;   mixins prepare first.
;;;;
;;;; FINISH-RESPONSE handler request response → response
;;;;
;;;;   Called through MAP-RESPONSE after the primary, so RESPONSE is always an
;;;;   immediate response (for a Clack streaming response: the head list given
;;;;   to the responder). REQUEST is PREPARE-REQUEST's value. The method on
;;;;   (HANDLER T T) returns RESPONSE. Written as
;;;;     (let ((response (call-next-method))) (transform response)),
;;;;   inner mixins finish first and the leftmost last: the onion.
;;;;
;;;; FUNCTION-HANDLER
;;;;
;;;;   A terminal (subclass of HANDLER) around a function of one argument, the
;;;;   request. Initarg :FUNCTION (a function designator), required: MAKE-INSTANCE
;;;;   without it signals an ERROR. Accessor HANDLER-FUNCTION. Its primary method
;;;;   on HANDLE returns (funcall (handler-function handler) request); a symbol is
;;;;   called through its current definition, so redefining the function changes
;;;;   the next request's answer.
;;;;
;;;; Cost: dispatch is CLOS's cached effective method, with no chain object and
;;;; no closure per layer. Per HANDLE call on a HANDLER the base method makes one
;;;; closure (FINISH-RESPONSE's continuation for MAP-RESPONSE); a mixin's
;;;; :AROUND that maps the response makes its own.

(in-package #:middleware-protocol)

(defclass handler ()
  ()
  (:documentation "Protocol class of handlers that take part in method combination: middleware
mixins and terminals (file header)."))

(defgeneric handle (handler request)
  (:documentation "HANDLER's response to REQUEST (file header)."))

(defgeneric prepare-request (handler request)
  (:documentation "Hook called by HANDLE before the primary method; returns the request the
inner methods see (file header)."))

(defgeneric finish-response (handler request response)
  (:documentation "Hook called through MAP-RESPONSE after the primary method; returns the
response (file header)."))

(defmethod handle ((handler function) request)
  "A function is a handler: its value for REQUEST, with no hooks (file header)."
  (funcall handler request))

(defmethod prepare-request ((handler handler) request)
  "The default hook: REQUEST as it is."
  request)

(defmethod finish-response ((handler handler) request response)
  "The default hook: RESPONSE as it is."
  (declare (ignore request))
  response)

(defmethod handle :around ((handler handler) request)
  "The base method (file header)."
  (let ((request* (prepare-request handler request)))
    (map-response (call-next-method handler request*)
                  (lambda (response)
                    (finish-response handler request* response)))))

(defclass function-handler (handler)
  ((%handler-function :initarg :function
                      :initform (error "FUNCTION-HANDLER needs :FUNCTION")
                      :accessor handler-function))
  (:documentation "Terminal handler calling a function of the request (file header)."))

(defmethod handle ((handler function-handler) request)
  "The terminal: HANDLER-FUNCTION called with REQUEST. A symbol is called through
its current definition (file header)."
  (funcall (handler-function handler) request))
