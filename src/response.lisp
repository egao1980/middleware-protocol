;;;; Responses: MAP-RESPONSE (D7).
;;;;
;;;; MAP-RESPONSE response function → response
;;;;
;;;;   How a layer transforms a response on the way out so that the same code
;;;;   works for immediate and for deferred (streamed, asynchronous) responses.
;;;;   FUNCTION takes one IMMEDIATE response and returns the response to use
;;;;   instead (its primary value).
;;;;
;;;;   * Immediate responses: the method on T calls FUNCTION once, now, and
;;;;     returns its value.
;;;;   * Deferred responses: a method for the deferred shape returns a NEW
;;;;     deferred response of the same shape. When that one produces its
;;;;     immediate response, it passes it through FUNCTION first (once) and
;;;;     produces FUNCTION's value instead. FUNCTION is never called before the
;;;;     response is produced, and MAP-RESPONSE itself returns without calling it.
;;;;     The core defines no deferred shape. The adapters define:
;;;;       - middleware-protocol/clack: a FUNCTION response is a Clack streaming
;;;;         response, (lambda (responder) ...) (clack.lisp);
;;;;       - middleware-protocol/blackbird: a BLACKBIRD:PROMISE (blackbird.lisp).
;;;;     A new deferred shape is one method on MAP-RESPONSE for its class.
;;;;
;;;;   Rules for layers:
;;;;   * An :AROUND method that transforms the response on the way out returns
;;;;     (map-response (call-next-method) fn), never a transformation of the raw
;;;;     value. FINISH-RESPONSE (handler.lisp) is always reached this way.
;;;;   * A transformation of a deferred response runs outside the dynamic extent
;;;;     of HANDLE (later, maybe on another thread): special bindings made by a
;;;;     layer are gone. Close over what FUNCTION needs, or keep it on the
;;;;     request (request.lisp). Conditions FUNCTION signals then are not seen by
;;;;     ERROR-HANDLER (errors.lisp); for a promise they reject the new promise.

(in-package #:middleware-protocol)

(defgeneric map-response (response function)
  (:documentation "RESPONSE transformed by FUNCTION: now for an immediate response, when it is
produced for a deferred one (file header)."))

(defmethod map-response (response function)
  "An immediate response: FUNCTION's primary value, computed now (file header)."
  (values (funcall function response)))
