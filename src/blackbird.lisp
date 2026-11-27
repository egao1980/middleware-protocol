;;;; middleware-protocol/blackbird: MAP-RESPONSE on blackbird promises.
;;;;
;;;; For handlers that answer asynchronously with a BLACKBIRD:PROMISE. This
;;;; system adds one method and no package: the core stays free of blackbird.
;;;;
;;;; MAP-RESPONSE on BLACKBIRD:PROMISE
;;;;   Returns a NEW promise; RESPONSE is not changed. When RESPONSE resolves,
;;;;   the new promise resolves with (map-response value function), VALUE being
;;;;   the first value RESPONSE resolved with. So a promise of an immediate
;;;;   response resolves with FUNCTION's value, and a promise of another deferred
;;;;   response (a Clack streaming function, a promise) resolves with that
;;;;   response mapped in turn. FUNCTION is not called before RESPONSE resolves.
;;;;   When RESPONSE is rejected, FUNCTION is not called and the new promise is
;;;;   rejected with the same condition. A condition FUNCTION signals rejects
;;;;   the new promise (blackbird's callback rule).

(in-package #:middleware-protocol)

(defmethod map-response ((response blackbird:promise) transform)
  "A new promise of RESPONSE's first value mapped in turn (file header). ATTACH
forwards a rejection without calling the callback, and rejects the new promise
when the callback signals."
  (blackbird:attach response
                    (lambda (&rest values)
                      (map-response (first values) transform))))
