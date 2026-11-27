;;;; Layer order: the class precedence list (D2), the base method and the
;;;; hooks (D8), short-circuit, rewriting (handler.lisp).

(in-package #:middleware-protocol/tests)

(defclass traced-app (m-outer m-middle m-inner function-handler) ())

(defparameter +full-trace+
  '((:enter m-outer) (:enter m-middle) (:enter m-inner)
    (:prepare m-outer) (:prepare m-middle) (:prepare m-inner)
    (:before m-outer) (:before m-middle) (:before m-inner)
    :primary
    (:after m-inner) (:after m-middle) (:after m-outer)
    (:finish m-inner) (:finish m-middle) (:finish m-outer)
    (:exit m-inner) (:exit m-middle) (:exit m-outer)))

(deftest onion-order-of-a-defclass-handler
  (let ((handler (make-instance 'traced-app :function (primary :done))))
    (multiple-value-bind (trace response) (traced (handle handler :r))
      (ok (eq :done response))
      (ok (equal +full-trace+ trace)))))

(deftest onion-order-follows-the-class-precedence-list
  (let ((handler (make-instance 'traced-app :function (primary :done))))
    (let ((order (tracing-names-in-precedence-order handler))
          (trace (traced (handle handler :r))))
      (ok (equal '(m-outer m-middle m-inner) order))
      (ok (equal order (events-of :enter trace)))
      (ok (equal order (events-of :prepare trace)))
      (ok (equal order (events-of :before trace)))
      (ok (equal (reverse order) (events-of :after trace)))
      (ok (equal (reverse order) (events-of :finish trace)))
      (ok (equal (reverse order) (events-of :exit trace))))))

(deftest-parametrize make-handler-order-is-the-mixin-order
    ((mixins) :ids ("as-defclass" "reversed" "rotated")
     ('(m-outer m-middle m-inner))
     ('(m-inner m-middle m-outer))
     ('(m-middle m-inner m-outer)))
  (let ((handler (make-handler (append mixins '(function-handler))
                               :function (primary :done))))
    (let ((trace (traced (handle handler :r))))
      (ok (equal mixins (tracing-names-in-precedence-order handler)))
      (ok (equal mixins (events-of :enter trace)))
      (ok (equal mixins (events-of :prepare trace)))
      (ok (equal (reverse mixins) (events-of :finish trace)))
      (ok (equal (reverse mixins) (events-of :exit trace))))))

;;; Short-circuit: M-GATE answers :DENIED for :BLOCKED without calling the
;;; next method.
(defclass m-gate (handler) ())

(defmethod handle :around ((handler m-gate) request)
  (note :gate)
  (if (eq request :blocked)
      :denied
      (call-next-method)))

(deftest short-circuit-skips-inner-layers-and-all-hooks
  (let ((handler (make-handler '(m-outer m-gate m-inner function-handler)
                               :function (primary :done))))
    (multiple-value-bind (trace response) (traced (handle handler :blocked))
      (ok (eq :denied response))
      (ok (equal '((:enter m-outer) :gate (:exit m-outer)) trace)
          "outer :around sees the answer; no hook, :before, :after or primary runs"))
    (multiple-value-bind (trace response) (traced (handle handler :open))
      (ok (eq :done response))
      (ok (member :primary trace))
      (ok (equal '(m-inner m-outer) (events-of :finish trace))
          "when the request gets through, every mixin's hooks run"))))

;;; Rewriting the request: by an :AROUND (call-next-method with arguments) and
;;; by PREPARE-REQUEST.
(defclass m-tag-around (handler) ())

(defmethod handle :around ((handler m-tag-around) request)
  (call-next-method handler (list :around request)))

(defclass m-tag-prepare (handler) ())

(defmethod prepare-request ((handler m-tag-prepare) request)
  (call-next-method handler (list :prepared request)))

(defclass m-tag-prepare-2 (handler) ())

(defmethod prepare-request ((handler m-tag-prepare-2) request)
  (call-next-method handler (list :prepared-2 request)))

(defclass m-observe-before (handler) ())

(defmethod handle :before ((handler m-observe-before) request)
  (note (list :before-saw request)))

(deftest around-rewrites-the-request-for-inner-layers
  (let ((handler (make-handler '(m-tag-around function-handler)
                               :function (lambda (request) (list :got request)))))
    (ok (equal '(:got (:around :r)) (handle handler :r)))))

(deftest prepare-request-rewrites-in-precedence-order
  (let ((handler (make-handler '(m-tag-prepare m-tag-prepare-2 m-observe-before function-handler)
                               :function (lambda (request) (list :got request)))))
    (multiple-value-bind (trace response) (traced (handle handler :r))
      (ok (equal '(:got (:prepared-2 (:prepared :r))) response)
          "the leftmost mixin prepares first; the primary sees the result")
      (ok (equal '((:before-saw (:prepared-2 (:prepared :r)))) trace)
          ":before methods see the prepared request"))))

;;; FINISH-RESPONSE sees the prepared request and transforms in onion order.
(defclass m-wrap-a (handler) ())
(defclass m-wrap-b (handler) ())

(defmethod finish-response ((handler m-wrap-a) request response)
  (list :a (call-next-method)))

(defmethod finish-response ((handler m-wrap-b) request response)
  (list :b request (call-next-method)))

(deftest finish-response-onion
  (let ((handler (make-handler '(m-wrap-a m-wrap-b m-tag-prepare function-handler)
                               :function (constantly :base))))
    (ok (equal '(:a (:b (:prepared :r) :base)) (handle handler :r))
        "inner finishes first; REQUEST is the prepared request")))

;;; :BEFORE / :AFTER observe without changing the response.
(defclass m-observer (handler) ())

(defmethod handle :after ((handler m-observer) request)
  (note (list :after-saw request))
  :ignored-value)

(deftest after-methods-do-not-change-the-response
  (let ((handler (make-handler '(m-observer function-handler) :function (constantly :base))))
    (multiple-value-bind (trace response) (traced (handle handler :r))
      (ok (eq :base response))
      (ok (equal '((:after-saw :r)) trace)))))

;;; A mixin that must sit outside another says so by inheritance.
(define-tracing-mixin m-requires-inner (m-inner))

(deftest inheritance-places-a-mixin-outside-the-one-it-requires
  (let ((handler (make-handler '(m-requires-inner function-handler) :function (primary :done))))
    (let ((trace (traced (handle handler :r))))
      (ok (equal '(m-requires-inner m-inner) (events-of :enter trace)))
      (ok (equal '(m-inner m-requires-inner) (events-of :exit trace)))))
  (ok (signals (make-handler '(m-inner m-requires-inner function-handler)
                             :function (primary :done))
               'error)
      "listing the required mixin outside it is an inconsistent precedence order"))
