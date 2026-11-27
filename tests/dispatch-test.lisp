;;;; Dispatch on the request class: inapplicable methods are the pass-through
;;;; (D5, handler.lisp).

(in-package #:middleware-protocol/tests)

;;; A mixin whose every method is specialized on ALPHA-REQUEST.
(defclass alpha-only (handler) ())

(defmethod handle :around ((handler alpha-only) (request alpha-request))
  (note :alpha-around)
  (list :alpha (call-next-method)))

(defmethod handle :before ((handler alpha-only) (request alpha-request))
  (note :alpha-before))

(defmethod prepare-request ((handler alpha-only) (request alpha-request))
  (note :alpha-prepare)
  (setf (request-property request :alpha) t)
  (call-next-method))

(defmethod finish-response ((handler alpha-only) (request alpha-request) response)
  (note :alpha-finish)
  (call-next-method))

(defun %alpha-app ()
  (make-handler '(m-outer alpha-only function-handler)
                :function (lambda (request)
                            (list :base (request-property request :alpha)))))

(deftest mixin-runs-for-the-request-class-it-specializes
  (multiple-value-bind (trace response)
      (traced (handle (%alpha-app) (make-instance 'alpha-request)))
    (ok (equal '(:alpha (:base t)) response))
    (ok (equal '(:alpha-around :alpha-prepare :alpha-before :alpha-finish)
               (remove-if #'consp trace)))))

(deftest mixin-is-a-pass-through-for-other-request-classes
  (let ((request (make-instance 'beta-request)))
    (multiple-value-bind (trace response) (traced (handle (%alpha-app) request))
      (ok (equal '(:base nil) response) "the terminal's response, unchanged")
      (ok (null (remove-if #'consp trace)) "no ALPHA-ONLY method ran")
      (ok (equal '(m-outer) (events-of :enter trace)) "the other layers ran")
      (ok (null (nth-value 1 (request-property request :alpha)))))))

(deftest pass-through-for-requests-that-are-not-request-objects
  (let ((handler (make-handler '(alpha-only function-handler)
                               :function (lambda (request) (list :base request)))))
    (multiple-value-bind (trace response) (traced (handle handler "a string request"))
      (ok (equal '(:base "a string request") response))
      (ok (null trace)))))

(deftest adapter-mixin-is-a-pass-through-for-other-request-classes
  (let* ((response (list 200 (list :content-type "text/plain") (list "ok")))
         (handler (make-handler '(response-headers-handler function-handler)
                                :function (constantly response)
                                :response-headers '(:x-frame-options "DENY"))))
    (ok (eq response (handle handler (make-instance 'alpha-request)))
        "RESPONSE-HEADERS-HANDLER is specialized on CLACK-REQUEST")))
