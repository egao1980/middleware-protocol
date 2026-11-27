;;;; Errors: propagation unchanged, ERROR-HANDLER, HANDLE-ERROR, DECLINE
;;;; (errors.lisp, D9).

(in-package #:middleware-protocol/tests)

(defun %signals (condition)
  "A terminal function that signals CONDITION with ERROR."
  (lambda (request)
    (declare (ignore request))
    (error condition)))

(defmacro caught (form)
  "The condition FORM signals (a SERIOUS-CONDITION), or :NONE."
  `(handler-case (progn ,form :none)
     (serious-condition (c) c)))

(deftest errors-propagate-unchanged-through-layers
  (let* ((condition (make-condition 'test-error :tag :inner))
         (handler (make-handler '(m-outer m-middle timing-handler function-handler)
                                :function (%signals condition)
                                :timing-report (lambda (&rest args) (declare (ignore args))))))
    (let ((*trace* nil))
      (ok (eq condition (caught (handle handler :r))) "the same condition object"))))

(deftest errors-propagate-unchanged-without-a-mapping
  (let* ((condition (make-condition 'test-error))
         (handler (make-handler '(error-handler function-handler) :function (%signals condition))))
    (ok (eq condition (caught (handle handler :r)))
        "an ERROR-HANDLER without ERROR-FUNCTION declines")))

(deftest error-handler-maps-a-condition-to-a-response
  (let* ((condition (make-condition 'test-error :tag :boom))
         (seen nil)
         (handler (make-handler '(error-handler function-handler)
                                :function (%signals condition)
                                :error-function (lambda (request c)
                                                  (setf seen (list request c))
                                                  (list 500 '() (list "failed"))))))
    (ok (equal '(500 () ("failed")) (handle handler :req)))
    (ok (eq :req (first seen)))
    (ok (eq condition (second seen)))))

(deftest-parametrize error-type-selects-conditions
    ((error-type signalled mapped-p)
     :ids ("matching" "not-matching" "supertype" "or-type")
     ('test-error 'test-error t)
     ('test-error 'other-test-error nil)
     ('error 'other-test-error t)
     ('(or test-error other-test-error) 'other-test-error t))
  (let* ((condition (make-condition signalled))
         (handler (make-handler '(error-handler function-handler)
                                :function (%signals condition)
                                :error-type error-type
                                :error-function (constantly :mapped))))
    (if mapped-p
        (ok (eq :mapped (handle handler :r)))
        (ok (eq condition (caught (handle handler :r)))))))

(deftest decline-lets-the-condition-propagate-to-outer-handlers
  (let* ((condition (make-condition 'test-error))
         (inner (make-handler '(error-handler function-handler)
                              :function (%signals condition)
                              :error-function (lambda (request c)
                                                (declare (ignore request))
                                                (ok (find-restart 'decline c))
                                                (decline c))))
         (outer-saw nil)
         (outer (make-handler '(error-handler function-handler)
                              :function (lambda (request) (handle inner request))
                              :error-function (lambda (request c)
                                                (declare (ignore request))
                                                (setf outer-saw c)
                                                :outer-mapped))))
    (ok (eq condition (caught (handle inner :r))) "alone, the inner handler declines")
    (ok (eq :outer-mapped (handle outer :r)))
    (ok (eq condition outer-saw) "the outer handler gets the same condition")))

(deftest decline-without-a-restart-is-a-control-error
  (ok (signals (decline) 'control-error))
  (ok (signals (decline (make-condition 'test-error)) 'control-error)))

(deftest handle-error-runs-before-unwinding
  ;; The terminal offers USE-VALUE; the error function takes it, so the
  ;; terminal itself returns normally and its response goes out.
  (let* ((handler (make-handler '(error-handler m-inner function-handler)
                                :function (lambda (request)
                                            (declare (ignore request))
                                            (restart-case (error 'test-error)
                                              (use-value (value) (list :recovered value))))
                                :error-function (lambda (request c)
                                                  (declare (ignore request))
                                                  (note :handle-error)
                                                  (use-value 42 c)))))
    (multiple-value-bind (trace response) (traced (handle handler :r))
      (ok (equal '(:recovered 42) response))
      (ok (member :handle-error trace))
      (ok (equal '(m-inner) (events-of :finish trace))
          "the request went on through the inner layers' hooks"))))

(deftest inner-cleanup-runs-after-handle-error
  (let* ((handler (make-handler '(error-handler function-handler)
                                :function (lambda (request)
                                            (declare (ignore request))
                                            (unwind-protect (error 'test-error)
                                              (note :cleanup)))
                                :error-function (lambda (request c)
                                                  (declare (ignore request c))
                                                  (note :handle-error)
                                                  :mapped))))
    (multiple-value-bind (trace response) (traced (handle handler :r))
      (ok (eq :mapped response))
      (ok (equal '(:handle-error :cleanup) trace)
          "HANDLER-BIND, not HANDLER-CASE: the stack is unwound only after the decision"))))

(deftest error-handler-gets-the-request-of-its-layer
  (let* ((seen nil)
         (handler (make-handler '(m-tag-around error-handler m-tag-prepare function-handler)
                                :function (%signals (make-condition 'test-error))
                                :error-function (lambda (request c)
                                                  (declare (ignore c))
                                                  (setf seen request)
                                                  :mapped))))
    (ok (eq :mapped (handle handler :r)))
    (ok (equal '(:around :r) seen)
        "rewritten by the outer layer, not yet by the inner one")))

(deftest errors-in-inner-hooks-are-handled
  (let ((handler (make-handler '(error-handler m-wrap-a function-handler)
                               :function (constantly :base)
                               :error-function (lambda (request c)
                                                 (declare (ignore request))
                                                 (list :mapped (test-error-tag c))))))
    (let ((method (defmethod finish-response :before ((handler m-wrap-a) request response)
                    (error 'test-error :tag :in-finish))))
      (unwind-protect
           (ok (equal '(:mapped :in-finish) (handle handler :r)))
        (remove-method #'finish-response method)))))

(deftest errors-from-handle-error-are-not-handled-by-the-same-handler
  (let* ((second (make-condition 'other-test-error))
         (calls 0)
         (handler (make-handler '(error-handler function-handler)
                                :function (%signals (make-condition 'test-error))
                                :error-function (lambda (request c)
                                                  (declare (ignore request c))
                                                  (incf calls)
                                                  (error second)))))
    (ok (eq second (caught (handle handler :r))))
    (ok (= 1 calls))))

(deftest warnings-are-not-touched
  (let* ((warned nil)
         (handler (make-handler '(error-handler function-handler)
                                :function (lambda (request)
                                            (warn "careful")
                                            (list :ok request))
                                :error-function (constantly :mapped))))
    (ok (equal '(:ok :r)
               (handler-bind ((warning (lambda (w)
                                         (setf warned t)
                                         (muffle-warning w))))
                 (handle handler :r))))
    (ok warned "the warning reached the caller's handler")))

;;; HANDLE-ERROR specialized by a mixin of ERROR-HANDLER, per condition class.
(defclass json-errors (error-handler) ())

(defmethod handle-error ((handler json-errors) request (condition test-error))
  (list :json (test-error-tag condition)))

(deftest handle-error-specialization-and-call-next-method
  (let ((handler (make-handler '(json-errors function-handler)
                               :function (lambda (request) (error request))
                               :error-function (lambda (request c)
                                                 (declare (ignore request))
                                                 (list :default (type-of c))))))
    (ok (equal '(:json :tagged)
               (handle handler (make-condition 'test-error :tag :tagged))))
    (ok (equal '(:default other-test-error)
               (handle handler (make-condition 'other-test-error)))
        "other conditions reach the default method")))

(deftest error-handler-accessors
  (let ((handler (make-handler '(error-handler function-handler)
                               :function (%signals (make-condition 'test-error)))))
    (ok (eq 'error (error-handler-type handler)))
    (ok (null (error-handler-function handler)))
    (setf (error-handler-function handler) (constantly :later))
    (ok (eq :later (handle handler :r)))
    (setf (error-handler-type handler) 'other-test-error)
    (ok (typep (caught (handle handler :r)) 'test-error))))
