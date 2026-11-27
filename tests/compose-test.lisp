;;;; Run-time composition: HANDLER-CLASS, MAKE-HANDLER (compose.lisp, D3, D4).

(in-package #:middleware-protocol/tests)

(deftest handler-class-is-cached-per-mixin-list
  (let ((a (handler-class '(m-outer m-inner function-handler)))
        (b (handler-class '(m-outer m-inner function-handler)))
        (c (handler-class (list (find-class 'm-outer) 'm-inner (find-class 'function-handler))))
        (d (handler-class '(m-inner m-outer function-handler))))
    (ok (eq a b) "same list, same class")
    (ok (eq a c) "class objects and symbols designate the same classes")
    (ok (not (eq a d)) "another order is another class")))

(deftest handler-class-direct-superclasses-and-precedence
  (let ((class (handler-class '(m-outer m-inner function-handler))))
    (ok (typep class 'standard-class))
    (ok (closer-mop:class-finalized-p class))
    (ok (equal '(m-outer m-inner function-handler)
               (mapcar #'class-name (closer-mop:class-direct-superclasses class))))
    (ok (equal '(m-outer m-inner function-handler handler)
               (subseq (mapcar #'class-name (closer-mop:class-precedence-list class)) 1 5)))))

(deftest handler-class-is-anonymous
  (let* ((class (handler-class '(m-middle function-handler)))
         (name (class-name class)))
    (ok (or (null name) (null (symbol-package name)))
        "its name is NIL or an uninterned symbol")))

(deftest make-handler-instance-and-initargs
  (let* ((report (lambda (request response seconds)
                   (declare (ignore request response seconds))))
         (handler (make-handler '(timing-handler error-handler function-handler)
                                :function #'identity
                                :timing-report report
                                :error-type 'test-error)))
    (ok (eq (class-of handler) (handler-class '(timing-handler error-handler function-handler))))
    (ok (typep handler 'timing-handler))
    (ok (typep handler 'error-handler))
    (ok (typep handler 'function-handler))
    (ok (typep handler 'handler))
    (ok (eq report (timing-handler-report handler)))
    (ok (eq 'test-error (error-handler-type handler)))
    (ok (eq #'identity (handler-function handler)))
    (ok (eq :x (handle handler :x)))))

(deftest make-handler-rejects-unknown-initargs
  (ok (typep (make-handler '(function-handler) :function #'identity) 'function-handler))
  (ok (signals (make-handler '(function-handler) :function #'identity :no-such-initarg 1)
               'error)))

;;; A mixin used by no other test, so its class is built here first.
(defclass m-fresh (handler) ())

(defmethod handle :around ((handler m-fresh) request)
  (list :fresh (call-next-method)))

(deftest make-handler-adds-no-methods
  (let ((before (mapcar (lambda (gf) (length (closer-mop:generic-function-methods gf)))
                        (list #'handle #'prepare-request #'finish-response #'map-response))))
    (let ((handler (make-handler '(m-fresh m-outer function-handler) :function (constantly :base))))
      (ok (equal '(:fresh :base) (let ((*trace* nil)) (handle handler :r)))))
    (ok (equal before
               (mapcar (lambda (gf) (length (closer-mop:generic-function-methods gf)))
                       (list #'handle #'prepare-request #'finish-response #'map-response))))))

(deftest-parametrize invalid-mixins-signalled
    ((mixins) :ids ("empty" "not-a-list" "improper" "unknown-name" "not-a-designator" "duplicate"
                    "duplicate-class-and-symbol")
     ('())
     ('m-outer)
     ('(m-outer . function-handler))
     ('(m-outer %no-such-class% function-handler))
     ('(m-outer 42 function-handler))
     ('(m-outer m-outer function-handler))
     (:class-and-symbol))
  (let ((mixins (if (eq mixins :class-and-symbol)
                    (list (find-class 'm-outer) 'm-outer 'function-handler)
                    mixins)))
    (ok (signals (handler-class mixins) 'invalid-mixins))
    (ok (signals (make-handler mixins :function #'identity) 'invalid-mixins))
    (let ((condition (handler-case (handler-class mixins)
                       (invalid-mixins (c) c))))
      (ok (eq mixins (invalid-mixins-mixins condition)))
      (ok (stringp (invalid-mixins-reason condition)))
      (ok (plusp (length (invalid-mixins-reason condition)))))))

(deftest inconsistent-precedence-signals-and-caches-nothing
  (ok (typep (handler-class '(m-outer handler)) 'class)
      "a subclass before its superclass is a consistent order")
  (ok (signals (handler-class '(handler m-outer)) 'error))
  (ok (signals (handler-class '(handler m-outer)) 'error) "the second call signals again"))

;;; Another mixin of its own, so the concurrent calls race on a new list.
(defclass m-concurrent (handler) ())

(deftest handler-class-is-thread-safe
  (let* ((mixins '(m-concurrent m-inner function-handler))
         ;; A worker returns its condition instead of dying with it: an
         ;; unhandled error in a thread would end the whole test process.
         (threads (loop repeat 8
                        collect (bt2:make-thread (lambda ()
                                                   (handler-case (handler-class mixins)
                                                     (error (c) c)))
                                                 :name "handler-class race")))
         (classes (mapcar #'bt2:join-thread threads)))
    (ok (every (lambda (class) (typep class 'class)) classes))
    (ok (every (lambda (class) (eq class (first classes))) classes))
    (ok (eq (first classes) (handler-class mixins)))))

(defclass m-redefined (handler) ())

(deftest methods-added-to-a-mixin-reach-existing-handlers
  (let ((handler (make-handler '(m-redefined function-handler) :function (constantly :base))))
    (ok (eq :base (handle handler :r)))
    (let ((method (defmethod handle :around ((handler m-redefined) request)
                    (list :redefined (call-next-method)))))
      (unwind-protect
           (ok (equal '(:redefined :base) (handle handler :r))
               "a method added to the mixin later applies to existing handlers")
        (remove-method #'handle method)))
    (ok (eq :base (handle handler :r)))))

(defclass m-reslotted (handler) ())

(deftest redefining-a-mixin-keeps-the-cached-class
  (let* ((class (handler-class '(m-reslotted function-handler)))
         (handler (make-instance class :function (constantly :base))))
    (eval '(defclass m-reslotted (handler)
            ((extra :initform :added))))
    (unwind-protect
         (progn
           (ok (eq class (handler-class '(m-reslotted function-handler)))
               "the cached class is still the one for this list")
           (ok (eq :added (slot-value handler 'extra))
               "the redefinition reached the cached class and its instances")
           (ok (eq :base (handle handler :r))))
      (eval '(defclass m-reslotted (handler) ())))))

(deftest combined-mixins-keep-their-own-slots
  ;; D4: slot names are symbols of the mixin's package, never shared, so the
  ;; slots of two mixins in one class do not merge.
  (let* ((terminal (lambda (request) (list :terminal request)))
         (on-error (lambda (request condition) (declare (ignore request condition)) :mapped))
         (report (lambda (request response seconds) (declare (ignore request response seconds))))
         (clock (constantly 0))
         (handler (make-handler '(timing-handler error-handler function-handler)
                                :function terminal
                                :error-function on-error
                                :error-type 'test-error
                                :timing-report report
                                :timing-clock clock)))
    (ok (eq terminal (handler-function handler)))
    (ok (eq on-error (error-handler-function handler)))
    (ok (eq 'test-error (error-handler-type handler)))
    (ok (eq report (timing-handler-report handler)))
    (ok (eq clock (timing-handler-clock handler)))
    (ok (equal '(:terminal :r) (handle handler :r)))
    (ok (= (length (closer-mop:class-slots (class-of handler)))
           (+ (length (closer-mop:class-slots (find-class 'timing-handler)))
              (length (closer-mop:class-slots (find-class 'error-handler)))
              (length (closer-mop:class-slots (find-class 'function-handler)))))
        "no two mixins share a slot")))
