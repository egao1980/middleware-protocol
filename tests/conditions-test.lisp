;;;; The condition tree (conditions.lisp).

(in-package #:middleware-protocol/tests)

(deftest condition-tree
  (ok (subtypep 'middleware-error 'error))
  (ok (subtypep 'invalid-mixins 'middleware-error)))

(deftest invalid-mixins-slots-and-report
  (let ((condition (make-condition 'invalid-mixins
                                   :mixins '(a a)
                                   :reason "A appears twice")))
    (ok (equal '(a a) (invalid-mixins-mixins condition)))
    (ok (equal "A appears twice" (invalid-mixins-reason condition)))
    (let ((report (princ-to-string condition)))
      (ok (search "A appears twice" report))
      (ok (search "(A A)" report)))))
