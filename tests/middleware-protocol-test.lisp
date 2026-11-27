(defpackage #:middleware-protocol/tests
  (:use #:cl #:rove))

(in-package #:middleware-protocol/tests)

(deftest skeleton-loads
  (ok (find-package '#:middleware-protocol)))
