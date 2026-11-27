(defpackage #:middleware-protocol
  (:use #:cl)
  (:documentation "Generic handler middleware for cl-stack: one HANDLE generic function, middleware as mixin classes with :around methods, deferred responses, a Clack adapter. Skeleton: the API lands through contract branches; see the capability brief."))
