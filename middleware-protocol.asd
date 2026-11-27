(defsystem "middleware-protocol"
  :version "0.0.1"
  :description "Generic handler middleware for any request/response type: handlers, middleware mixins, deferred responses, Clack adapter"
  :author "egao1980"
  :license "MIT"
  :pathname "src"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "middleware-protocol/tests"))))

(defsystem "middleware-protocol/tests"
  :depends-on ("middleware-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "middleware-protocol-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
