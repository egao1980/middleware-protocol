(defsystem "middleware-protocol"
  :version "0.1.0"
  :description "Generic handler middleware for any request/response type: one HANDLE generic function, middleware as mixin classes, deferred responses, a Clack adapter"
  :author "egao1980"
  :license "MIT"
  :depends-on ("closer-mop" "bordeaux-threads")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "response")
               (:file "handler")
               (:file "compose")
               (:file "request")
               (:file "timing")
               (:file "errors"))
  :in-order-to ((test-op (test-op "middleware-protocol/tests"))))

(defsystem "middleware-protocol/clack"
  :version "0.1.0"
  :description "middleware-protocol adapter for Clack apps: clack-request over the Clack env, clack-handler, clack-app, Clack streaming responses, response headers"
  :author "egao1980"
  :license "MIT"
  :depends-on ("middleware-protocol")
  :pathname "src"
  :components ((:file "clack")))

(defsystem "middleware-protocol/blackbird"
  :version "0.1.0"
  :description "middleware-protocol adapter for blackbird promises: MAP-RESPONSE on asynchronous responses"
  :author "egao1980"
  :license "MIT"
  :depends-on ("middleware-protocol" "blackbird")
  :pathname "src"
  :components ((:file "blackbird")))

(defsystem "middleware-protocol/tests"
  :depends-on ("middleware-protocol"
               "middleware-protocol/clack"
               "middleware-protocol/blackbird"
               "rove"
               "blackbird"
               "bordeaux-threads"
               "closer-mop")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "conditions-test")
               (:file "handler-test")
               (:file "order-test")
               (:file "compose-test")
               (:file "dispatch-test")
               (:file "request-test")
               (:file "response-test")
               (:file "timing-test")
               (:file "error-test")
               (:file "clack-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
