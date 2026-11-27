# middleware-protocol

Middleware for cl-stack handlers of any request and response type, written the CL way: with method combination.

- **One generic function**, `(handle handler request) → response`. A function is a handler; so is any object whose class has a primary method on `handle`.
- **A middleware is a mixin class** with `:around` methods on `handle` (short-circuit, rewrite the request, map errors, transform the response) or `:before` / `:after` methods (observe). The **class precedence list orders the layers**: the leftmost mixin is the outermost.
- **Run-time stacks** come from `make-handler`, which builds an anonymous class from a list of mixins (cached per list). No chain object, no `next` argument, no runtime `add-method`.
- **Hooks** for the common cases: `prepare-request` before the primary, `finish-response` after it.
- **Requests** that carry context subclass `request` (a property bag). **Responses** may be immediate or deferred; `map-response` transforms either, so "commit on the way out" code is written once.
- **Standard mixins:** `timing-handler` (reports to a function), `error-handler` (conditions → responses through `handle-error`, with `handler-bind`, so restarts survive), and in the Clack adapter `response-headers-handler`.
- **Adapters:** `middleware-protocol/clack` (Clack apps ↔ handlers, Clack streaming responses) and `middleware-protocol/blackbird` (promises).

```lisp
(asdf:load-system "middleware-protocol/clack")

(defclass app (stack-middleware-clack:response-headers-handler
               stack-middleware:timing-handler
               stack-middleware:error-handler
               stack-middleware-clack:clack-handler)
  ())

(stack-middleware-clack:clack-app
 (make-instance 'app
                :app #'my-clack-app
                :response-headers '(:x-frame-options "DENY")
                :timing-report (lambda (request response seconds) …)
                :error-function (lambda (request condition)
                                  '(500 (:content-type "text/plain") ("Internal error")))))
;; → a Clack app for any http-server-protocol backend

;; The same stack chosen at run time (both packages used):
(stack-middleware:make-handler '(response-headers-handler timing-handler error-handler clack-handler)
                               :app #'my-clack-app …)
```

**Status:** M0 (0.1.0) implemented (team-solve: 112 tests, 356 assertions). The rules are in the file headers of `src/`, and the Rove suite in `tests/` is the specification. The design and its locked decisions are in the capability brief [cl-stack/docs/capabilities/middleware.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/middleware.md).

```sh
cl-repo test --system middleware-protocol
```

## License

MIT, see [LICENSE](LICENSE).
