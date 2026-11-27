# middleware-protocol

Middleware for cl-stack handlers of any request and response type. A handler answers `(handle handler request)`; a middleware is a mixin class whose `:around`, `:before` and `:after` methods on `handle` wrap it, ordered by the class precedence list. Responses may be immediate or deferred (Clack streaming functions, promises). The first adapter bridges Clack apps.

**Status:** skeleton (0.0.1). The design is in the capability brief [cl-stack/docs/capabilities/middleware.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/middleware.md). The API and tests land through contract branches and team-solve.

```sh
cl-repo test --system middleware-protocol
```

## License

MIT, see [LICENSE](LICENSE).
