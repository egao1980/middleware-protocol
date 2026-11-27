;;;; middleware-protocol/clack: Clack apps as handlers, handlers as Clack apps.
;;;;
;;;; The Clack app contract (http-server-protocol's): an app is
;;;; (lambda (env) → response). ENV is a property list with the normative Clack
;;;; keys (:REQUEST-METHOD :SCRIPT-NAME :PATH-INFO :QUERY-STRING :HEADERS
;;;; :RAW-BODY :REMOTE-ADDR …). A response is either immediate,
;;;; (status headers body) with HEADERS a property list, or a streaming
;;;; function (lambda (responder) …) that calls RESPONDER with (status headers)
;;;; to get a writer (writer data &key start end close), or with
;;;; (status headers body). This adapter depends on no Clack system: the
;;;; contract is plain data and functions.
;;;;
;;;; CLACK-REQUEST (subclass of REQUEST)
;;;;   Initarg :ENV, accessor CLACK-REQUEST-ENV. The env stays normative:
;;;;   middleware reads its keys, and a layer that changes the env installs a new
;;;;   property list with (setf clack-request-env) instead of modifying the one
;;;;   it got. The property bag (request.lisp) is separate from the env.
;;;;
;;;; CLACK-HANDLER (a terminal, subclass of HANDLER)
;;;;   Initarg :APP (a Clack app, any function designator, including one built
;;;;   with Lack), required: MAKE-INSTANCE without it signals an ERROR. Accessor
;;;;   CLACK-HANDLER-APP. Its primary method on HANDLE, for a CLACK-REQUEST, calls
;;;;   the app with
;;;;     (list* :middleware.request request (clack-request-env request))
;;;;   and returns the app's response unchanged (EQ): a list or a streaming
;;;;   function. Handling a request that is not a CLACK-REQUEST signals an ERROR
;;;;   (no applicable method).
;;;;
;;;; ENV-REQUEST env → request or NIL
;;;;   The request under :MIDDLEWARE.REQUEST in ENV, so a Clack app inside a
;;;;   CLACK-HANDLER reaches the request's properties (a session, the client
;;;;   address).
;;;;
;;;; CLACK-APP handler &key (request-class 'clack-request) → Clack app
;;;;   (lambda (env) (handle handler (make-instance request-class :env env))):
;;;;   a fresh request per call, and the response goes back to the server as
;;;;   HANDLE returned it. HANDLER may be any handler (a FUNCTION gets the
;;;;   CLACK-REQUEST). REQUEST-CLASS names CLACK-REQUEST or a subclass.
;;;;
;;;; MAP-RESPONSE on FUNCTION: the Clack streaming response.
;;;;   With this system loaded a FUNCTION response is a Clack streaming response;
;;;;   no other adapter may claim FUNCTION. The result is a new streaming
;;;;   function: when the server calls it with RESPONDER, it calls RESPONSE with
;;;;   a responder of its own that passes the list it gets ((status headers) or
;;;;   (status headers body)) through FUNCTION, gives FUNCTION's value to
;;;;   RESPONDER, and returns RESPONDER's value (the writer) unchanged. Nothing
;;;;   runs before the server calls it. Body chunks written through the writer
;;;;   are not passed to FUNCTION.
;;;;
;;;; RESPONSE-HEADERS-HANDLER (a mixin, subclass of HANDLER)
;;;;   Initarg :RESPONSE-HEADERS (a property list of header names, keywords or
;;;;   strings, and values; default NIL), accessor
;;;;   RESPONSE-HEADERS-HANDLER-HEADERS. Its HANDLE :AROUND method on
;;;;   (RESPONSE-HEADERS-HANDLER CLACK-REQUEST) returns
;;;;     (map-response (call-next-method) f)
;;;;   where F takes an immediate response (status headers . rest) and returns a
;;;;   NEW list (status headers* . rest): HEADERS followed by each configured
;;;;   header whose name the response does not have yet (names compared with
;;;;   STRING-EQUAL on (string name)), in the configured order. The response's
;;;;   own lists are not modified. Through MAP-RESPONSE it also works for
;;;;   streaming responses (the headers go into the head). It is an :AROUND, not
;;;;   a FINISH-RESPONSE method, so it also adds its headers to responses that an
;;;;   inner :AROUND answered without reaching the primary (a 403 from a gate):
;;;;   hooks run only when the request reaches the base method (handler.lisp).
;;;;   For requests that are not CLACK-REQUESTs the method does not apply
;;;;   (pass-through).

(defpackage #:middleware-protocol/clack
  (:use #:cl #:middleware-protocol)
  (:nicknames #:stack-middleware-clack)
  (:documentation "middleware-protocol adapter for Clack apps (file header of clack.lisp).")
  (:export #:clack-request
           #:clack-request-env
           #:clack-handler
           #:clack-handler-app
           #:env-request
           #:clack-app
           #:response-headers-handler
           #:response-headers-handler-headers))

(in-package #:middleware-protocol/clack)

(defclass clack-request (request)
  ((%clack-env :initarg :env :initform nil :accessor clack-request-env))
  (:documentation "A request holding a Clack env (file header)."))

(defclass clack-handler (handler)
  ((%clack-app :initarg :app
               :initform (error "CLACK-HANDLER needs :APP")
               :accessor clack-handler-app))
  (:documentation "Terminal handler calling a Clack app (file header)."))

(defmethod handle ((handler clack-handler) (request clack-request))
  "The terminal: the app's response to the request's env, with the request under
:MIDDLEWARE.REQUEST (file header)."
  (funcall (clack-handler-app handler)
           (list* :middleware.request request (clack-request-env request))))

(defun env-request (env)
  "The request stored under :MIDDLEWARE.REQUEST in ENV, or NIL."
  (getf env :middleware.request))

(defun clack-app (handler &key (request-class 'clack-request))
  "A Clack app that handles each env with HANDLER (file header)."
  ;; Checked, not resolved: a class defined after this call still works by name.
  (check-type request-class (or symbol class))
  (lambda (env)
    (handle handler (make-instance request-class :env env))))

(defmethod map-response ((response function) transform)
  "A Clack streaming response whose head goes through TRANSFORM (file header)."
  (lambda (responder)
    (funcall response
             (lambda (head)
               ;; RESPONDER's value (the writer) goes back to the app unchanged.
               (funcall responder (funcall transform head))))))

(defun %add-response-headers (response additions)
  "A new immediate response: RESPONSE's headers followed by each header of the plist
ADDITIONS whose name (STRING-EQUAL) the response lacks, in order. APPEND copies
the header list, so RESPONSE's own lists are not modified."
  (destructuring-bind (status headers &rest rest) response
    (list* status
           (append headers
                   (loop for (name value) on additions by #'cddr
                         unless (loop for (present) on headers by #'cddr
                                      thereis (string-equal (string name) (string present)))
                           collect name
                           and collect value))
           rest)))

(defclass response-headers-handler (handler)
  ((%response-headers :initarg :response-headers :initform nil
                      :accessor response-headers-handler-headers))
  (:documentation "Mixin adding response headers the response does not have (file header)."))

(defmethod handle :around ((handler response-headers-handler) (request clack-request))
  "Add the configured headers the response lacks, also to streamed heads and to
short-circuit answers (file header)."
  (let ((additions (response-headers-handler-headers handler)))
    (map-response (call-next-method)
                  (lambda (response)
                    (%add-response-headers response additions)))))
