;;;; middleware-protocol/clack: the Clack bridge round trip, streaming,
;;;; RESPONSE-HEADERS-HANDLER (clack.lisp).

(in-package #:middleware-protocol/tests)

(defun %env (&rest overrides)
  "A Clack env for GET /hello, with OVERRIDES in front."
  (append overrides
          (list :request-method :get
                :script-name ""
                :path-info "/hello"
                :query-string "a=1"
                :server-name "localhost"
                :server-port 8080
                :remote-addr "127.0.0.1"
                :headers (make-hash-table :test 'equal))))

(defun %recording-app (box response)
  "A Clack app that stores its env in (car BOX) and answers RESPONSE."
  (lambda (env)
    (setf (car box) env)
    response))

(deftest clack-request-holds-the-env
  (let* ((env (%env))
         (request (make-instance 'clack-request :env env)))
    (ok (typep request 'request))
    (ok (eq env (clack-request-env request)))
    (setf (request-property request :session) :s)
    (ok (eq :s (request-property request :session)))
    (ok (null (getf (clack-request-env request) :session))
        "properties are not env keys")))

(deftest clack-handler-calls-the-app-with-the-env
  (let* ((box (list nil))
         (response (list 200 (list :content-type "text/plain") (list "hi")))
         (handler (make-instance 'clack-handler :app (%recording-app box response)))
         (env (%env))
         (request (make-instance 'clack-request :env env)))
    (ok (eq response (handle handler request)) "the app's response, unchanged")
    (let ((seen (car box)))
      (ok (eq request (getf seen :middleware.request)))
      (ok (eq request (env-request seen)))
      (loop for (key value) on env by #'cddr
            do (ok (eq value (getf seen key)) (format nil "env key ~S" key))))))

(deftest clack-handler-requires-app-and-a-clack-request
  (ok (equal '(200 () ())
             (handle (make-instance 'clack-handler :app (constantly '(200 () ())))
                     (make-instance 'clack-request :env (%env)))))
  (ok (signals (make-instance 'clack-handler) 'error))
  (ok (signals (handle (make-instance 'clack-handler :app (constantly '(200 () ())))
                       (make-instance 'request))
               'error)))

(deftest env-request-without-a-request
  (ok (null (env-request (%env)))))

(deftest clack-app-is-a-clack-app
  (let* ((box (list nil))
         (response (list 200 (list :content-type "text/plain") (list "hi")))
         (app (clack-app (make-instance 'clack-handler :app (%recording-app box response))))
         (env (%env)))
    (ok (functionp app))
    (ok (eq response (funcall app env)))
    (let ((request (env-request (car box))))
      (ok (typep request 'clack-request))
      (ok (eq env (clack-request-env request)) "a fresh request around the server's env"))
    (funcall app (%env))
    (ok (not (eq env (clack-request-env (env-request (car box))))) "one request per call")))

(defclass my-clack-request (clack-request) ())

(deftest clack-app-request-class
  (let* ((box (list nil))
         (app (clack-app (make-instance 'clack-handler :app (%recording-app box '(204 () ())))
                         :request-class 'my-clack-request)))
    (funcall app (%env))
    (ok (typep (env-request (car box)) 'my-clack-request))))

(deftest clack-app-over-a-function-handler
  (let ((app (clack-app (lambda (request)
                          (list 200 '() (list (getf (clack-request-env request) :path-info)))))))
    (ok (equal '(200 () ("/hello")) (funcall app (%env))))))

;;; The round trip: a Clack app wrapped with header and timing middleware
;;; behaves as a Clack app.

(defun %stack (app log &rest mixins-before-terminal)
  (clack-app (make-handler (append mixins-before-terminal '(clack-handler))
                           :app app
                           :response-headers '(:x-frame-options "DENY"
                                               :content-security-policy "default-src 'self'")
                           :timing-report (lambda (request response seconds)
                                            (push (list request response seconds) (car log)))
                           :timing-clock (let ((now 0)) (lambda () (incf now 2))))))

(deftest round-trip-immediate
  (let* ((log (list '()))
         (inner (list 200 (list :content-type "text/html") (list "<p>hi</p>")))
         (app (%stack (constantly inner) log 'response-headers-handler 'timing-handler))
         (response (funcall app (%env))))
    (ok (equal '(200 (:content-type "text/html"
                      :x-frame-options "DENY"
                      :content-security-policy "default-src 'self'")
                 ("<p>hi</p>"))
               response))
    (ok (equal '(200 (:content-type "text/html") ("<p>hi</p>")) inner)
        "the app's own lists are not modified")
    (ok (= 1 (length (car log))))
    (destructuring-bind (request reported seconds) (first (car log))
      (ok (typep request 'clack-request))
      (ok (eq inner reported) "timing sits inside the headers: it saw the app's response")
      (ok (= 2 seconds)))))

(deftest round-trip-streaming
  (let* ((log (list '()))
         (app (%stack (lambda (env)
                        (declare (ignore env))
                        (streaming-app-response (list 200 (list :content-type "text/event-stream"))
                                                "data: 1" "data: 2"))
                      log 'timing-handler 'response-headers-handler))
         (response (funcall app (%env))))
    (ok (functionp response) "still a Clack streaming response")
    (ok (null (car log)))
    (multiple-value-bind (head chunks closed) (run-streaming response)
      (ok (equal '(200 (:content-type "text/event-stream"
                        :x-frame-options "DENY"
                        :content-security-policy "default-src 'self'"))
                 head))
      (ok (equal '("data: 1" "data: 2") chunks))
      (ok closed))
    (ok (= 1 (length (car log))))
    (ok (equal '(200 (:content-type "text/event-stream"
                      :x-frame-options "DENY"
                      :content-security-policy "default-src 'self'"))
               (second (first (car log))))
        "timing sits outside the headers: it saw the head with them")))

(deftest round-trip-error-mapped-inside-headers
  (let* ((app (clack-app
               (make-handler '(response-headers-handler error-handler clack-handler)
                             :app (lambda (env) (declare (ignore env)) (error 'test-error))
                             :error-function (lambda (request c)
                                               (declare (ignore request c))
                                               (list 500 (list :content-type "text/plain")
                                                     (list "Internal error")))
                             :response-headers '(:x-frame-options "DENY")))))
    (ok (equal '(500 (:content-type "text/plain" :x-frame-options "DENY") ("Internal error"))
               (funcall app (%env))))))

(defclass m-token-gate (handler) ())

(defmethod handle :around ((handler m-token-gate) (request clack-request))
  (if (getf (clack-request-env request) :token-ok)
      (call-next-method)
      (list 403 (list :content-type "text/plain") (list "Forbidden"))))

(deftest round-trip-short-circuit-still-gets-headers
  (let* ((calls 0)
         (app (clack-app
               (make-handler '(response-headers-handler m-token-gate clack-handler)
                             :app (lambda (env)
                                    (declare (ignore env))
                                    (incf calls)
                                    (list 200 '() (list "secret")))
                             :response-headers '(:x-frame-options "DENY")))))
    (ok (equal '(403 (:content-type "text/plain" :x-frame-options "DENY") ("Forbidden"))
               (funcall app (%env))))
    (ok (= 0 calls))
    (ok (equal '(200 (:x-frame-options "DENY") ("secret"))
               (funcall app (%env :token-ok t))))
    (ok (= 1 calls))))

;;; A layer that rewrites the env installs a new plist.
(defclass m-strip-prefix (handler) ())

(defmethod prepare-request ((handler m-strip-prefix) (request clack-request))
  (let* ((env (clack-request-env request))
         (path (getf env :path-info)))
    (when (and (stringp path) (eql 0 (search "/api" path)))
      (setf (clack-request-env request)
            (list* :path-info (subseq path 4) :script-name "/api" env))))
  (call-next-method))

(deftest env-rewrite-by-a-layer
  (let* ((box (list nil))
         (app (clack-app (make-handler '(m-strip-prefix clack-handler)
                                       :app (%recording-app box '(200 () ())))))
         (env (%env :path-info "/api/items")))
    (funcall app env)
    (ok (equal "/items" (getf (car box) :path-info)))
    (ok (equal "/api" (getf (car box) :script-name)))
    (ok (equal "/api/items" (getf env :path-info)) "the server's env is not modified")))

(deftest lack-style-app-as-the-primary
  ;; A Lack-style middleware (a function from app to app) around an app; the
  ;; result is the primary of a CLACK-HANDLER, below protocol middleware.
  (let* ((lack-middleware (lambda (app)
                            (lambda (env)
                              (destructuring-bind (status headers body) (funcall app env)
                                (list status (append headers (list :x-lack "1")) body)))))
         (base (lambda (env)
                 (list 200 '() (list (if (env-request env) "has request" "no request")))))
         (app (clack-app (make-handler '(response-headers-handler clack-handler)
                                       :app (funcall lack-middleware base)
                                       :response-headers '(:x-frame-options "DENY")))))
    (ok (equal '(200 (:x-lack "1" :x-frame-options "DENY") ("has request"))
               (funcall app (%env))))))

(deftest inner-app-reads-request-properties
  (let* ((app (clack-app
               (make-handler '(m-context clack-handler)
                             :app (lambda (env)
                                    (let ((request (env-request env)))
                                      (list 200 '()
                                            (list (princ-to-string
                                                   (request-property request :from-outer)))))))))
         (*observed* '()))
    (ok (equal '(200 () ("OUTER-VALUE")) (funcall app (%env))))))

(deftest-parametrize response-headers-merge
    ((configured headers expected)
     :ids ("added" "appended" "present-kept" "case-insensitive" "order" "none")
     ('(:x-frame-options "DENY") '() '(:x-frame-options "DENY"))
     ('(:x-frame-options "DENY") '(:content-type "text/html")
      '(:content-type "text/html" :x-frame-options "DENY"))
     ('(:content-type "text/plain") '(:content-type "text/html") '(:content-type "text/html"))
     ('("X-Frame-Options" "DENY") '(:x-frame-options "SAMEORIGIN") '(:x-frame-options "SAMEORIGIN"))
     ('(:a "1" :b "2" :c "3") '(:b "0") '(:b "0" :a "1" :c "3"))
     ('() '(:c "1") '(:c "1")))
  (let* ((inner (list 200 (copy-list headers) (list "body")))
         (handler (make-handler '(response-headers-handler function-handler)
                                :function (constantly inner)
                                :response-headers configured))
         (response (handle handler (make-instance 'clack-request :env (%env)))))
    (ok (equal (list 200 expected (list "body")) response))
    (ok (equal headers (second inner)) "the response's own header list is not modified")))

(deftest response-headers-accessor
  (let ((handler (make-handler '(response-headers-handler function-handler)
                               :function (constantly (list 200 '() '())))))
    (ok (null (response-headers-handler-headers handler)))
    (setf (response-headers-handler-headers handler) '(:x-a "1"))
    (ok (equal '(200 (:x-a "1") ())
               (handle handler (make-instance 'clack-request :env (%env)))))))

(deftest clack-mixins-keep-their-own-slots
  (let* ((app (lambda (env) (declare (ignore env)) (list 200 '() '())))
         (handler (make-handler '(response-headers-handler timing-handler error-handler clack-handler)
                                :app app
                                :response-headers '(:x-a "1")
                                :error-function (constantly :mapped))))
    (ok (eq app (clack-handler-app handler)))
    (ok (equal '(:x-a "1") (response-headers-handler-headers handler)))
    (ok (eq :mapped (funcall (error-handler-function handler) nil nil)))
    (ok (= (length (closer-mop:class-slots (class-of handler)))
           (reduce #'+ '(response-headers-handler timing-handler error-handler clack-handler)
                   :key (lambda (name) (length (closer-mop:class-slots (find-class name))))))
        "no two mixins share a slot")
    (let ((request (make-instance 'clack-request :env (%env) :properties '(:p 1))))
      (ok (eql 1 (request-property request :p)))
      (ok (equal "/hello" (getf (clack-request-env request) :path-info))
          "the env and the property bag are separate slots"))))
