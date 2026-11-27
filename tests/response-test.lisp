;;;; MAP-RESPONSE on immediate, Clack streaming and promise responses
;;;; (response.lisp, clack.lisp, blackbird.lisp, D7).

(in-package #:middleware-protocol/tests)

(defun %add-x (response)
  "The transformation under test: adds :X \"y\" to a Clack-style list."
  (destructuring-bind (status headers &rest rest) response
    (list* status (append headers (list :x "y")) rest)))

(defun %shape (id)
  "A response of shape ID whose immediate form is (200 (:a \"1\") …)."
  (ecase id
    (:immediate (list 200 (list :a "1") (list "body")))
    (:streaming-writer (streaming-app-response (list 200 (list :a "1")) "b1" "b2"))
    (:streaming-full (lambda (responder)
                       (funcall responder (list 200 (list :a "1") (list "body")))))
    (:promise (resolved-promise (list 200 (list :a "1") (list "body"))))
    (:promise-of-streaming (resolved-promise
                            (streaming-app-response (list 200 (list :a "1")) "b1" "b2")))))

(defun %realize (response)
  "→ (values head chunks): RESPONSE as a server would end up sending it."
  (typecase response
    (bb:promise (multiple-value-bind (state value) (settle response)
                  (assert (eq state :resolved))
                  (%realize value)))
    (function (multiple-value-bind (head chunks) (run-streaming response)
                (values head chunks)))
    (t (values response nil))))

(deftest-parametrize map-response-shapes
    ((shape expected-head expected-chunks)
     :ids ("immediate" "streaming-writer" "streaming-full" "promise" "promise-of-streaming")
     (:immediate '(200 (:a "1" :x "y") ("body")) '())
     (:streaming-writer '(200 (:a "1" :x "y")) '("b1" "b2"))
     (:streaming-full '(200 (:a "1" :x "y") ("body")) '())
     (:promise '(200 (:a "1" :x "y") ("body")) '())
     (:promise-of-streaming '(200 (:a "1" :x "y")) '("b1" "b2")))
  (multiple-value-bind (head chunks) (%realize (map-response (%shape shape) #'%add-x))
    (ok (equal expected-head head))
    (ok (equal expected-chunks chunks))))

(deftest map-response-immediate-calls-once-now
  (let* ((calls 0)
         (response (list :r))
         (result (list :mapped)))
    (ok (eq result (map-response response (lambda (r)
                                            (incf calls)
                                            (ok (eq response r))
                                            result))))
    (ok (= 1 calls))))

(deftest map-response-streaming-is-lazy
  (let* ((body-runs 0) (calls 0)
         (original (lambda (responder)
                     (incf body-runs)
                     (funcall (funcall responder (list 200 '())) "chunk" :close t)))
         (mapped (map-response original (lambda (head) (incf calls) (%add-x head)))))
    (ok (functionp mapped))
    (ok (not (eq original mapped)) "a new streaming function")
    (ok (= 0 body-runs) "the body has not run")
    (ok (= 0 calls) "FUNCTION has not been called")
    (multiple-value-bind (head chunks closed) (run-streaming mapped)
      (ok (equal '(200 (:x "y")) head))
      (ok (equal '("chunk") chunks) "chunks are not passed to FUNCTION")
      (ok closed)
      (ok (= 1 body-runs))
      (ok (= 1 calls)))))

(deftest map-response-streaming-returns-the-writer
  (let* ((the-writer (lambda (data &key start end close)
                       (declare (ignore data start end close))))
         (got-from-responder :unset)
         (mapped (map-response (lambda (responder)
                                 (setf got-from-responder (funcall responder (list 200 '()))))
                               #'identity)))
    (funcall mapped (lambda (head) (declare (ignore head)) the-writer))
    (ok (eq the-writer got-from-responder)
        "the app gets the server's writer unchanged")))

(deftest map-response-promise-waits-for-resolution
  (multiple-value-bind (promise resolve) (pending-promise)
    (let* ((calls 0)
           (original (list 200 '() '("x")))
           (mapped (map-response promise (lambda (r) (incf calls) (list :mapped r)))))
      (ok (typep mapped 'bb:promise))
      (ok (not (eq promise mapped)) "a new promise")
      (ok (eq :pending (settle mapped)))
      (ok (= 0 calls))
      (funcall resolve original)
      (ok (= 1 calls))
      (multiple-value-bind (state value) (settle mapped)
        (ok (eq :resolved state))
        (ok (equal (list :mapped original) value)))
      (multiple-value-bind (state value) (settle promise)
        (ok (eq :resolved state))
        (ok (eq original value) "the original promise is unchanged")))))

(deftest map-response-rejected-promise
  (multiple-value-bind (promise resolve reject) (pending-promise)
    (declare (ignore resolve))
    (let* ((calls 0)
           (condition (make-condition 'test-error :tag :rejected))
           (mapped (map-response promise (lambda (r) (incf calls) r))))
      (funcall reject condition)
      (multiple-value-bind (state value) (settle mapped)
        (ok (eq :rejected state))
        (ok (eq condition value) "the same condition"))
      (ok (= 0 calls)))))

(deftest map-response-promise-function-error-rejects
  (let ((mapped (map-response (resolved-promise :v)
                              (lambda (r) (error 'test-error :tag r)))))
    (multiple-value-bind (state value) (settle mapped)
      (ok (eq :rejected state))
      (ok (typep value 'test-error))
      (ok (eq :v (test-error-tag value))))))

(deftest map-response-composes
  (let ((mapped (map-response (map-response (%shape :streaming-writer) #'%add-x)
                              (lambda (head)
                                (destructuring-bind (status headers &rest rest) head
                                  (list* (1+ status) headers rest))))))
    (ok (equal '(201 (:a "1" :x "y")) (%realize mapped))
        "the inner mapping applies first")))
