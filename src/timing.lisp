;;;; TIMING-HANDLER: time each request and report it to a function.
;;;;
;;;; TIMING-HANDLER (a mixin, subclass of HANDLER)
;;;;
;;;;   Initargs and accessors:
;;;;   * :TIMING-REPORT, TIMING-HANDLER-REPORT: NIL (the default) or a function
;;;;     of three arguments, REQUEST RESPONSE SECONDS. Its value is ignored.
;;;;   * :TIMING-CLOCK, TIMING-HANDLER-CLOCK: a function of no arguments that
;;;;     returns the time in seconds as a real number, never decreasing. The
;;;;     default reads GET-INTERNAL-REAL-TIME divided by
;;;;     INTERNAL-TIME-UNITS-PER-SECOND.
;;;;
;;;;   HANDLE :AROUND on (TIMING-HANDLER T):
;;;;   * REPORT NIL: returns (call-next-method)'s response as it is (EQ) and does
;;;;     not read the clock.
;;;;   * Otherwise it reads the clock once on entry (START), before any inner
;;;;     layer runs, calls CALL-NEXT-METHOD, and returns
;;;;       (map-response response f)
;;;;     where F reads the clock once (END), calls REPORT with the request as this
;;;;     layer received it, the immediate response, and (- END START), and
;;;;     returns the response unchanged (EQ). So the report comes before HANDLE
;;;;     returns for an immediate response, when the head reaches the responder
;;;;     for a Clack streaming response, and when the promise resolves for a
;;;;     promise. Two clock readings per request, one report.
;;;;   * A condition from an inner layer propagates unchanged and REPORT is not
;;;;     called. A TIMING-HANDLER outside an ERROR-HANDLER times the mapped
;;;;     error responses too.

(in-package #:middleware-protocol)

(defun %internal-real-seconds ()
  "The default clock (file header)."
  (%m0-pending '%internal-real-seconds))

(defclass timing-handler (handler)
  ((%timing-report :initarg :timing-report :initform nil :accessor timing-handler-report)
   (%timing-clock :initarg :timing-clock :initform #'%internal-real-seconds
                  :accessor timing-handler-clock))
  (:documentation "Mixin reporting each request's time to a function (file header)."))

(defmethod handle :around ((handler timing-handler) request)
  (declare (ignore request))
  (%m0-pending '(handle :around timing-handler)))
