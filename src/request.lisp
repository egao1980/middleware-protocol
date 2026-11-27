;;;; Requests: REQUEST and its property bag (D5, D6).
;;;;
;;;; Context for inner layers lives ON THE REQUEST. A dynamic variable ends with
;;;; the :AROUND that bound it, before a streamed or asynchronous body runs; the
;;;; request object travels with every layer and with FINISH-RESPONSE, also when
;;;; a deferred response is produced later. A mixin may in addition bind a
;;;; documented special variable of its own for synchronous code, never as the
;;;; only channel.
;;;;
;;;; REQUEST
;;;;
;;;;   Base class of requests that carry a property bag. Adapters subclass it
;;;;   (CLACK-REQUEST holds the Clack env). Initarg :PROPERTIES, a property list
;;;;   copied at initialization: the caller's list is never modified. Requests
;;;;   of other types (strings, plists, foreign objects) are fine for HANDLE but
;;;;   have no bag: the functions below signal an ERROR for them (no applicable
;;;;   method) unless an adapter defines methods.
;;;;
;;;; REQUEST-PROPERTY request key &optional default → value, present-p
;;;;   Keys are compared with EQL: keywords, or symbols of the mixin's package.
;;;;   An absent key gives DEFAULT and NIL.
;;;;
;;;; (SETF REQUEST-PROPERTY) value request key &optional default → value
;;;;   Adds KEY or replaces its value. DEFAULT is ignored (as for GETHASH).
;;;;
;;;; REMOVE-REQUEST-PROPERTY request key → generalized boolean
;;;;   Removes KEY; true when it was present.
;;;;
;;;; REQUEST-PROPERTIES request → plist
;;;;   A fresh property list with each present key once. Modifying it does not
;;;;   change the request.
;;;;
;;;; Visibility: one HANDLE call passes one request object through every layer
;;;; (unless a layer rewrites the request), so a property set by an outer
;;;; layer's PREPARE-REQUEST or :AROUND is seen by inner layers and the primary,
;;;; and a property set inside is seen by outer FINISH-RESPONSE methods and
;;;; :AROUND exits, also when a deferred response is produced later.
;;;;
;;;; Not synchronized: a request's properties are written by one thread at a
;;;; time (the one handling it, then the one producing its deferred response).

(in-package #:middleware-protocol)

(defclass request ()
  ((%request-properties :initarg :properties :initform nil))
  (:documentation "Base class of requests with a property bag (file header)."))

(defgeneric request-property (request key &optional default)
  (:documentation "KEY's value in REQUEST's property bag, and whether it is present."))

(defgeneric (setf request-property) (value request key &optional default)
  (:documentation "Set KEY's value in REQUEST's property bag."))

(defgeneric remove-request-property (request key)
  (:documentation "Remove KEY from REQUEST's property bag; true when it was present."))

(defgeneric request-properties (request)
  (:documentation "A fresh property list of REQUEST's properties."))

;;; The slot holds a plist with each key once (compared with EQL) whose conses
;;; belong to the request alone: :PROPERTIES is copied in and REQUEST-PROPERTIES
;;; copies out. So (SETF REQUEST-PROPERTY) may replace a value in place.

(defun %property-list-p (object)
  "True when OBJECT is a proper list of even length (CHECK-TYPE's test for :PROPERTIES)."
  (multiple-value-bind (shape length) (%list-shape object)
    (and (eq shape :proper) (evenp length))))

(defun %property-tail (plist key)
  "The tail of PLIST that starts with KEY (compared with EQL), or NIL."
  (loop for tail on plist by #'cddr
        when (eql (car tail) key)
          return tail))

(defun %unique-properties (plist)
  "A fresh plist of PLIST's keys, each once (EQL), the first occurrence winning as
for GETF, in PLIST's order."
  (loop with keys = '()
        for (key value) on plist by #'cddr
        unless (member key keys)        ; MEMBER's default test is EQL
          do (push key keys)
          and collect key
          and collect value))

(defmethod shared-initialize :after ((request request) slot-names
                                     &key ((:properties properties) nil properties-p))
  "Copy :PROPERTIES, so the caller's list is never modified (file header)."
  (declare (ignore slot-names))
  (when properties-p
    (check-type properties (satisfies %property-list-p) "a property list")
    (setf (slot-value request '%request-properties) (%unique-properties properties))))

(defmethod request-property ((request request) key &optional default)
  "KEY's value and T, or DEFAULT and NIL (file header)."
  (let ((tail (%property-tail (slot-value request '%request-properties) key)))
    (if tail
        (values (second tail) t)
        (values default nil))))

(defmethod (setf request-property) (value (request request) key &optional default)
  "Add KEY or replace its value; DEFAULT is ignored (file header)."
  (declare (ignore default))
  (let ((tail (%property-tail (slot-value request '%request-properties) key)))
    (if tail
        (setf (second tail) value)
        (setf (slot-value request '%request-properties)
              (list* key value (slot-value request '%request-properties))))
    value))

(defmethod remove-request-property ((request request) key)
  "Remove KEY; true when it was present (file header)."
  (let ((plist (slot-value request '%request-properties)))
    (when (%property-tail plist key)
      (setf (slot-value request '%request-properties)
            (loop for (k v) on plist by #'cddr
                  unless (eql k key)
                    collect k
                    and collect v))
      t)))

(defmethod request-properties ((request request))
  "A fresh plist of REQUEST's properties (file header)."
  (copy-list (slot-value request '%request-properties)))
