;;;; Run-time composition: HANDLER-CLASS, MAKE-HANDLER (D3).
;;;;
;;;; A stack chosen at run time (from configuration, per route) is a class built
;;;; at run time: the only dynamic API. Nothing here adds methods to a generic
;;;; function (no ADD-METHOD as public API, as in the http client).
;;;;
;;;; HANDLER-CLASS mixins → class
;;;;
;;;;   * MIXINS: a non-empty proper list of class designators (symbols naming
;;;;     classes, or class objects), outermost layer first, normally ending with
;;;;     a terminal.
;;;;   * Returns a class of metaclass STANDARD-CLASS whose direct superclasses are
;;;;     exactly those classes, in that order, built with closer-mop's
;;;;     ENSURE-CLASS. It is finalized, so CLASS-PRECEDENCE-LIST works on it at
;;;;     once. It is anonymous: its name is NIL or an uninterned symbol, so no
;;;;     interned name ever reaches it.
;;;;   * Cached per list of classes: two calls whose designators resolve to the
;;;;     same classes in the same order return the same (EQ) class, also when the
;;;;     calls come from several threads at once. Redefining a mixin (DEFCLASS
;;;;     again) keeps the class object, so the cached class follows it.
;;;;   * Signals INVALID-MIXINS (MIXINS the argument as given, REASON a string)
;;;;     when MIXINS is not a non-empty proper list, when an element is neither a
;;;;     class nor a symbol naming one, or when a class appears twice (a mixin is
;;;;     in a class once: D4).
;;;;   * Mixins whose class precedence list cannot be computed (for example a
;;;;     class listed before one of its own subclasses) signal an ERROR, and
;;;;     nothing is cached: the next call signals again.
;;;;
;;;; MAKE-HANDLER mixins &rest initargs → handler
;;;;
;;;;   (apply #'make-instance (handler-class mixins) initargs). Each mixin takes
;;;;   its own initargs (D4); an initarg no mixin declares signals CLOS's ERROR.

(in-package #:middleware-protocol)

(defun handler-class (mixins)
  "The cached anonymous class whose direct superclasses are MIXINS (file header)."
  (declare (ignore mixins))
  (%m0-pending 'handler-class))

(defun make-handler (mixins &rest initargs)
  "An instance of (HANDLER-CLASS MIXINS) made with INITARGS (file header)."
  (declare (ignore mixins initargs))
  (%m0-pending 'make-handler))
