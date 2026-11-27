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

(defvar *handler-classes* (make-hash-table :test 'equal)
  "HANDLER-CLASS's cache: a list of class objects → the class built for it. EQUAL
compares class objects with EQL, and a redefined mixin keeps its class object, so
an entry follows redefinitions. A DEFVAR, so reloading this file keeps the classes
already handed out (image-first).")

(defvar *handler-classes-lock* (bt2:make-lock :name "middleware-protocol handler classes")
  "Held across the cache lookup and the build, so racing callers get one class.")

(defun %invalid-mixins (mixins control &rest arguments)
  "Signal INVALID-MIXINS for MIXINS (the argument as given), REASON formatted from
CONTROL and ARGUMENTS."
  (error 'invalid-mixins :mixins mixins :reason (apply #'format nil control arguments)))

(defun %list-shape (list)
  "How LIST ends: :PROPER (and its length as a second value), :DOTTED or :CIRCULAR.
A non-NIL atom is :DOTTED. Never signals, and terminates on a circular list (the
two pointers of CLHS LIST-LENGTH)."
  (do ((n 0 (+ n 2))
       (fast list (cddr fast))
       (slow list (cdr slow)))
      (nil)
    (cond ((null fast) (return (values :proper n)))
          ((atom fast) (return :dotted))
          ((null (cdr fast)) (return (values :proper (1+ n))))
          ((atom (cdr fast)) (return :dotted))
          ((and (eq fast slow) (plusp n)) (return :circular)))))

(defun %class-label (class)
  "CLASS's name for a message, or CLASS itself when it is anonymous."
  (or (class-name class) class))

(defun %mixin-classes (mixins)
  "The classes MIXINS designates, in order, as a fresh list. Signals INVALID-MIXINS
when MIXINS breaks a rule of the file header. Runs on every call, cache hit or not,
and without the lock."
  (cond ((null mixins) (%invalid-mixins mixins "the list is empty"))
        ((atom mixins) (%invalid-mixins mixins "it is not a list"))
        (t (case (%list-shape mixins)
             (:dotted (%invalid-mixins mixins "it is a dotted list"))
             (:circular (%invalid-mixins mixins "it is a circular list")))))
  (let ((classes (mapcar (lambda (designator)
                           (typecase designator
                             (class designator)
                             (symbol (or (find-class designator nil)
                                         (%invalid-mixins mixins "~S names no class" designator)))
                             (t (%invalid-mixins mixins "~S is neither a class nor a symbol naming one"
                                                 designator))))
                         mixins)))
    ;; On the resolved classes, so a class and a symbol naming it are one mixin.
    (loop for (class . more) on classes
          when (member class more :test #'eq)
            do (%invalid-mixins mixins "~S appears more than once: a mixin is in a class once"
                                (%class-label class)))
    classes))

(defun %unbuildable-reason (classes)
  "NIL when a STANDARD-CLASS with the direct superclasses CLASSES can be built and
finalized; otherwise a string saying why not. It reads the class graph and changes
nothing. ENSURE-CLASS would register a half-built class as a direct subclass of each
of CLASSES before it found the problem, and that class then breaks every later
redefinition of those classes. The checks: no class among CLASSES and their
superclasses is forward-referenced, the metaobject protocol accepts each of CLASSES
as a direct superclass (VALIDATE-SUPERCLASS), and the union of the local precedence
orders has no cycle, which is when a class precedence list exists (CLHS 4.3.5)."
  (let ((prototype (closer-mop:class-prototype (find-class 'standard-class)))
        (successors (make-hash-table :test 'eq)) ; class → the classes it must precede
        (state (make-hash-table :test 'eq)))     ; class → :UNVISITED, :ACTIVE or :DONE
    (labels ((fail (control &rest arguments)
               (return-from %unbuildable-reason (apply #'format nil control arguments)))
             (precede (list)
               ;; A local precedence order: each element precedes the next.
               (loop for (a b) on list
                     while b
                     do (pushnew b (gethash a successors))))
             (collect (class)
               (unless (gethash class state)
                 (when (typep class 'closer-mop:forward-referenced-class)
                   (fail "~S is not defined yet" (%class-label class)))
                 (setf (gethash class state) :unvisited)
                 (let ((supers (closer-mop:class-direct-superclasses class)))
                   (precede (cons class supers))
                   (mapc #'collect supers))))
             (visit (class)
               (setf (gethash class state) :active)
               (dolist (next (gethash class successors))
                 (case (gethash next state)
                   (:active (fail "no class precedence list exists: ~S must both precede and follow ~S"
                                  (%class-label next) (%class-label class)))
                   (:unvisited (visit next))))
               (setf (gethash class state) :done)))
      (dolist (class classes)
        (cond ((typep class 'closer-mop:forward-referenced-class)
               (fail "~S is not defined yet" (%class-label class)))
              ((not (closer-mop:validate-superclass prototype class))
               (fail "~S cannot be a direct superclass of a STANDARD-CLASS" (%class-label class)))))
      ;; The new class precedes the first of CLASSES; as it has no other edge, it
      ;; is in no cycle, so only the order among CLASSES needs recording.
      (precede classes)
      (mapc #'collect classes)
      ;; Every superclass is reachable from CLASSES, so this visits the whole graph.
      (dolist (class classes nil)
        (when (eq (gethash class state) :unvisited)
          (visit class))))))

(defun %handler-class-name (classes)
  "An uninterned symbol for the class built from CLASSES, named after them: anonymous,
yet readable in a backtrace."
  (make-symbol (with-standard-io-syntax
                 (let ((*print-pretty* nil))
                   (prin1-to-string (mapcar #'class-name classes))))))

(defun %build-handler-class (classes)
  "A new finalized anonymous STANDARD-CLASS whose direct superclasses are CLASSES."
  (let ((class (closer-mop:ensure-class (%handler-class-name classes)
                                        :direct-superclasses classes
                                        :metaclass 'standard-class)))
    ;; ENSURE-CLASS need not finalize (SBCL's does not).
    (closer-mop:ensure-finalized class)
    class))

(defun handler-class (mixins)
  "The cached anonymous class whose direct superclasses are MIXINS (file header)."
  (let ((classes (%mixin-classes mixins))
        (reason nil))
    (or (bt2:with-lock-held (*handler-classes-lock*)
          (or (gethash classes *handler-classes*)
              (progn
                (setf reason (%unbuildable-reason classes))
                (and (null reason)
                     ;; Stored only once the class is built and finalized: a
                     ;; failure caches nothing.
                     (setf (gethash classes *handler-classes*)
                           (%build-handler-class classes))))))
        ;; Signalled with the lock released, so a handler may call HANDLER-CLASS again.
        (%invalid-mixins mixins "~A" reason))))

(defun make-handler (mixins &rest initargs)
  "An instance of (HANDLER-CLASS MIXINS) made with INITARGS (file header)."
  (apply #'make-instance (handler-class mixins) initargs))
