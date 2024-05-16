(cl:in-package #:invistra)

(defun merge-layout-requirements (r1 r2 ancestor)
  (when (or (and (member :justify-dynamic r1)
                 (member :logical-block r2))
            (and (member :logical-block r1)
                 (member :justify-dynamic r2))
            (and ancestor
                 (member :justify r1)
                 (member :logical-block r2)))
    (error 'incompatible-layout-requirements
           :requirement1 r1
           :requirement2 r2
           :ancestor ancestor))
  (union r1 r2))

(defgeneric control-string (directive))

(defgeneric start (directive))

(defgeneric suffix-start (directive))

(defgeneric end (directive))

(defgeneric directive-character (directive))

(defgeneric parameters (directive))

(defgeneric colon-p (directive))

(defgeneric at-sign-p (directive))

(defgeneric structured-end-p (directive)
  (:method (directive)
    (declare (ignore directive))
    nil))

(defgeneric structured-separator-p (directive)
  (:method (directive)
    (declare (ignore directive))
    nil))

(defgeneric structured-start-p (directive)
  (:method (directive)
    (declare (ignore directive))
    nil))

(defgeneric interpret-parameter (parameter))

(defgeneric compile-parameter (parameter))

(defclass parameter ()
  ((%name :accessor parameter-name
          :initarg :name
          :initform (gensym))
   (%type :accessor parameter-type
          :initarg :type
          :initform '(or null character integer))
   (%default :accessor parameter-default
             :initarg :default
             :initform nil)))

(defclass argument-reference-parameter (parameter)
  ())

(defclass remaining-argument-count-parameter (parameter)
  ())

(defclass literal-parameter (parameter)
  ((%value :accessor parameter-value
           :initarg :value
           :initform nil)))

;;; How we represent a directive.  It may seem wasteful to allocate
;;; a class instance for each directive, but most format directives
;;; are handled at compile time anyway.
(defclass directive ()
  (;; the entire control string in which this directive was found
   (%control-string :initarg :control-string :reader control-string)
   ;; the position in the control string of the ~ character.
   (%start :initarg :start :reader start)
   (%suffix-start :initarg :suffix-start :reader suffix-start)
   ;; the first position beyond the directive character
   (%end :initarg :end :reader end)
   ;; The directive character used.
   (%directive-character :initarg :directive-character :reader directive-character)
   ;; a list of parameters, each one is either an integer or a character
   (%parameters :initarg :parameters :accessor parameters)
   ;; true if and only if the `:' modifier was given
   (%colon-p :initarg :colon-p :reader colon-p)
   ;; true if and only if the `@' modifier was given
   (%at-sign-p :initarg :at-sign-p :reader at-sign-p)))

;;; Mixin class for directives that take no modifiers
(defclass no-modifiers-mixin () ())

;;; Mixin class for directives that take only colon modifiers
(defclass only-colon-mixin () ())

;;; Mixin class for directives that take only at-sign modifiers
(defclass only-at-sign-mixin () ())

;;; Mixin class for directives that take at most one modifier
(defclass at-most-one-modifier-mixin () ())

;;; Mixin class for structured directives
(defclass structured-directive-mixin ()
  ((%clauses :initarg :clauses
             :initform #()
             :accessor clauses)))

(defmethod structured-start-p ((directive structured-directive-mixin))
  t)

;;; Mixin class for directives that end structured directives
(defclass end-structured-directive-mixin () ())

(defmethod structured-end-p ((directive end-structured-directive-mixin))
  t)

(defmethod layout-requirements ((item structured-directive-mixin))
  (loop with requirements = nil
        for clause across (clauses item)
        finally (return requirements)
        do (loop for it across clause
                 do (setf requirements
                          (merge-layout-requirements (layout-requirements it)
                                                     requirements
                                                     nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking syntax, interpreting, and compiling directives.

(defmethod check-directive-syntax progn (client directive)
  (loop for remaining-parameters = (parameters directive) then (cdr remaining-parameters)
        for parameter = (car remaining-parameters)
        for remaining-specs = (parameter-specifications client directive)
          then (if (getf (car remaining-specs) :rest)
                   remaining-specs
                   (cdr remaining-specs))
        for spec = (car remaining-specs)
        for count from 0
        finally (setf (parameters directive) parameters)
        if (and parameter spec)
          do (apply #'reinitialize-instance parameter
                    :allow-other-keys t spec)
        else if (and (null parameter)
                     (or (null spec)
                         (getf spec :rest)))
               do (loop-finish)
        else if (null spec)
          do (error 'too-many-parameters
                    :directive directive
                    :at-most-how-many (1+ count)
                    :how-many-found (+ count (length remaining-parameters)))
        else
          do (setf parameter (apply #'make-instance 'literal-parameter
                                    :allow-other-keys t spec))
        collect parameter into parameters
        when (typep parameter 'literal-parameter)
          do (with-accessors ((parameter-value parameter-value)
                              (parameter-type parameter-type)
                              (parameter-default parameter-default))
                 parameter
               (unless parameter-value
                 (setf parameter-value parameter-default))
               (unless (typep parameter-value parameter-type)
                 (error 'parameter-type-error
                        :expected-type parameter-type
                        :datum parameter-value)))))

;;; Signal an error if a modifier has been given for such a directive.
(defmethod check-directive-syntax progn (client (directive no-modifiers-mixin))
  (declare (ignore client))
  (when (or (colon-p directive) (at-sign-p directive))
    (error 'directive-takes-no-modifiers
           :directive directive)))

;;; Signal an error if an at-sign has been given for such a directive.
(defmethod check-directive-syntax progn (client (directive only-colon-mixin))
  (declare (ignore client))
  (when (at-sign-p directive)
    (error 'directive-takes-only-colon
           :directive directive)))

;;; Signal an error if a colon has been given for such a directive.
(defmethod check-directive-syntax progn (client (directive only-at-sign-mixin))
  (declare (ignore client))
  (when (colon-p directive)
    (error 'directive-takes-only-at-sign
           :directive directive)))

;;; Signal an error if both modifiers have been given for such a directive.
(defmethod check-directive-syntax progn (client (directive at-most-one-modifier-mixin))
  (declare (ignore client))
  (when (and (colon-p directive) (at-sign-p directive))
    (error 'directive-takes-at-most-one-modifier
           :directive directive)))
