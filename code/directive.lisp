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
   (%bind :accessor parameter-bind-p
          :initarg :bind
          :initform t
          :type boolean)
   (%default :accessor parameter-default
             :initarg :default
             :initform nil)
   (%start :accessor start
           :initarg :start)
   (%end :accessor end
         :initarg :end)))

(defclass argument-reference-parameter (parameter)
  ())

(defmethod calculate-argument-position (position (item argument-reference-parameter))
  (when position
    (1+ position)))

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
   (%control-string :accessor control-string
                    :initarg :control-string)
   ;; the position in the control string of the ~ character.
   (%start :accessor start
           :initarg :start)
   (%modifiers-start :accessor modifiers-start
                     :initarg :modifiers-start)
   (%character-start :accessor character-start
                     :initarg :character-start)
   (%suffix-start :accessor suffix-start
                  :initarg :suffix-start)
   ;; the first position beyond the directive character
   (%end :accessor end
         :initarg :end)
   ;; The directive character used.
   (%directive-character :accessor directive-character
                         :initarg :directive-character)
   ;; a list of parameters, each one is either an integer or a character
   (%parameters :accessor parameters
                :initarg :parameters
                :initform nil)
   ;; true if and only if the `:' modifier was given
   (%colon-p :accessor colon-p
             :initarg :colon-p
             :initform nil)
   ;; true if and only if the `@' modifier was given
   (%at-sign-p :accessor at-sign-p
               :initarg :at-sign-p
               :initform nil)))

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

(defmethod calculate-argument-position (position (item directive))
  (reduce #'calculate-argument-position (parameters item) :initial-value position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking syntax, interpreting, and compiling directives.

(defmethod check-item-syntax progn
    ((client standard-client) (directive structured-directive-mixin) parent
     &optional group position)
  (declare (ignore parent group position))
  (loop for items across (clauses directive)
        for group from 0
        do (loop for item across items
                 for position from 0
                 do (check-item-syntax client item directive group position))))

(defmethod check-item-syntax progn
    ((client standard-client) (directive directive) parent &optional group position)
  (declare (ignore parent group position))
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
                    :client client
                    :directive directive
                    :at-most-how-many (1+ count)
                    :how-many-found (+ count (length remaining-parameters))
                    :positions (loop for i from (1+ (start directive))
                                       below (modifiers-start directive)
                                     collect i))
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
                        :client client
                        :directive directive
                        :positions (loop for i from (start parameter) below (end parameter)
                                         collect i)
                        :expected-type parameter-type
                        :datum parameter-value)))))

;;; Signal an error if a modifier has been given for such a directive.
(defmethod check-item-syntax progn
    ((client standard-client) (directive no-modifiers-mixin) parent &optional group position)
  (declare (ignore parent group position))
  (cond ((and (colon-p directive) (at-sign-p directive))
         (error 'illegal-modifiers
                :client client
                :directive directive
                :modifier-characters '(#\@ #\:)
                :list (loop for i from (modifiers-start directive)
                              below (character-start directive)
                            collect i)))
        ((colon-p directive)
         (error 'illegal-modifiers
                :client client
                :directive directive
                :modifier-characters '(#\:)
                :list (loop for i from (modifiers-start directive)
                              below (character-start directive)
                            collect i)))
        ((at-sign-p directive)
         (error 'illegal-modifiers
                :client client
                :directive directive
                :modifier-characters '(#\@)
                :list (loop for i from (modifiers-start directive)
                              below (character-start directive)
                            collect i)))))

;;; Signal an error if an at-sign has been given for such a directive.
(defmethod check-item-syntax progn
    ((client standard-client) (directive only-colon-mixin) parent &optional group position)
  (declare (ignore parent group position))
  (when (at-sign-p directive)
    (error 'illegal-modifiers
           :client client
           :directive directive
           :modifier-characters '(#\@)
           :positions (list (position #\@ (control-string directive)
                                      :start (modifiers-start directive))))))

;;; Signal an error if a colon has been given for such a directive.
(defmethod check-item-syntax progn
    ((client standard-client) (directive only-at-sign-mixin) parent &optional group position)
  (declare (ignore parent group position))
  (when (colon-p directive)
    (error 'illegal-modifiers
           :client client
           :directive directive
           :modifier-characters '(#\:)
           :positions (list (position #\: (control-string directive)
                                      :start (modifiers-start directive))))))

;;; Signal an error if both modifiers have been given for such a directive.
(defmethod check-item-syntax progn
    ((client standard-client)(directive at-most-one-modifier-mixin) parent
     &optional group position)
  (declare (ignore parent group position))
  (when (and (colon-p directive) (at-sign-p directive))
    (error 'illegal-modifiers
           :client client
           :directive directive
           :modifier-characters '(#\@ #\:)
           :conflicting t
           :positions (list (position #\@ (control-string directive)
                                      :start (modifiers-start directive))
                            (position #\: (control-string directive)
                                      :start (modifiers-start directive))))))
