(cl:in-package #:invistra)

(defclass layout ()
  ((%logical-block :accessor logical-block-p
                   :initarg :logical-block
                   :initform nil)
   (%justification :accessor justificationp
                   :initarg :justification
                   :initform nil)
   (%dynamic :accessor dynamicp
             :initarg :dynamic
             :initform nil)))

(defmethod merge-layout ((client standard-client) directive global local
                         &rest args
                         &key ((:logical-block logical-block-p) nil)
                              ((:justification justificationp) nil)
                              ((:dynamic dynamicp) nil))
  (declare (ignore args))
  (when (or (and (dynamicp global) logical-block-p)
            (and dynamicp (logical-block-p global)))
    (signal-global-layout-conflict client directive))
  (when logical-block-p
    (setf (logical-block-p global) t))
  (when justificationp
    (setf (justificationp global) t))
  (when dynamicp
    (setf (dynamicp global) t))
  (when (and (justificationp local) logical-block-p)
    (signal-local-layout-conflict  client directive))
  (make-instance 'layout
                 :logical-block (or (logical-block-p local) logical-block-p)
                 :justification (or (justificationp local) justificationp)
                 :dynamic (or (dynamicp local) dynamicp)))

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

(defmethod null-parameter-p ((parameter literal-parameter))
  (null (parameter-value parameter)))

;;; How we represent a directive.  It may seem wasteful to allocate
;;; a class instance for each directive, but most format directives
;;; are handled at compile time anyway.
(defclass directive ()
  (;; the entire control string in which this directive was found
   (%control-string :accessor control-string
                    :initarg :control-string)
   ;; the position in the control string of the ~ character.
   (%start :accessor start
           :initarg :start
           :initform nil)
   (%modifiers-start :accessor modifiers-start
                     :initarg :modifiers-start
                     :initform nil)
   (%character-start :accessor character-start
                     :initarg :character-start
                     :initform nil)
   (%suffix-start :accessor suffix-start
                  :initarg :suffix-start
                  :initform nil)
   ;; the first position beyond the directive character
   (%end :accessor end
         :initarg :end
         :initform nil)
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

(defmethod structured-end ((directive structured-directive-mixin))
  (or (loop with clauses = (clauses directive)
            for i from (1- (length clauses)) downto 0
            unless (zerop (length (aref clauses i)))
              return (structured-end (aref (aref clauses i) (1- (length (aref clauses i))))))
      (end directive)))

(defmethod calculate-argument-position (position (item directive))
  (reduce #'calculate-argument-position (parameters item) :initial-value position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking syntax, interpreting, and compiling directives.

(defmethod check-item-syntax progn
    ((client standard-client) (directive structured-directive-mixin) global-layout local-layout parent
     &optional group position)
  (declare (ignore parent group position))
  (loop for items across (clauses directive)
        for group from 0
        do (loop for item across items
                 for position from 0
                 do (check-item-syntax client item global-layout local-layout directive group position))))

(defmethod check-item-syntax progn
    ((client standard-client) (directive directive) global-layout local-layout parent &optional group position)
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
          do (signal-illegal-parameter client directive parameter count)
        else
          do (setf parameter (apply #'make-instance 'literal-parameter
                                    :allow-other-keys t spec))
        collect parameter into parameters
        when (typep parameter 'literal-parameter)
          do (with-accessors ((parameter-value parameter-value)
                              (parameter-default parameter-default))
                 parameter
               (unless parameter-value
                 (setf parameter-value parameter-default))
               (check-parameter-type client directive parameter))))

;;; Signal an error if a modifier has been given for such a directive.
(defmethod check-item-syntax progn
    ((client standard-client) (directive no-modifiers-mixin) global-layout local-layout parent &optional group position)
  (declare (ignore global-layout local-layout parent group position))
  (cond ((and (colon-p directive) (at-sign-p directive))
         (signal-illegal-modifiers client directive #\@ #\:))
        ((colon-p directive)
         (signal-illegal-modifiers client directive #\:))
        ((at-sign-p directive)
         (signal-illegal-modifiers client directive #\@))))

;;; Signal an error if an at-sign has been given for such a directive.
(defmethod check-item-syntax progn
    ((client standard-client) (directive only-colon-mixin) global-layout local-layout parent &optional group position)
  (declare (ignore global-layout local-layout parent group position))
  (when (at-sign-p directive)
    (signal-illegal-modifiers client directive #\@)))

;;; Signal an error if a colon has been given for such a directive.
(defmethod check-item-syntax progn
    ((client standard-client) (directive only-at-sign-mixin) global-layout local-layout parent &optional group position)
  (declare (ignore global-layout local-layout parent group position))
  (when (colon-p directive)
    (signal-illegal-modifiers client directive #\:)))

;;; Signal an error if both modifiers have been given for such a directive.
(defmethod check-item-syntax progn
    ((client standard-client)(directive at-most-one-modifier-mixin) global-layout local-layout parent
     &optional group position)
  (declare (ignore global-layout local-layout parent group position))
  (when (and (colon-p directive) (at-sign-p directive))
    (signal-conflicting-modifiers client directive #\@ #\:)))
