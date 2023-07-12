(cl:in-package #:invistra)

(defgeneric control-string (directive))

(defgeneric start (directive))

(defgeneric end (directive))

(defgeneric directive-character (directive))

(defgeneric given-parameters (directive))

(defgeneric colonp (directive))

(defgeneric at-signp (directive))

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

;;; How we represent a directive.  It may seem wasteful to allocate
;;; a class instance for each directive, but most format directives
;;; are handled at compile time anyway.
(defclass directive ()
  (;; the entire control string in which this directive was found
   (%control-string :initarg :control-string :reader control-string)
   ;; the position in the control string of the ~ character.
   (%start :initarg :start :reader start)
   ;; the first position beyond the directive character
   (%end :initarg :end :reader end)
   ;; The directive character used.
   (%directive-character :initarg :directive-character :reader directive-character)
   ;; a list of parameters, each one is either an integer or a character
   (%given-parameters :initarg :given-parameters :reader given-parameters)
   ;; true if and only if the `:' modifier was given
   (%colonp :initarg :colonp :reader colonp)
   ;; true if and only if the `@' modifier was given
   (%at-signp :initarg :at-signp :reader at-signp)))

;;; The base class of all directives that take a maximum number of
;;; named parameters.  Those are all the directives except the
;;; call-function directive.
(defclass named-parameters-directive (directive) ())

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

;;; Specialize a directive according to a particular directive
;;; character.
(defun specialize-directive (directive end-directive)
  (change-class
   directive
   (directive-subclass-name (directive-character directive) directive end-directive)))

;;; A macro that helps us define directives. It takes a directive
;;; character, a directive name (to be used for the class) and a body
;;; in the form of a list of parameter specifications.  Each parameter
;;; specification is a list where the first element is the name of the
;;; parameter, and the remaining elemnts are keyword/value pairs.
;;; Currently, the only keywords allowed are :type and
;;; :default-value.
(defmacro define-directive (character name end-name superclasses parameters &body slots)
  `(progn
     (defmethod directive-subclass-name
         ((char (eql ,(char-upcase character))) directive
          ,(if end-name `(end-directive ,end-name) 'end-directive))
       (declare (ignore directive))
       ',name)

     ,(when end-name
        `(defmethod directive-subclass-name
             ((char (eql ,(char-upcase character))) directive end-directive)
           (declare (ignore end-directive))
           (error 'unmatched-directive
                  :directive directive
                  :control-string (control-string directive)
                  :tilde-position (start directive))))

     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defmethod parameter-specs ((directive-name (eql ',name)))
         ',(loop for parameter in parameters
                 collect (if (getf (cdr parameter) :default-value)
                             parameter
                             (cons (car parameter)
                                   (list* :default-value nil (cdr parameter)))))))

     (defclass ,name ,superclasses
       (,@(loop for parameter in parameters
                collect `(,(car parameter)
                           :initform nil
                           :reader
                           ,(car parameter)))
          ,@slots))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking syntax, interpreting, and compiling directives.

(defmethod check-directive-syntax progn (directive)
  (with-accessors ((given-parameters given-parameters))
    directive
    (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
      ;; When a parameter was explicitly given, check that
      ;; what was given does not have an incompatible type
      ;; with respect to the default value of the corresponding
      ;; slot, and assign the explicitly given value to
      ;; the slot.
      (let ((parameter-number 1))
        (mapc (lambda (parameter-spec parameter-value)
                (unless (or (eq parameter-value '|#|)
                            (eq parameter-value 'V))
                  (unless
                      (or
                       ;; Either a parameter was not supplied, but it has a
                       ;; default value
                       (and (null parameter-value)
                            (not (null (getf (cdr parameter-spec) :default-value))))
                       ;; Or else it was supplied, and it is of the right type.
                       (typep parameter-value (getf (cdr parameter-spec) :type)))
                    (error 'parameter-type-error
                           :expected-type
                           (getf (cdr parameter-spec) :type)
                           :datum parameter-value)))
                (setf (slot-value directive (car parameter-spec))
                      parameter-value)
                (incf parameter-number))
        parameter-specs
        given-parameters)))))

(defmethod check-directive-syntax progn ((directive named-parameters-directive))
  (with-accessors ((given-parameters given-parameters))
    directive
    (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
      ;; Check that the number of parameters given is no more than
      ;; what this type of directive allows.
      (when (> (length given-parameters) (length parameter-specs))
        (error 'too-many-parameters
               :directive directive
               :at-most-how-many (length parameter-specs)
               :how-many-found (length given-parameters))))))
