(cl:in-package #:invistra)

;;; The base class of all format errors.

(define-condition format-error (error acclimation:condition)
  ())

(define-condition control-string-error (format-error)
  ((%client :reader client
            :initarg :client)
   (%control-string :accessor control-string
                    :initarg :control-string)
   (%regions :reader regions
             :initarg :regions)))

;;; This is the base class for all parse errors,
(define-condition format-parse-error (control-string-error)
  ())

(define-condition end-of-control-string (format-parse-error)
  ())

(defun signal-end-of-control-string (client directive)
  (error 'end-of-control-string
         :client client
         :control-string (control-string directive)
         :regions (list (cons (start directive) (end directive))
                        (1- (end directive)))))

(defun check-end-of-control-string (client directive position)
  (unless (< position (length (control-string directive)))
    (error 'end-of-control-string
           :client client
           :control-string (control-string directive)
           :regions (list (cons (start directive) (end directive))
                          (1- (end directive))))))

(define-condition expected-integer-error (format-parse-error)
  ())

(defun signal-expected-integer-error (client directive position)
  (error 'expected-integer-error
         :client client
         :control-string (control-string directive)
         :regions (list (cons (start directive) (end directive))
                        position)))

(define-condition duplicate-modifiers (format-parse-error)
  ())

(defun signal-duplicate-modifiers (client directive character)
  (error 'duplicate-modifiers
         :client client
         :control-string (control-string directive)
         :regions (list* (cons (start directive) (end directive))
                         (loop for i from (modifiers-start directive)
                                 below (or (character-start directive)
                                           (end directive)
                                           (length (control-string directive)))
                               when (char= character (char (control-string directive) i))
                                 collect i))))

;;; The base class of all syntax errors.  When one of these is
;;; signaled, we have correctly parsed the directive, so we know where
;;; in the control string it starts and ends.  
(define-condition format-syntax-error (control-string-error)
  ())

(define-condition unknown-directive-character (format-syntax-error)
  ((%directive-character :accessor directive-character
                         :initarg :directive-character)))

(defun signal-unknown-directive-character (client directive)
  (error 'unknown-directive-character
         :client client
         :control-string (control-string directive)
         :directive-character (directive-character directive)
         :regions (list (cons (start directive) (end directive))
                        (character-start directive))))

(define-condition illegal-modifiers (format-syntax-error)
  ((%modifier-characters :reader modifier-characters
                         :initarg :modifier-characters)
   (%conflicting :reader conflictingp
                 :initarg :conflicting
                 :initform nil)))

(defun signal-illegal-modifiers (client directive &rest modifier-characters)
  (error 'illegal-modifiers
         :client client
         :control-string (control-string directive)
         :modifier-characters modifier-characters
         :regions (list* (cons (start directive) (end directive))
                         (loop for i from (modifiers-start directive)
                                 below (character-start directive)
                               when (member (char (control-string directive) i)
                                            modifier-characters)
                                 collect i))))

(defun signal-conflicting-modifiers (client directive &rest modifier-characters)
  (error 'illegal-modifiers
         :client client
         :control-string (control-string directive)
         :modifier-characters modifier-characters
         :conflicting t
         :regions (list* (cons (start directive) (end directive))
                         (loop for i from (modifiers-start directive)
                                 below (character-start directive)
                               when (member (char (control-string directive) i)
                                            modifier-characters)
                                 collect i))))

(define-condition illegal-outer-modifier (illegal-modifiers)
  ())

(defun signal-illegal-outer-modifier (client directive)
  (error 'illegal-outer-modifier
         :client client
         :control-string (control-string directive)
         :modifier-characters '(#\:)
         :regions (list (cons (start directive) (end directive))
                        (position #\: (control-string directive)
                                  :start (modifiers-start directive)
                                  :end (character-start directive)))))

(define-condition illegal-conditional-modifier (illegal-modifiers)
  ())

(defun signal-illegal-conditional-modifier (client directive)
  (error 'illegal-conditional-modifier
         :client client
         :control-string (control-string directive)
         :modifier-characters (loop for i from (modifiers-start directive)
                                      below (character-start directive)
                                    collect (char (control-string directive) i))
         :regions (list* (cons (start directive) (end directive))
                         (loop for i from (modifiers-start directive)
                                 below (character-start directive)
                               collect i))))

(define-condition illegal-default-modifier (illegal-modifiers)
  ())

(defun signal-illegal-default-modifier (client directive)
  (error 'illegal-default-modifier
         :client client
         :control-string (control-string directive)
         :modifier-characters #\:
         :regions (list* (cons (start directive) (end directive))
                         (loop for i from (modifiers-start directive)
                                 below (character-start directive)
                               when (char= #\: (char (control-string directive) i))
                                 collect i))))

(define-condition illegal-parameter (format-syntax-error)
  ((%maximum-count :reader maximum-count
                   :initarg :maximum-count)))

(defun signal-illegal-parameter (client directive parameter maximum-count)
  (error 'illegal-parameter
         :client client
         :control-string (control-string directive)
         :maximum-count maximum-count
         :regions (list (if (= (start parameter) (end parameter))
                            (start parameter)
                            (cons (start parameter) (end parameter))))))

(define-condition parameter-type-error (type-error format-syntax-error)
  ()
  (:report (lambda (condition stream)
	           (acclimation:report-condition condition stream
                                           (acclimation:language acclimation:*locale*)))))

(defun check-parameter-type (client directive parameter)
  (with-accessors ((parameter-value parameter-value)
                   (parameter-type parameter-type))
      parameter
    (unless (typep parameter-value parameter-type)
      (error 'parameter-type-error
             :client client
             :control-string (control-string directive)
             :regions (list (cons (start parameter) (end parameter)))
             :expected-type parameter-type
             :datum parameter-value))))

(define-condition no-such-package (format-syntax-error)
  ((%package-name :reader no-such-package-package-name
                  :initarg :package-name)))

(defun signal-no-such-package (client directive name start end)
  (error 'no-such-package
         :client client
         :control-string (control-string directive)
         :regions (list (cons start end))
         :package-name name))

(define-condition no-such-symbol (format-syntax-error)
  ((%symbol-name :reader no-such-symbol-symbol-name
                 :initarg :symbol-name)))

(defun signal-no-such-symbol (client directive name start end)
  (error 'no-such-symbol
         :client client
         :control-string (control-string directive)
         :regions (list (cons start end))
         :symbol-name name))

(define-condition symbol-not-external (format-syntax-error)
  ((%symbol :reader symbol-not-external-symbol
            :initarg :symbol)))

(defun signal-symbol-not-external (client directive symbol start end)
  (error 'symbol-not-external
         :client client
         :control-string (control-string directive)
         :regions (list (cons start end))
         :symbol symbol))

(define-condition illegal-directive (format-syntax-error)
  ())

(define-condition illegal-clause-separator (illegal-directive)
  ())

(defun signal-illegal-clause-separator (client directive)
  (error 'illegal-clause-separator
         :client client
         :control-string (control-string directive)
         :regions (list (cons (start directive) (end directive)))))

(define-condition illegal-fix-directive (illegal-directive)
  ())

(defun signal-illegal-fix-directive (client directive)
  (error 'illegal-fix-directive
         :client client
         :control-string (control-string directive)
         :regions (list (cons (start directive) (end directive)))))

(define-condition global-layout-conflict (illegal-directive)
  ())

(defun signal-global-layout-conflict (client directive)
  (error 'global-layout-conflict
         :client client
         :control-string (control-string directive)
         :regions (list (cons (start directive) (structured-end directive)))))

(define-condition local-layout-conflict (illegal-directive)
  ())

(defun signal-local-layout-conflict (client directive)
  (error 'local-layout-conflict
         :client client
         :control-string (control-string directive)
         :regions (list (cons (start directive) (structured-end directive)))))

(define-condition excessive-clause-separators (illegal-directive)
  ())

(define-condition missing-directive (format-syntax-error)
  ((%directive-character :accessor directive-character
                         :initarg :directive-character)))

(defun signal-missing-directive (client directive character)
  (error 'missing-directive
         :client client
         :control-string (control-string directive)
         :directive-character character
         :regions (list (cons (start directive) (structured-end directive)))))

(define-condition missing-end-logical-block-or-end-justification (missing-directive)
  ())

(defun signal-missing-end-logical-block-or-end-justification (client directive)
  (error 'missing-end-logical-block-or-end-justification
         :client client
         :control-string (control-string directive)
         :directive-character #\>
         :regions (list (cons (start directive) (structured-end directive)))))

(define-condition missing-end-conditional (missing-directive)
  ())

(defun signal-missing-end-conditional (client directive)
  (error 'missing-end-conditional
         :client client
         :control-string (control-string directive)
         :directive-character #\]
         :regions (list (cons (start directive) (structured-end directive)))))

(define-condition missing-end-case-conversion (missing-directive)
  ())

(defun signal-missing-end-case-conversion (client directive)
  (error 'missing-end-case-conversion
         :client client
         :control-string (control-string directive)
         :directive-character #\)
         :regions (list (cons (start directive) (structured-end directive)))))

(define-condition missing-end-iteration (missing-directive)
  ())

(defun signal-missing-end-iteration (client directive)
  (error 'missing-end-iteration
         :client client
         :control-string (control-string directive)
         :directive-character #\}
         :regions (list (cons (start directive) (structured-end directive)))))

(define-condition missing-clause-separator (missing-directive)
  ())

(defun signal-missing-clause-separator (client directive)
  (error 'missing-clause-separator
         :client client
         :control-string (control-string directive)
         :directive-character #\;
         :regions (list (cons (start directive) (structured-end directive)))))

(defun check-clause-count (client directive minimum-count maximum-count)
  (let ((len (length (clauses directive))))
    (cond ((< len minimum-count)
           (error 'missing-clause-separator
                  :client client
                  :control-string (control-string directive)
                  :directive-character #\;
                  :regions (list (cons (start directive) (structured-end directive)))))
          ((< maximum-count len)
           (let* ((clause (aref (clauses directive) (1- maximum-count)))
                  (separator (aref clause (1- (length clause)))))
             (error 'excessive-clause-separators
                    :client client
                    :control-string (control-string directive)
                    :regions (list (cons (start separator) (end separator)))))))))

;;; Runtime conditions

(define-condition format-runtime-error (format-error)
  ())

(define-condition no-more-arguments (format-runtime-error)
  ;; maybe add the number of the argument that
  ;; was accessed?
  ())

(define-condition argument-type-error (format-runtime-error type-error)
  ()
  (:report (lambda (condition stream)
	           (acclimation:report-condition condition stream
                                           (acclimation:language acclimation:*locale*)))))

(define-condition go-to-out-of-bounds (format-runtime-error)
  ((%argument-position :reader argument-position
                       :initarg :argument-position)
   (%argument-count :reader argument-count
                    :initarg :argument-count)))

(define-condition invalid-destination (format-error)
  ((%destination :reader destination
                 :initarg :destination)))
