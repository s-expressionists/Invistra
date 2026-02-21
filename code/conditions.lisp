(cl:in-package #:invistra)

;;; The base class of all format errors.

(define-condition format-error (error acclimation:condition)
  ((%reason :reader format-error-reason
            :initarg :reason
            :initform nil)))

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
                         :initarg :modifier-characters)))

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
         :reason :conflicting
         :regions (list* (cons (start directive) (end directive))
                         (loop for i from (modifiers-start directive)
                                 below (character-start directive)
                               when (member (char (control-string directive) i)
                                            modifier-characters)
                                 collect i))))

(defun signal-illegal-outer-escape-upward (client directive)
  (error 'illegal-modifiers
         :client client
         :control-string (control-string directive)
         :modifier-characters '(#\:)
         :reason :outer-escape-upward
         :regions (list (cons (start directive) (end directive))
                        (position #\: (control-string directive)
                                  :start (modifiers-start directive)
                                  :end (character-start directive)))))

(defun signal-modifier-and-parameter (client directive)
  (error 'illegal-modifiers
         :client client
         :control-string (control-string directive)
         :modifier-characters (loop for i from (modifiers-start directive)
                                      below (character-start directive)
                                    collect (char (control-string directive) i))
         :reason :conditional-with-parameter
         :regions (list* (cons (start directive) (end directive))
                         (loop for i from (modifiers-start directive)
                                 below (character-start directive)
                               collect i))))

(defun signal-illegal-default-clause (client directive)
  (error 'illegal-modifiers
         :client client
         :control-string (control-string directive)
         :modifier-characters #\:
         :reason :default-clause
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

(defun signal-illegal-clause-separator (client directive)
  (error 'illegal-directive
         :client client
         :control-string (control-string directive)
         :regions (list (cons (start directive) (end directive)))
         :reason :clause-separator))

(defun signal-illegal-fix-directive (client directive)
  (error 'illegal-directive
         :client client
         :control-string (control-string directive)
         :regions (list (cons (start directive) (end directive)))
         :reason :logical-block-fix))

(define-condition missing-directive (format-syntax-error)
  ((%directive-character :accessor directive-character
                         :initarg :directive-character)))

(defun signal-missing-directive (client directive character &optional reason)
  (error 'missing-directive
         :client client
         :control-string (control-string directive)
         :directive-character character
         :reason reason
         :regions (list (cons (start directive) (structured-end directive)))))

(defun check-clause-count (client directive minimum-count maximum-count)
  (let ((len (length (clauses directive))))
    (cond ((< len minimum-count)
           (error 'missing-directive
                  :client client
                  :control-string (control-string directive)
                  :directive-character #\;
                  :reason :clause-count
                  :regions (list (cons (start directive) (structured-end directive)))))
          ((< maximum-count len)
           (let* ((clause (aref (clauses directive) (1- maximum-count)))
                  (separator (aref clause (1- (length clause)))))
             (error 'illegal-directive
                    :client client
                    :control-string (control-string directive)
                    :reason :clause-count
                    :regions (list (cons (start separator) (end separator)))))))))

(define-condition incompatible-layout-requirements
    (format-syntax-error)
  ((%requirement1 :reader requirement1
                  :initarg :requirement1)
   (%requirement2 :reader requirement2
                  :initarg :requirement2)
   (%ancestor :reader ancestor
              :initarg :ancestor)))

(defun signal-incompatible-layout-requirements (client directive requirement1 requirement2 ancestor)
  (error 'incompatible-layout-requirements
         :client client
         :control-string (control-string directive)
         :regions (list (cons (start directive) (structured-end directive)))
         :requirement1 requirement1
         :requirement2 requirement2
         :ancestor ancestor))

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
  ((%what-argument :reader what-argument
                   :initarg :what-argument)
   (%max-arguments :reader max-arguments
                   :initarg :max-arguments)))

(define-condition invalid-destination (format-error)
  ((%destination :reader destination
                 :initarg :destination)))
