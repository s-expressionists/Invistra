(cl:in-package #:invistra)

;;; The base class of all format errors.

(define-condition format-error (error acclimation:condition) ())

(define-condition directive-error (format-error)
  ((%client :reader client
            :initarg :client)
   (%directive :reader directive
               :initarg :directive)
   (%positions :reader positions
               :initarg :positions
               :initform nil)))

;;; This is the base class for all parse errors,
(define-condition directive-parse-error (directive-error)
  ())

(define-condition end-of-control-string (directive-parse-error)
  ())

(define-condition expected-integer-error (directive-parse-error)
  ())

#+(or)(define-condition expected-parameter-start (found-something-else-error)
  ())

(define-condition duplicate-modifiers (directive-parse-error)
  ())

#+(or)(define-condition unknown-format-directive (found-something-else-error)
  ())

;;; The base class of all syntax errors.  When one of these is
;;; signaled, we have correctly parsed the directive, so we know where
;;; in the control string it starts and ends.  
(define-condition directive-syntax-error (directive-error)
  ())

(define-condition unknown-directive-character (directive-syntax-error)
  ())

(define-condition illegal-modifiers (directive-syntax-error)
  ((%modifier-characters :reader modifier-characters
                         :initarg :modifier-characters)
   (%conflicting :reader conflictingp
                 :initarg :conflicting
                 :initform nil)))

(define-condition too-many-parameters (directive-syntax-error)
  ((%at-most-how-many :reader at-most-how-many
                      :initarg :at-most-how-many)
   (%how-many-found :reader how-many-found
                    :initarg :how-many-found)))

(define-condition parameter-type-error (type-error directive-syntax-error)
  ()
  (:report (lambda (condition stream)
	           (acclimation:report-condition condition stream
                                           (acclimation:language acclimation:*locale*)))))

(define-condition no-such-package (directive-syntax-error)
  ((%package-name :reader no-such-package-package-name
                  :initarg :package-name)))

(define-condition no-such-symbol (directive-syntax-error)
  ((%symbol-name :reader no-such-symbol-symbol-name
                 :initarg :symbol-name)))

(define-condition symbol-not-external (directive-syntax-error)
  ((%symbol :reader symbol-not-external-symbol
            :initarg :symbol)))

(define-condition modifier-and-parameter (directive-syntax-error)
  ())

(define-condition illegal-clause-separators (directive-syntax-error)
  ())

(define-condition clause-separator-with-colon-modifier-not-allowed
    (directive-syntax-error)
  ())

(define-condition parameter-omitted (directive-syntax-error)
  ((%parameter1 :initarg :parameter1 :reader parameter1)
   (%parameter2 :initarg :parameter2 :reader parameter2)))

(define-condition unmatched-directive (directive-syntax-error)
  ())

(define-condition illegal-clause-separator (directive-syntax-error)
  ())

(define-condition illegal-outer-escape-upward (directive-syntax-error)
  ())

(define-condition illegal-fix-directive (directive-syntax-error)
  ())

(define-condition invalid-clause-count (directive-syntax-error)
  ((%minimum-count :reader minimum-count
                   :initarg :minimum-count
                   :initform 1)
   (%maximum-count :reader maximum-count
                   :initarg :maximum-count
                   :initform 1)
   (%actual-count :reader actual-count
                  :initarg :actual-count)))

(define-condition incompatible-layout-requirements
    (directive-syntax-error)
  ((%requirement1 :reader requirement1
                  :initarg :requirement1)
   (%requirement2 :reader requirement2
                  :initarg :requirement2)
   (%ancestor :reader ancestor
              :initarg :ancestor)))

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
