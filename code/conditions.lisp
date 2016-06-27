(cl:in-package #:sicl-format)

;;; The base class of all format errors.

(define-condition format-error (error acclimation:condition) ())

;;; This is the base class for all parse errors, i.e. error found
;;; before we were able to construct a directive object at all.  All
;;; we know is where the directive start (tilde-position), and the
;;; control string.
(define-condition directive-parse-error (format-error)
  ((%control-string :initarg :control-string :reader control-string)
   (%tilde-position :initarg :tilde-position :reader tilde-position)))

(define-condition end-of-control-string-error (directive-parse-error)
  ((%why :initarg :why :reader why)))

(define-condition found-something-else-error (directive-parse-error)
  ((%index :initarg :index :reader index)))

(define-condition expected-integer-error (found-something-else-error)
  ())

(define-condition expected-parameter-start (found-something-else-error)
  ())

(define-condition two-identical-modifiers (found-something-else-error)
  ())

(define-condition more-than-two-modifiers (found-something-else-error)
  ())

(define-condition unknown-format-directive (found-something-else-error)
  ())

;;; The base class of all syntax errors.  When one of these is
;;; signaled, we have correctly parsed the directive, so we know where
;;; in the control string it starts and ends.  
(define-condition directive-syntax-error (format-error)
  ((%directive :initarg :directive :reader directive)))

(define-condition unknown-directive-character (directive-syntax-error)
  ())

(define-condition directive-takes-no-modifiers (directive-syntax-error)
  ())

(define-condition directive-takes-only-colon (directive-syntax-error)
  ())

;;; FIXME, report the index
(define-condition directive-takes-only-at-sign (directive-syntax-error)
  ())

(define-condition directive-takes-at-most-one-modifier (directive-syntax-error)
  ())

(define-condition too-many-parameters (directive-syntax-error)
  ((%at-most-how-many :initarg :at-most-how-many :reader at-most-how-many)
   (%how-many-found :initarg :how-many-found :reader how-many-found)))

(define-condition parameter-type-error (directive-syntax-error type-error)
  ())

;;; Runtime conditions

(define-condition format-runtime-error (format-error) ())

(define-condition no-more-arguments (format-runtime-error)
  ;; maybe add the number of the argument that
  ;; was accessed?
  ())

(define-condition argument-type-error (format-runtime-error type-error)
  ())

(define-condition too-many-package-markers (directive-syntax-error)
  ())

(define-condition no-such-package (directive-syntax-error)
  ())

(define-condition no-such-symbol (directive-syntax-error)
  ())

(define-condition symbol-not-external (directive-syntax-error)
  ())

(define-condition go-to-out-of-bounds (format-runtime-error)
  ((%what-argument :initarg :what-argument :reader what-argument)
   (%max-arguments :initarg :max-arguments :reader max-arguments)))

(define-condition modifier-and-parameter (directive-syntax-error)
  ())

(define-condition illegal-clause-separators (directive-syntax-error)
  ())

(define-condition clause-separator-with-colon-modifier-not-allowed
    (directive-syntax-error)
  ())

(define-condition at-least-one-item-required (directive-syntax-error)
  ())

(define-condition colon-modifier-requires-two-clauses
    (directive-syntax-error)
  ())

(define-condition at-sign-modifier-requires-one-clause
    (directive-syntax-error)
  ())

(define-condition parameter-omitted (directive-syntax-error)
  ((%parameter1 :initarg :parameter1 :reader parameter1)
   (%parameter2 :initarg :parameter2 :reader parameter2)))

(define-condition unmatched-directive (directive-syntax-error)
  ())

(define-condition nesting-violation (directive-syntax-error)
  ())

(define-condition invalid-destination (format-error)
  ((%destination :initarg :destination :reader destination)))
