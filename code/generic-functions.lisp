(cl:in-package #:invistra)

(defclass standard-client () ())

;;; Return the name of a subclass to be used for a particular
;;; directive.  Each particular directive subclass must be accompanied
;;; by an eql-specialized method on this generic function.
(defgeneric specialize-directive (client character directive end-directive))

;;; For the default case, signal an error.
(defmethod specialize-directive (client character directive end-directive)
  (declare (ignore client character end-directive))
  (error 'unknown-directive-character
         :directive directive))

(defgeneric parameter-specifications (client directive)
  (:method (client directive)
    (declare (ignore client directive))
    nil))

;;; Check the syntax of a directive.
(defgeneric check-directive-syntax (client directive)
  (:method-combination progn :most-specific-last)
  (:method progn (client directive)
    (declare (ignore client directive))))

(defgeneric valid-nesting-p (client child parent)
  (:method (client child parent)
    (declare (ignore client child parent))
    t))

(defgeneric interpret-item (client item &optional parameters)
  (:method (client item &optional parameters)
    (declare (ignore client item parameters))))

(defgeneric compile-item (client item &optional parameters)
  (:method (client item &optional parameters)
    (declare (ignore client item parameters))))

(defgeneric parse-parameter (client character directive)
  (:method (client character directive)
    (declare (ignore client character directive))
    nil))

(defgeneric parse-modifier (client character directive)
  (:method (client character directive)
    (declare (ignore client character directive))
    nil))

(defgeneric parse-suffix (client character directive)
  (:method (client character directive)
    (declare (ignore client character directive))
    nil))

(defgeneric parse-directive (client character control-string position)
  (:method (client character control-string position)
    (declare (ignore client character control-string position))
    nil))

(defgeneric layout-requirements (item)
  (:method (item)
    (declare (ignore item))
    nil))

(defgeneric coerce-function-designator (client object)
  (:method (client object)
    (declare (ignore client))
    object))

(defgeneric print-key-sequence (client character stream)
  (:method (client character stream)
    (declare (ignore client stream))
    character))

(defgeneric make-argument-cursor (client object))

(defgeneric calculate-argument-position (position item)
  (:method (position item)
    (declare (ignore item))
    position))

(defgeneric whitespace-char-p (client ch))
