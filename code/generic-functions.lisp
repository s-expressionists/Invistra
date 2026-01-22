(cl:in-package #:invistra)

(defclass standard-client () ())

;;; Return the name of a subclass to be used for a particular
;;; directive.  Each particular directive subclass must be accompanied
;;; by an eql-specialized method on this generic function.
(defgeneric specialize-directive (client directive-character directive end-directive))

;;; For the default case, signal an error.
(defmethod specialize-directive (client directive-character directive end-directive)
  (declare (ignore client directive-character end-directive))
  (error 'unknown-directive-character
         :directive directive))

(defgeneric parameter-specifications (client directive)
  (:method (client directive)
    (declare (ignore client directive))
    nil))

;;; Check the syntax of a directive.
(defgeneric check-directive-syntax (client directive)
  (:method-combination progn :most-specific-last))

(defgeneric interpret-item (client item &optional parameters)
  (:method (client item &optional parameters)
    (declare (ignore client item parameters))))

(defgeneric compile-item (client item &optional parameters)
  (:method (client item &optional parameters)
    (declare (ignore client item parameters))))

(defgeneric parse-directive-suffix (client directive-character control-string start end)
  (:method (client directive-character control-string start end)
    (declare (ignore client directive-character control-string end))
    start))

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
