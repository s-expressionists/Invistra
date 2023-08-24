(cl:in-package #:invistra)

;;; Return the name of a subclass to be used for a particular
;;; directive.  Each particular directive subclass must be accompanied
;;; by an eql-specialized method on this generic function.
(defgeneric directive-subclass-name (directive-character directive end-directive))

;;; For the default case, signal an error.
(defmethod directive-subclass-name (directive-character directive end-directive)
  (error 'unknown-directive-character
         :directive directive))

;;; Given a name of a type of a directive, return a list of parameter
;;; specifiers for that type of directive.  Each type of directive
;;; should supply an eql specialized method for this generic function.
(defgeneric parameter-specs (directive-name)
  (:method (directive-name)
    (declare (ignore directive-name))
    nil))

;;; Check the syntax of a directive.
(defgeneric check-directive-syntax (directive)
  (:method-combination progn :most-specific-last))

;;; DIRECTIVE is an instance of a subclass of the DIRECTIVE class
;;; describing the directive.
(defgeneric interpret-format-directive (client directive))

;;; The directive compiler.
(defgeneric compile-format-directive (client directive))

(defgeneric parse-directive-suffix (directive-character control-string start end)
  (:method (directive-character control-string start end)
    (declare (ignore directive-character control-string end))
    start))

(defgeneric layout-requirements (item)
  (:method (item)
    (declare (ignore item))
    nil))

(defgeneric specialize-directive (client directive-character directive end-directive))
