(cl:in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing a control string

;;; Parse a parameter.  This function is called only if a parameter
;;; should be there, either because it is the first possible parameter
;;; position, and we have verified that the character at the start
;;; position is a member of "',vV#+-0123456789" or because the
;;; previous character was a comma, so there ought to be a parameter
;;; next.  If no parameter is found, signal an error.  Return two
;;; values, the parameter that was parsed and the position immediately
;;; beyond the parameter that was parsed.
(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\,)) control-string)
  (declare (ignore control-string))
  (setf (parameters directive)
        (nconc (parameters directive)
               (list (make-instance 'literal-parameter))))
  t)

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\V)) control-string)
  (declare (ignore control-string))
  (setf (parameters directive)
        (nconc (parameters directive)
               (list (make-instance 'argument-reference-parameter))))
  (incf (end directive))
  t)

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\#)) control-string)
  (declare (ignore control-string))
  (setf (parameters directive)
        (nconc (parameters directive)
               (list (make-instance 'remaining-argument-count-parameter))))
  (incf (end directive))
  t)

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\')) control-string)
  (declare (ignore))
  (incf (end directive))
  (cond ((< (end directive) (length control-string))
         (setf (parameters directive)
               (nconc (parameters directive)
                      (list (make-instance 'literal-parameter
                                           :value (char control-string (end directive))))))
         (incf (end directive))
         t)
        (t
         (error 'end-of-control-string
                :control-string control-string
                :start (start directive)
                :index (end directive)))))

(defun parse-integer-parameter (directive string)
  (multiple-value-bind (value end-position)
      (parse-integer string :start (end directive) :end (length string) :junk-allowed t)
    (when (null value)
      (error 'expected-integer-error
             :control-string string
             :start (start directive)
             :index (end directive)))
    (setf (parameters directive)
          (nconc (parameters directive)
                 (list (make-instance 'literal-parameter :value value)))
          (end directive) end-position)
    t))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\-)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\+)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\0)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\1)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\2)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\3)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\4)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\5)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\6)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\7)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\8)) control-string)
  (parse-integer-parameter directive control-string))

(defmethod parse-parameter
    ((client standard-client) directive (character (eql #\9)) control-string)
  (parse-integer-parameter directive control-string))

;;; Parse the parameters of a format directive.  STRING is the entire
;;; control string START is the position of the tilde character that
;;; starts the directive.  END is the length of the control string.
;;; Return the list of parameters and the position immediately beyond
;;; the last parameter.
(defun parse-parameters (client directive control-string)
  (prog ((required nil))
   next
     (when (>= (end directive) (length control-string))
       (error 'end-of-control-string
              :control-string control-string
              :start (start directive)
              :index (end directive)))
     (unless (parse-parameter client directive
                              (char-upcase (char control-string (end directive)))
                              control-string)
       (if required
           (setf (parameters directive)
                 (nconc (parameters directive)
                        (list (make-instance 'literal-parameter))))
           (return nil)))
     (when (and (< (end directive) (length control-string))
                (char= #\, (char control-string (end directive))))
       (incf (end directive))
       (setf required t)
       (go next))))

(defmethod parse-modifier
    ((client standard-client) directive (character (eql #\@)) control-string)
  (when (at-sign-p directive)
    (error 'duplicate-modifiers
           :control-string control-string
           :start (start directive)
           :index (end directive)))
  (setf (at-sign-p directive) t)
  (incf (end directive))
  t)

(defmethod parse-modifier
    ((client standard-client) directive (character (eql #\:)) control-string)
  (when (colon-p directive)
    (error 'duplicate-modifiers
           :control-string control-string
           :start (start directive)
           :index (end directive)))
  (setf (colon-p directive) t)
  (incf (end directive))
  t)

(defun parse-modifiers (client directive control-string)
  (tagbody
   next
     (unless (< (end directive) (length control-string))
       (error 'end-of-control-string
              :control-string control-string
              :start (start directive)))
     (when (parse-modifier client directive (char-upcase (char control-string (end directive)))
                           control-string)
       (go next))))

;;; Parse a format directive.  The string is a format control string.
;;; The start position is the position of the tilde character that
;;; starts the directive.  Return the the character indicating the
;;; directive, a list of format parameters, two booleans indicating
;;; whether the colon and the at-sign modifiers were given, and the
;;; position in the string immediately beyond the character indicating
;;; the directive.
(defmethod parse-directive
    ((client standard-client) (character (eql #\~)) control-string position)
  (let ((directive (make-instance 'directive
                                  :start position
                                  :end (1+ position)
                                  :control-string control-string
                                  :directive-character character)))
    (parse-parameters client directive control-string)
    (parse-modifiers client directive control-string)
    (unless (< (end directive) (length control-string))
      (error 'end-of-control-string
             :control-string control-string
             :start (start directive)))
    (setf (directive-character directive) (char-upcase (char control-string (end directive))))
    (incf (end directive))
    (setf (suffix-start directive) (end directive))
    (parse-suffix client directive (directive-character directive) control-string)
    directive))
