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
    ((client standard-client) (parameter-character (eql #\V)) control-string position start)
  (declare (ignore control-string start))
  (values (make-instance 'argument-reference-parameter)
          (1+ position)))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\#)) control-string position start)
  (declare (ignore control-string start))
  (values (make-instance 'remaining-argument-count-parameter)
          (1+ position)))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\')) control-string position start)
  (incf position)
  (if (< position (length control-string))
      (values (make-instance 'literal-parameter :value (char control-string position))
              (1+ position))
      (error 'end-of-control-string
             :control-string control-string
             :start start
             :index position)))

(defun parse-integer-parameter (string position start)
  (multiple-value-bind (value end-position)
      (parse-integer string :start position :end (length string) :junk-allowed t)
    (when (null value)
      (error 'expected-integer-error
             :control-string string
             :start start
             :index position))
    (values (make-instance 'literal-parameter :value value)
            end-position)))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\-)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\+)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\0)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\1)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\2)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\3)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\4)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\5)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\6)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\7)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\8)) control-string position start)
  (parse-integer-parameter control-string position start))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\9)) control-string position start)
  (parse-integer-parameter control-string position start))

;;; Parse the parameters of a format directive.  STRING is the entire
;;; control string START is the position of the tilde character that
;;; starts the directive.  END is the length of the control string.
;;; Return the list of parameters and the position immediately beyond
;;; the last parameter.
(defun parse-parameters (client control-string position start)
  (prog ((parameter nil)
         (parameters nil)
         (end (length control-string)))
   next
     (unless (< position end)
       (error 'end-of-control-string
              :control-string control-string
              :start start
              :index position))
     (setf (values parameter position)
           (parse-parameter client (char-upcase (char control-string position)) control-string
                            position start))
     (cond (parameter
            (push parameter parameters)
            (when (and (< position end)
                       (char= #\, (char control-string position)))
              (incf position)
              (go next)))
           ((and (< position end)
                 (char= #\, (char control-string position)))
            (push (make-instance 'literal-parameter) parameters)
            (incf position)
            (go next))
           (parameters
            (push (make-instance 'literal-parameter) parameters)))
     (return (values (nreverse parameters) position))))

(defmethod parse-modifier
    ((client standard-client) (modifier-character (eql #\@)) control-string position start)
  (declare (ignore control-string start))
  (values :at-sign-p (1+ position)))

(defmethod parse-modifier
    ((client standard-client) (modifier-character (eql #\:)) control-string position start)
  (declare (ignore control-string start))
  (values :colon-p (1+ position)))

(defun parse-modifiers (client control-string position start)
  (prog ((end (length control-string))
         (modifiers nil)
         (modifier nil))
   next
     (unless (< position end)
       (error 'end-of-control-string
              :control-string control-string
              :start start))
     (setf (values modifier position)
           (parse-modifier client (char-upcase (char control-string position)) control-string
                           position start))
     (cond ((null modifier)
            (return (values modifiers position)))
           ((getf modifiers modifier)
            (error 'duplicate-modifiers
                   :control-string control-string
                   :start start
                   :index position))
           (t
            (setf (getf modifiers modifier) t)
            (go next)))))

;;; Parse a format directive.  The string is a format control string.
;;; The start position is the position of the tilde character that
;;; starts the directive.  Return the the character indicating the
;;; directive, a list of format parameters, two booleans indicating
;;; whether the colon and the at-sign modifiers were given, and the
;;; position in the string immediately beyond the character indicating
;;; the directive.
(defmethod parse-directive
    ((client standard-client) (character (eql #\~)) control-string position)
  (let ((start position)
        (end (length control-string))
        parameters
        modifiers)
    (incf position)
    (setf (values parameters position) (parse-parameters client control-string position start)
          (values modifiers position) (parse-modifiers client control-string position start))
    (when (= position end)
      (error 'end-of-control-string
             :control-string control-string
             :start start))
    (let ((directive-character (char-upcase (char control-string position)))
          (suffix-start (incf position)))
      (setf position (parse-suffix client directive-character control-string suffix-start start))
      (values (apply #'make-instance 'directive
                     :control-string control-string
                     :start start
                     :suffix-start suffix-start
                     :end position
                     :directive-character directive-character
                     :parameters parameters
                     modifiers)
              position))))
