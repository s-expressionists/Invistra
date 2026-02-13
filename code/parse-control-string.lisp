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
#+(or)(defun parse-parameter (string start end tilde-position)
  (when (= start end)
         (error 'end-of-control-string-error
                :control-string string
                :tilde-position tilde-position
                :why "expected a parameter"))
  (case (char string start)
    ((#\v #\V)
         ;; Indicates that the value is to be taken from the arguments.
         (values (make-instance 'argument-reference-parameter)
                 (1+ start)))
    (#\#
         ;; Indicates that the value is the remaining number of arguments
         (values (make-instance 'remaining-argument-count-parameter)
                 (1+ start)))
    (#\'
         (incf start)
         (when (= start end)
           (error 'end-of-control-string-error
                  :control-string string
                  :tilde-position tilde-position
                  :why "character expected"))
         (values (make-instance 'literal-parameter :value (char string start))
                 (1+ start)))
    ((#\+ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (multiple-value-bind (value position)
             (parse-integer string :start start :junk-allowed t)
           (when (null value)
             (error 'expected-integer-error
                    :control-string string
                    :tilde-position tilde-position
                    :index start))
           (values (make-instance 'literal-parameter :value value)
                   position)))
    (#\,
     (values (make-instance 'literal-parameter) start))
    (otherwise
     (values nil start))))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\V)) control-string start end position)
  (declare (ignore control-string start end))
  (values (make-instance 'argument-reference-parameter)
          (1+ position)))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\#)) control-string start end position)
  (declare (ignore control-string start end))
  (values (make-instance 'remaining-argument-count-parameter)
          (1+ position)))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\')) control-string start end position)
  (incf position)
  (if (< position end)
      (values (make-instance 'literal-parameter :value (char control-string position))
              (1+ position))
      (error 'end-of-control-string-error
             :control-string control-string
             :start start
             :index position
             :why "character expected")))

(defun parse-integer-parameter (string start end position)
  (multiple-value-bind (value end-position)
      (parse-integer string :start position :end end :junk-allowed t)
    (when (null value)
      (error 'expected-integer-error
             :control-string string
             :start start
             :index position))
    (values (make-instance 'literal-parameter :value value)
            end-position)))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\-)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\+)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\0)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\1)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\2)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\3)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\4)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\5)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\6)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\7)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\8)) control-string start end position)
  (parse-integer-parameter control-string start end position))

(defmethod parse-parameter
    ((client standard-client) (parameter-character (eql #\9)) control-string start end position)
  (parse-integer-parameter control-string start end position))

;;; Parse the parameters of a format directive.  STRING is the entire
;;; control string START is the position of the tilde character that
;;; starts the directive.  END is the length of the control string.
;;; Return the list of parameters and the position immediately beyond
;;; the last parameter.
(defun parse-parameters (client control-string start end position)
  (prog (parameter parameters)
   next
     (unless (< position end)
       (error 'end-of-control-string-error
              :control-string control-string
              :start start
              :index position
              :why "character expected"))
     (setf (values parameter position)
           (parse-parameter client (char-upcase (char control-string position)) control-string
                            start end position))
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

;;; Parse the modifiers of a format directive.  The colon and at-sign
;;; modifiers are optional and can appear in any order.  However, we
;;; do not allow more than one of each kind.  Return three values, a
;;; boolean indicating whether the colon modifier was found, a boolean
;;; indicating whether the at-sign modifier was found, and the first
;;; position beyond the modifiers in the string.
(defun parse-modifiers (string tilde-position end start)
  (let ((position (position-if-not (lambda (char)
                                     (or (eql char #\@)
                                         (eql char #\:)))
                                   string
                                   :start start)))
    (when (null position)
      (setf position end))
    (cond ((= position start)
           (values nil nil start))
          ((= position (1+ start))
           (if (eql (char string start) #\:)
               (values t nil (1+ start))
               (values nil t (1+ start))))
          ((= position (+ start 2))
           (if (eql (char string start)
                    (char string (1+ start)))
               (error 'two-identical-modifiers
                      :control-string string
                      :start tilde-position
                      :index start)
               (values t t (+ start 2))))
          (t
           (error 'more-than-two-modifiers
                  :control-string string
                  :start tilde-position
                  :index start)))))

;;; Parse a format directive.  The string is a format control string.
;;; The start position is the position of the tilde character that
;;; starts the directive.  Return the the character indicating the
;;; directive, a list of format parameters, two booleans indicating
;;; whether the colon and the at-sign modifiers were given, and the
;;; position in the string immediately beyond the character indicating
;;; the directive.
(defun parse-format-directive (client string start)
  (let ((end (length string)))
    (multiple-value-bind (parameters position1)
        (parse-parameters client string start end (1+ start))
      (multiple-value-bind (colon-p at-sign-p position2)
          (parse-modifiers string start end position1)
        (when (= position2 end)
          (error 'end-of-control-string-error
                 :control-string string
                 :start start
                 :why "expected a letter corresponding to a format directive"))
        ;; We need to handle the special cases of the ~Newline and ~/
        ;; directives, because those directive comprise characters
        ;; that follow the directive character itself.
        (let ((directive-character (char string position2))
              (suffix-start (incf position2)))
          (setf position2 (parse-directive-suffix client directive-character string suffix-start end))
          (values directive-character parameters colon-p at-sign-p suffix-start position2))))))
