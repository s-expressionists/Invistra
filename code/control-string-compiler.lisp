(cl:in-package #:invistra)

(defun compile-parameter-value (directive parameter-spec)
  (let* ((parameter-name (car parameter-spec))
         (compile-time-value (funcall parameter-name directive)))
    (cond ((null compile-time-value)
           ;; The parameter was not given at all, in the format control
           ;; string, neither as a constant value, nor as a value to
           ;; be acquired at runtime (# or V).  We must use a default
           ;; value of it has any.
           (getf (cdr parameter-spec) :default-value))
          ((eq compile-time-value :argument-reference)
           ;; The parameter was given the explicit value V in the
           ;; format control string, meaning we use the next argument
           ;; to acquire the value of the parameter.  We must generate
           ;; code to test that there are more arguments, to consume
           ;; the next one, and to check that the type of the argument
           ;; acquired is correct.
           `(progn (when (>= *next-argument-pointer*
                             (length *arguments*))
                     (error 'no-more-arguments))
                   (let ((argument (aref *arguments*
                                         *next-argument-pointer*)))
                     (incf *next-argument-pointer*)
                     (cond ((null argument)
                            ,(getf (cdr parameter-spec) :default-value))
                           (t
                            (unless (typep argument ',(getf (cdr parameter-spec) :type))
                              (error 'argument-type-error
                                     :expected-type
                                     ',(getf (cdr parameter-spec) :type)
                                     :datum
                                     argument))
                            argument)))))
          ((eq compile-time-value :remaining-argument-count)
           ;; The parameter was given the explicit value # in the
           ;; format control string, meaning we use the number of
           ;; remaining arguments as the value of the parameter.
           `(let ((number-of-remaining-arguments
                   (- (length *arguments*) *next-argument-pointer*)))
              (unless (typep number-of-remaining-arguments
                             ',(getf (cdr parameter-spec) :type))
                (error 'argument-type-error
                       :expected-type
                       ',(getf (cdr parameter-spec) :type)
                       :datum
                       number-of-remaining-arguments))
              number-of-remaining-arguments))
          (t
           ;; The parameter was given an explicit value (number or
           ;; character) in the format control string, and this is the
           ;; value we want.
           compile-time-value))))

(defun compile-directive (client directive)
  (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
    (if parameter-specs
        `((let ,(loop for parameter-spec in parameter-specs
                      collect `(,(car parameter-spec)
                                ,(compile-parameter-value directive parameter-spec)))
            (declare (ignorable ,@(mapcar #'car parameter-specs)))
            ,@(compile-format-directive client directive)))
        (compile-format-directive client directive))))

(defun compile-item (client item)
  (if (stringp item)
      `((write-string ,item *destination*))
      (compile-directive client item)))

(defun compile-items (client items)
  (loop for item across items
        append (compile-item client item)))

(defun compile-control-string (client control-string)
  (let ((items (structure-items (split-control-string control-string) nil)))
    `(progn ,@(loop for item across items
                    collect (compile-item client item)))))
