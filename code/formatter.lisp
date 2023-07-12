(cl:in-package #:invistra)

(defun formatter (client control-string)
  (check-type control-string string)
  (let ((items (structure-items (split-control-string control-string) nil)))
    `(lambda (destination &rest args)
       (let* ((*destination* (cond ((null destination)
                                    (make-string-output-stream))
                                   ((or (streamp destination)
                                        (and (stringp destination)
                                             (array-has-fill-pointer-p destination)))
                                    destination)
                                   ((eq destination t)
                                    *standard-output*)
                                   (t
                                    (error 'invalid-destination
                                           :destination destination))))
              (*arguments* (coerce args 'vector))
              (*next-argument-hook* nil)
              (*next-argument-pointer* 0)
              (*catch-tag* (list nil))
              (*escape-hook* (lambda ()
                               (throw *catch-tag* nil))))
         (catch *catch-tag*
           ,@(compile-items client items))
         (when (null destination)
           (get-output-stream-string *destination*))))))

(defun format-compiler-macro (client form destination control-string args)
  (if (not (stringp control-string))
      form
      `(funcall ,(formatter client control-string)
                ,destination ,@args)))
