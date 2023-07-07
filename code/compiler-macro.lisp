(cl:in-package #:invistra)

(defun format-compiler-macro (client form destination control-string args)
  (if (not (stringp control-string))
      form
      (let ((items (structure-items (split-control-string control-string) nil)))
        `(flet ((format-aux (stream)
                  ;; Execute the items in a new environment.
                  (let ((*destination* stream)
                        (*arguments* (vector ,@args))
                        ;; We are at the beginning of the argument vector.
                        (*next-argument-pointer* 0)
                        ;; Any unique object will do.
                        (*catch-tag* (list nil)))
                    (catch *catch-tag*
                      ,@(compile-items client items)))))
           (let ((destination ,destination))
             (cond ((or (streamp destination)
                        (and (stringp destination)
                             (array-has-fill-pointer-p destination)))
                    (format-aux destination))
                   ((null destination)
                    (with-output-to-string (stream)
                      (format-aux stream)))
                   ((eq destination t)
                    (format-aux *standard-output*))
                   (t
                    (error 'invalid-destination
                           :destination destination))))))))
