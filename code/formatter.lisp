(cl:in-package #:invistra)

(defun formatter (client control-string)
  (check-type control-string string)
  (let ((items (structure-items client (split-control-string client control-string))))
    `(lambda (*destination* &rest args)
       (with-arguments args
         ,@(compile-items client items)
         (pop-remaining-arguments)))))

(defun format-compiler-macro (client form destination control-string args)
  (declare (ignore form))
  `(format ,(incless:client-form client) ,destination
           ,(if (stringp control-string)
                (formatter client control-string)
                control-string)
           ,@args))
