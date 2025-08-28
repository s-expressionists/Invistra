(cl:in-package #:invistra)

(defun formatter (client control-string)
  (check-type control-string string)
  (let ((items (structure-items client (split-control-string control-string))))
    `(lambda (*destination* &rest args)
       (with-arguments args
         ,@(compile-items client items)
         (pop-remaining-arguments)))))

(defun format-compiler-macro (client form destination control-string args)
  `(format ,(trinsic:client-form client) ,destination
           ,(if (stringp control-string)
                (formatter client control-string)
                control-string)
           ,@args))
