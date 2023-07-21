(cl:in-package #:invistra)

(defun formatter (client control-string)
  (check-type control-string string)
  (let ((items (structure-items (split-control-string control-string) nil)))
    `(lambda (*destination* &rest args)
       (with-arguments args
         ,@(compile-items client items)
         (consume-remaining-arguments)))))

(defun format-compiler-macro (client form destination control-string args)
  (if (not (stringp control-string))
      form
      `(format ,(incless:client-form client) ,destination
               ,(formatter client control-string)
               ,@args)))
