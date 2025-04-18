(cl:in-package #:invistra)

(defun compile-items (client items)
  (loop for item across items
        append (compile-item client item)))

(defun compile-control-string (client control-string)
  (let ((items (structure-items client (split-control-string client control-string))))
    `(progn ,@(loop for item across items
                    collect (compile-item client item)))))
