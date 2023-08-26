(cl:in-package #:invistra)

(defun compile-directive (client directive)
  (let ((specs (parameter-specs (class-name (class-of directive)))))
    (if specs
        `((destructuring-bind ,(mapcar #'car specs)
              (list ,@(mapcar #'run-time-value (given-parameters directive)))
            (declare (ignorable ,@(mapcar #'car specs)))
            ,@(compile-format-directive client directive)))
        (compile-format-directive client directive))))

(defun compile-item (client item)
  (if (stringp item)
      `((write-string ,item *destination*))
      (compile-directive client item)))

(defun compile-items (client items)
  (loop for item across items
        append (compile-directive client item)))

(defun compile-control-string (client control-string)
  (let ((items (structure-items client (split-control-string control-string))))
    `(progn ,@(loop for item across items
                    collect (compile-item client item)))))
