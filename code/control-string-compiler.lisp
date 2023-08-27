(cl:in-package #:invistra)

#+(or)(defun compile-item (client item)
  (if (stringp item)
      `((write-string ,item *destination*))
      (let ((specs (parameter-specs (class-name (class-of directive)))))
        (if specs
            `((destructuring-bind ,(mapcar #'car specs)
                  (list ,@(mapcar #'compile-parameter (parameters directive)))
                (declare (ignorable ,@(mapcar #'car specs)))
                ,@(compile-item client directive)))
            (compile-item client directive)))))

(defun compile-items (client items)
  (loop for item across items
        append (compile-item client item)))

(defun compile-control-string (client control-string)
  (let ((items (structure-items client (split-control-string control-string))))
    `(progn ,@(loop for item across items
                    collect (compile-item client item)))))
