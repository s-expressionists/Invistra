(cl:in-package #:invistra)

(defstruct group
  end
  (clauses (list nil)))

(defun structure-items (client items)
  (loop with result = (list (make-group))
        for item in (reverse items)
        finally (return (coerce (car (group-clauses (car result))) 'vector))
        unless (stringp item)
          do (specialize-directive client (directive-character item)
                                   item (group-end (car result)))
             (cond ((structured-start-p item)
                    (setf (clauses item) (map 'vector
                                              (lambda (items)
                                                (coerce items 'vector))
                                              (group-clauses (car result))))
                    (pop result))
                   ((structured-end-p item)
                    (push (make-group :end item) result))
                   ((structured-separator-p item)
                    (push nil (group-clauses (car result)))))
        do (push item (car (group-clauses (car result))))))

(defun parse-control-string (client control-string)
  (loop with items = (structure-items client (split-control-string client control-string))
        with global = (make-instance 'layout)
        with local = (make-instance 'layout)
        for item across items
        finally (return items)
        do (check-item-syntax client item global local nil)))
