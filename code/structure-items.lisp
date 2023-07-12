(cl:in-package #:invistra)

(defstruct group
  end
  (clauses (list nil)))

(defun structure-items (items end)
  (loop with result = (list (make-group))
        for item in (reverse items)
        finally (return (coerce (car (group-clauses (car result))) 'vector))
        unless (stringp item)
          do (specialize-directive item (group-end (car result)))
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
             (check-directive-syntax item)
        do (push item (car (group-clauses (car result))))))

#|(defun structure-items (items end)
  (loop with result = '()
        with first = (car items)
        do (cond ((null items)
                  (if (null end)
                      (return (values (coerce (nreverse result) 'vector)
                                      '()))
                      (error 'unmatched-directive
                             :directive first
                             :control-string (control-string first)
                             :tilde-position (start first))))
                 ((stringp (car items))
                  (push (pop items) result))
                 ((find (directive-character (car items))
                        ">)}]")
                  (if (eql (directive-character (car items)) end)
                      (progn (push (pop items) result)
                             (return (values (coerce (nreverse result) 'vector)
                                             items)))
                      (error 'nesting-violation
                             :directive (car items))))
                 ((find (directive-character (car items))
                        "<({[")
                  (let ((item (pop items)))
                    (multiple-value-bind (nested-items rest)
                        (structure-items items
                                         (ecase (directive-character item)
                                           (#\< #\>) (#\( #\)) (#\{ #\}) (#\[ #\])))
                      (setf items rest)
                      (ecase (directive-character item)
                        (#\< (if (colonp (aref nested-items (1- (length nested-items))))
                                 (change-class item 'logical-block-directive
                                               :items nested-items)
                                 (change-class item 'justification-directive
                                               :items nested-items)))
                        (#\( (change-class item 'case-conversion-directive
                                           :items nested-items))
                        (#\{ (change-class item 'iteration-directive
                                           :items nested-items))
                        (#\[ (change-class item 'conditional-directive
                                           :items nested-items)))
                      (check-directive-syntax item)
                      (push item result))))
                 (t
                  (let ((item (pop items)))
                    (specialize-directive item)
                    (check-directive-syntax item)
                    (push item result))))))
|#
