;;;; 22.3.7 Control-flow operations

(in-package #:invistra)

;;; 22.3.7.1 ~* Go to

(defclass go-to-directive
    (directive at-most-one-modifier-mixin) nil)

(defmethod specialize-directive ((client client) (char (eql #\*)) directive)
  (change-class directive 'go-to-directive))

(defmethod parameter-specifications ((client client) (directive go-to-directive))
  '((:name n
     :type (or null (integer 0))
     :bind nil
     :default nil)))

(defmethod calculate-argument-position (position (item go-to-directive))
  (when (and position
             (typep (car (parameters item)) 'literal-parameter))
    (let ((n (parameter-value (car (parameters item)))))
      (cond ((colon-p item)
             (- position (or n 1)))
            ((at-sign-p item)
             (or n 0))
            (t
             (+ position (or n 1)))))))

(defmethod interpret-item
    ((client client) (directive go-to-directive) &optional parameters)
  (let ((n (car parameters)))
    (cond ((colon-p directive)
           ;; Back up in the list of arguments.
           ;; The default value for the parameter is 1.
           (go-to-argument (- (or n 1))))
          ((at-sign-p directive)
           ;; Go to an absolute argument number.
           ;; The default value for the parameter is 0.
           (go-to-argument (or n 0) t))
          (t
           ;; Skip the next arguments.
           ;; The default value for the parameter is 1.
           (go-to-argument (or n 1))))))

(defmethod compile-item
    ((client client) (directive go-to-directive) &optional parameters)
  (let ((n (car parameters)))
    (cond ((colon-p directive)
           ;; Back up in the list of arguments.
           ;; The default value for the parameter is 1.
           (go-to-argument-forms (typecase n
                                   (null -1)
                                   (number (- n))
                                   (t `(- (or ,n 1))))))
          ((at-sign-p directive)
           ;; Go to an absolute argument number.
           ;; The default value for the parameter is 0.
           (go-to-argument-forms (typecase n
                                   (null 0)
                                   (number n)
                                   (t `(or ,n 0)))
                                 t))
          (t
           ;; Skip the next arguments.
           ;; The default value for the parameter is 1.
           (go-to-argument-forms (typecase n
                                   (null 1)
                                   (number n)
                                   (t `(or ,n 1))))))))

;;; 22.3.7.3 ~] End of conditional expression

(defclass end-conditional-expression-directive
    (directive no-modifiers-mixin
     end-structured-directive-mixin)
  nil)

(defmethod specialize-directive ((client client) (char (eql #\])) directive)
  (change-class directive 'end-conditional-expression-directive))

;;; 22.3.7.2 ~[ Conditional expression

(defclass conditional-expression-directive
    (directive separated-directive-mixin
     at-most-one-modifier-mixin)
  ((%last-clause-is-default-p :initform nil
                              :accessor last-clause-is-default-p)))

(defmethod specialize-directive ((client client) (char (eql #\[)) directive)
  (change-class directive 'conditional-expression-directive))

(defmethod append-clause
    ((client client) (directive conditional-expression-directive) items
     (terminator end-conditional-expression-directive))
  (declare (ignore items)))

(defmethod append-clause
    ((client client) (directive conditional-expression-directive) items (terminator directive))
  (declare (ignore items))
  (when (at-sign-p terminator)
    (signal-illegal-modifiers client terminator #\@))
  (when (and (or (at-sign-p directive)
                 (colon-p directive))
             (colon-p terminator))
    (signal-illegal-default-modifier client terminator))
  (when (colon-p terminator)
    (if (last-clause-is-default-p directive)
        (signal-illegal-default-modifier client (car (last (clauses directive))))
        (setf (last-clause-is-default-p directive) t))))

(defmethod parameter-specifications
    ((client client) (directive conditional-expression-directive))
  '((:name n
     :type (or null integer)
     :bind nil
     :default nil)))

(defmethod calculate-argument-position (position (directive conditional-expression-directive))
  (setf position (call-next-method))
  (when position
    (cond ((at-sign-p directive)
           ;; consequent must consume 1 argument.
           (when (eql (1+ position)
                      (calculate-argument-position position (first (clauses directive))))
             (1+ position)))
          ((colon-p directive)
           ;; alternative and consequent must consume the same number of arguments.
           (incf position)
           (let ((pos0 (calculate-argument-position position (first (clauses directive))))
                 (pos1 (calculate-argument-position position (second (clauses directive)))))
             (when (eql pos0 pos1)
               pos0)))
          ((typep (car (parameters directive)) 'argument-reference-parameter)
           nil)
          ((and (typep (car (parameters directive)) 'literal-parameter)
                (parameter-value (car (parameters directive))))
           (let ((value (parameter-value (car (parameters directive)))))
             (cond ((< -1 value (length (clauses directive)))
                    (calculate-argument-position position (nth value (clauses directive))))
                   ((last-clause-is-default-p directive)
                    (calculate-argument-position position
                                                 (nth (1- (length (clauses directive)))
                                                      (clauses directive))))
                   (t
                    position))))
          (t
           (when (typep (car (parameters directive)) 'literal-parameter)
             (incf position))
           (let ((new-position (if (last-clause-is-default-p directive)
                                   (calculate-argument-position position (first (clauses directive)))
                                   position)))
             (when (every (lambda (clause)
                            (eql new-position
                                 (calculate-argument-position position clause)))
                          (clauses directive))
                      new-position))))))

(defmethod check-item-syntax progn
    ((client client) (directive conditional-expression-directive) global-layout
     local-layout parent &optional group position)
  (declare (ignore global-layout local-layout parent group position))
  (with-accessors ((at-sign-p at-sign-p)
                   (colon-p colon-p)
                   (parameters parameters)
                   (start start)
                   (character-start character-start)
                   (clauses clauses))
      directive
    ;; Check that, if a parameter is given, then there are
    ;; no modifiers.
    (when (and (or (not (typep (car parameters) 'literal-parameter))
                   (parameter-value (car parameters)))
               (or colon-p at-sign-p))
      (signal-illegal-conditional-modifier client directive))
    ;; Check that, if a colon modifier was given, then
    ;; there should be a single clause separator (two clauses).
    (when colon-p
      (check-clause-count client directive 2 2))
    ;; Check that, if an at-sign modifier was given, then
    ;; there should be a no clause separators (a single clause).
    (when at-sign-p
      (check-clause-count client directive 1 1))))

(defmethod interpret-item
    ((client client) (directive conditional-expression-directive) &optional parameters)
  (with-accessors ((at-sign-p at-sign-p)
                   (colon-p colon-p)
                   (clauses clauses))
      directive
    (cond (at-sign-p
           (when (pop-argument)
             (go-to-argument -1)
             (interpret-item client (first clauses))))
          (colon-p
           (interpret-item client (if (pop-argument) (second clauses) (first clauses))))
          (t
           ;; If a parameter was given, use it,
           ;; else use the next argument.
           (let ((n (or (car parameters) (pop-argument 'integer))))
             (cond ((< -1 n (length clauses))
                    (interpret-item client
                                    (nth n clauses)))
                   ((last-clause-is-default-p directive)
                    (interpret-item client
                                    (car (last clauses))))))))))

(defmethod compile-item
    ((client client) (directive conditional-expression-directive) &optional parameters)
  (with-accessors ((at-sign-p at-sign-p)
                   (colon-p colon-p)
                   (clauses clauses))
      directive
    (cond (at-sign-p
           `((when ,(pop-argument-form)
               ,@(go-to-argument-forms -1)
               ,@(compile-item client (first clauses)))))
          (colon-p
           (let ((arg-form (pop-argument-form)))
             (with-argument-branching
               `((cond (,arg-form
                        ,@(compile-item client (second clauses)))
                       (t
                        ,@(progn
                            (reset-branching)
                            (compile-item client (first clauses)))))))))
          (t
           (let ((n (car parameters)))
             (cond ((not (numberp n))
                    `((case ,(cond ((null n)
                                    (pop-argument-form 'integer))
                                   ((typep (car (parameters directive))
                                           'remaining-argument-count-parameter)
                                    n)
                                   (t
                                    `(or ,n ,(pop-argument-form 'integer))))
                        ,@(with-argument-branching
                            (loop for i from 0
                                  for j downfrom (1- (length clauses))
                                  for clause in clauses
                                  do (reset-branching)
                                  collect `(,(if (and (zerop j)
                                                      (last-clause-is-default-p directive))
                                                 'otherwise
                                                 i)
                                            ,@(compile-item client clause)))))))
                   ((< -1 n (length clauses))
                    (compile-item client (nth n clauses)))
                   ((last-clause-is-default-p directive)
                    (compile-item client (car (last clauses))))))))))

;;; 22.3.7.5 ~} End of iteration

(defclass end-iteration-directive
    (directive only-colon-mixin
     end-structured-directive-mixin)
  ())

(defmethod specialize-directive ((client client) (char (eql #\})) directive)
  (change-class directive 'end-iteration-directive))

;;; 22.3.7.4 ~{ Iteration

(defclass iteration-directive
    (directive separated-directive-mixin)
  ((%once :accessor oncep
          :initform nil)))

(defmethod specialize-directive ((client client) (char (eql #\{)) directive)
  (change-class directive 'iteration-directive))

(defmethod append-clause
    ((client client) (directive iteration-directive) items (terminator end-iteration-directive))
  (declare (ignore items)))

(defmethod parameter-specifications
            ((client client) (directive iteration-directive))
  '((:name n
     :bind nil
     :type (or null (integer 0))
     :default nil)))

(defmethod calculate-argument-position (position (directive iteration-directive))
  (unless (at-sign-p directive)
    (+ position
       (if (empty-clause-p (first (clauses directive))) 2 1))))

(defun format-single-recursive-iteration (client colon-p oncep iteration-limit control arg)
  (if colon-p
      (with-arguments (client arg)
        (if (stringp control)
            (loop with items = (parse-control-string client control)
                  for index from 0
                  while (or (null iteration-limit)
                            (< index iteration-limit))
                  when (or (not oncep) (plusp index))
                    do (funcall *inner-exit-if-exhausted*)
                  do (with-arguments (client (pop-argument) :outer t)
                       (interpret-items client items)))
            (loop for index from 0
                  while (or (null iteration-limit)
                            (< index iteration-limit))
                  when (or (not oncep) (plusp index))
                    do (funcall *inner-exit-if-exhausted*)
                  do (apply control *format-output* (pop-argument 'list)))))
      (if (stringp control)
          (with-arguments (client arg)
            (loop with items = (parse-control-string client control)
                  for index from 0
                  while (or (null iteration-limit)
                            (< index iteration-limit))
                  when (or (not oncep) (plusp index))
                    do (funcall *inner-exit-if-exhausted*)
                  do (interpret-items client items)))
          (loop for args = arg
                  then (apply control *format-output* args)
                for index from 0
                while (and (or (null iteration-limit)
                               (< index iteration-limit))
                           (or args
                               (and oncep (zerop index))))))))

(defun format-remaining-recursive-iteration (client colon-p oncep iteration-limit control)
  (if colon-p
      (with-remaining-arguments ()
        (if (stringp control)
            (loop with items = (parse-control-string client control)
                  for index from 0
                  while (or (null iteration-limit)
                            (< index iteration-limit))
                  when (or (not oncep) (plusp index))
                    do (funcall *inner-exit-if-exhausted*)
                  do (with-arguments (client (pop-argument) :outer t)
                       (interpret-items client items)))
            (loop for index from 0
                  while (or (null iteration-limit)
                            (< index iteration-limit))
                  when (or (not oncep) (plusp index))
                    do (funcall *inner-exit-if-exhausted*)
                  do (apply control *format-output* (pop-argument 'list)))))
      (if (stringp control)
          (with-remaining-arguments ()
            (loop with items = (parse-control-string client control)
                  for index from 0
                  while (or (null iteration-limit)
                            (< index iteration-limit))
                  when (or (not oncep) (plusp index))
                do (funcall *inner-exit-if-exhausted*)
                  do (interpret-items client items)))
          (loop for args = (pop-remaining-arguments)
                  then (apply control *format-output* args)
                for index from 0
                finally (go-to-argument (- (length args)))
                while (and (or (null iteration-limit)
                               (< index iteration-limit))
                           (or args
                               (and oncep (zerop index))))))))

(defmethod interpret-item
    ((client client) (directive iteration-directive) &optional parameters)
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let* ((colon-p (colon-p directive))
         (at-sign-p (at-sign-p directive))
         (iteration-limit (car parameters))
         (clause (first (clauses directive)))
         (oncep (colon-p (terminator clause))))
    (if (empty-clause-p clause)
        (if at-sign-p
            (format-remaining-recursive-iteration client colon-p oncep iteration-limit
                                                  (pop-argument 'format-control))
            (format-single-recursive-iteration client colon-p oncep iteration-limit
                                               (pop-argument 'format-control)
                                               (pop-argument)))
        (cond ((and colon-p at-sign-p)
               ;; The remaining arguments should be lists.  Each argument
               ;; is used in a different iteration.
               (with-remaining-arguments ()
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (with-arguments (client (pop-argument) :outer t)
                            (interpret-item client clause)))))
              (colon-p
               ;; We use one argument, and that should be a list of sublists.
               ;; Each sublist is used as arguments for one iteration.
               (with-arguments (client (pop-argument))
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (with-arguments (client (pop-argument) :outer t)
                            (interpret-item client clause)))))
              (at-sign-p
               (with-remaining-arguments ()
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (interpret-item client clause))))
              (t
               ;; no modifiers
               ;; We use one argument, and that should be a list.
               ;; The elements of that list are used by the iteration.
               (with-arguments (client (pop-argument))
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (interpret-item client clause))))))))

(defmethod compile-item
    ((client client) (directive iteration-directive) &optional parameters)
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let* ((colon-p (colon-p directive))
         (at-sign-p (at-sign-p directive))
         (iteration-limit (car parameters))
         (bind-iteration-limit-p (not (or (constantp iteration-limit)
                                          (symbolp iteration-limit))))
         (clause (first (clauses directive)))
         (oncep (colon-p (terminator clause))))
    (if (empty-clause-p clause)
        (if at-sign-p
            `((format-remaining-recursive-iteration ,(trinsic:client-form client) ,colon-p
                                                    ,oncep ,iteration-limit
                                                    ,(pop-argument-form 'format-control)))
            `((format-single-recursive-iteration ,(trinsic:client-form client) ,colon-p
                                                 ,oncep ,iteration-limit
                                                 ,(pop-argument-form 'format-control)
                                                 ,(pop-argument-form))))
        (flet ((expand-loop (&aux (compiled-items (compile-item client clause)))
                 (when compiled-items
                   (with-unique-names (index limit)
                     `((loop ,@(when bind-iteration-limit-p
                                 `(with ,limit = ,iteration-limit))
                             for ,index from 0
                             while (or (null ,(if bind-iteration-limit-p limit iteration-limit))
                                       (< ,index ,(if bind-iteration-limit-p
                                                      limit
                                                      iteration-limit)))
                             ,@(if oncep
                                   `(when (plusp ,index)
                                      do (funcall *inner-exit-if-exhausted*))
                                   `(do (funcall *inner-exit-if-exhausted*)))
                             do ,@(if colon-p
                                      `((with-arguments
                                            (,(trinsic:client-form client) (pop-argument)
                                             :outer t)
                                          ,@compiled-items))
                                      compiled-items)))))))
          (if at-sign-p
              `((with-remaining-arguments ()
                  ,@(expand-loop)))
              (let ((arg-form (pop-argument-form)))
                (with-dynamic-arguments ()
                  `((with-arguments (,(trinsic:client-form client) ,arg-form)
                      ,@(expand-loop))))))))))

;;; 22.3.7.6 ~? Recursive processing

(defclass recursive-processing-directive
    (directive only-at-sign-mixin)
  ())

(defmethod specialize-directive ((client client) (char (eql #\?)) directive)
  (change-class directive 'recursive-processing-directive))

(defmethod calculate-argument-position (position (directive recursive-processing-directive))
  (unless (at-sign-p directive)
    (1+ position)))

(defmethod interpret-item
    ((client client) (directive recursive-processing-directive) &optional parameters)
  (declare (ignore parameters))
  (if (at-sign-p directive)
      (format-remaining-recursive client (pop-argument 'format-control))
      (format-single-recursive client (pop-argument 'format-control) (pop-argument))))

  (defmethod compile-item
      ((client client) (directive recursive-processing-directive) &optional parameters)
    (declare (ignore parameters))
    (if (at-sign-p directive)
        `((format-remaining-recursive ,(trinsic:client-form client)
                                      ,(pop-argument-form 'format-control)))
        `((format-single-recursive ,(trinsic:client-form client)
                                   ,(pop-argument-form 'format-control)
                                   ,(pop-argument-form)))))
