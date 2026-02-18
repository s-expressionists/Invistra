;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7 Control-flow operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.1 ~* Go to

(defclass go-to-directive
    (directive at-most-one-modifier-mixin) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\*)) directive (end-directive t))
  (change-class directive 'go-to-directive))

(defmethod parameter-specifications ((client t) (directive go-to-directive))
  '((:name n :type (or null (integer 0)) :default nil)))

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

(defmethod interpret-item (client (directive go-to-directive) &optional parameters)
  (declare (ignore client))
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

(defmethod compile-item (client (directive go-to-directive) &optional parameters)
  (declare (ignore client))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.3 ~] End of conditional expression

(defclass end-conditional-expression-directive
    (directive no-modifiers-mixin
     end-structured-directive-mixin)
  nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\])) directive (end-directive t))
  (change-class directive 'end-conditional-expression-directive))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.2 ~[ Conditional expression

(defclass conditional-expression-directive
    (directive structured-directive-mixin
     at-most-one-modifier-mixin)
  ((%last-clause-is-default-p :initform nil
                              :accessor last-clause-is-default-p)))

(defmethod specialize-directive
    ((client standard-client) (char (eql #\[)) directive
     (end-directive end-conditional-expression-directive))
  (change-class directive 'conditional-expression-directive))

(defmethod specialize-directive
    ((client standard-client) (char (eql #\[)) directive (end-directive t))
  (error 'unmatched-directive
         :client client
         :directive directive))

(defmethod parameter-specifications
    ((client t) (directive conditional-expression-directive))
  '((:name n :type (or null integer) :default nil)))

(defmethod calculate-argument-position (position (directive conditional-expression-directive))
  (setf position (call-next-method))
  (when position
    (cond ((at-sign-p directive)
           ;; consequent must consume 1 argument.
           (when (eql (1+ position)
                      (reduce #'calculate-argument-position (aref (clauses directive) 0)
                              :initial-value position))
             (1+ position)))
          ((colon-p directive)
           ;; alternative and consequent must consume the same number of arguments.
           (incf position)
           (let ((pos0 (reduce #'calculate-argument-position (aref (clauses directive) 0)
                               :initial-value position))
                 (pos1 (reduce #'calculate-argument-position (aref (clauses directive) 1)
                               :initial-value position)))
             (when (eql pos0 pos1)
               pos0)))
          ((typep (car (parameters directive)) 'argument-reference-parameter)
           nil)
          ((and (typep (car (parameters directive)) 'literal-parameter)
                (parameter-value (car (parameters directive))))
           (let ((value (parameter-value (car (parameters directive)))))
             (cond ((< -1 value (length (clauses directive)))
                    (calculate-argument-position position (aref (clauses directive) value)))
                   ((last-clause-is-default-p directive)
                    (calculate-argument-position position (aref (clauses directive)
                                                                (1- (length (clauses directive))))))
                   (t
                    position))))
          (t
           (when (typep (car (parameters directive)) 'literal-parameter)
             (incf position))
           (let ((new-position (if (last-clause-is-default-p directive)
                                   (reduce #'calculate-argument-position (aref (clauses directive) 0)
                                           :initial-value position)
                                   position)))
             (when (every (lambda (clauses)
                            (eql new-position
                                 (reduce #'calculate-argument-position clauses
                                         :initial-value position)))
                          (clauses directive))
                      new-position))))))

(defmethod check-directive-syntax progn (client (directive conditional-expression-directive))
  ;; Check that, if a parameter is given, then there are
  ;; no modifiers.
  (when (and (not (null (parameters directive)))
             (or (colon-p directive) (at-sign-p directive)))
    (error 'modifier-and-parameter
           :client client
           :directive directive
           :positions (loop for i from (1+ (start directive)) below (character-start directive)
                            collect i)))
  ;; Check that, if a colon modifier was given, then
  ;; there should be a single clause separator (two clauses).
  (when (and (colon-p directive)
             (/= (length (clauses directive)) 2))
    (error 'invalid-clause-count
           :client client
           :directive directive
           :minimum-count 2
           :maximum-count 2))
  ;; Check that, if an at-sign modifier was given, then
  ;; there should be a no clause separators (a single clause).
  (when (and (at-sign-p directive)
             (/= (length (clauses directive)) 1))
    (error 'invalid-clause-count
           :client client
           :directive directive
           :minimum-count 1
           :maximum-count 1))
  (let ((pos (position-if (lambda (items)
                            (let ((last (aref items (1- (length items)))))
                              (and (structured-separator-p last)
                                   (colon-p last))))
                          (clauses directive))))
    ;; Check that, if a modifier is given, then there should
    ;; be no clause separator with colon modifier.
    (when (and (or (colon-p directive) (at-sign-p directive))
               pos)
      (error 'clause-separator-with-colon-modifier-not-allowed
             :client client
             :directive directive))
    (when (and pos
               (< pos (- (length (clauses directive)) 2)))
      (error 'illegal-clause-separators
             :client client
             :directive directive))
    (setf (last-clause-is-default-p directive) (and pos t))))


(defmethod interpret-item (client (directive conditional-expression-directive) &optional parameters)
  (with-accessors ((at-sign-p at-sign-p)
                   (colon-p colon-p)
                   (clauses clauses))
      directive
    (cond (at-sign-p
           (when (pop-argument)
             (go-to-argument -1)
             (interpret-items client (aref clauses 0))))
          (colon-p
           (interpret-items client
                            (aref clauses
                                  (if (pop-argument) 1 0))))
          (t
           ;; If a parameter was given, use it,
           ;; else use the next argument.
           (let ((n (or (car parameters) (pop-argument 'integer))))
             (cond ((< -1 n (length clauses))
                    (interpret-items client
                                     (aref clauses n)))
                   ((last-clause-is-default-p directive)
                    (interpret-items client
                                     (aref clauses
                                           (1- (length clauses)))))))))))

(defmethod compile-item (client (directive conditional-expression-directive) &optional parameters)
  (with-accessors ((at-sign-p at-sign-p)
                   (colon-p colon-p)
                   (clauses clauses))
      directive
    (cond (at-sign-p
           `((when ,(pop-argument-form)
               ,@(go-to-argument-forms -1)
               ,@(compile-items client (aref clauses 0)))))
          (colon-p
           `((cond (,(pop-argument-form)
                    ,@(compile-items client (aref clauses 1)))
                   (t
                    ,@(compile-items client (aref clauses 0))))))
          (t
           (let ((n (car parameters)))
             (cond ((not (numberp n))
                    `((case ,(cond ((null n)
                                    (pop-argument-form 'integer))
                                   ((typep (car (parameters directive)) 'remaining-argument-count-parameter)
                                    n)
                                   (t
                                    `(or ,n ,(pop-argument-form 'integer))))
                        ,@(loop for i from 0
                                for j downfrom (1- (length clauses))
                                for clause across clauses
                                collect `(,(if (and (zerop j)
                                                    (last-clause-is-default-p directive))
                                               'otherwise
                                               i)
                                          ,@(compile-items client clause))))))
                   ((< -1 n (length clauses))
                    (compile-items client (aref clauses n)))
                   ((last-clause-is-default-p directive)
                    (compile-items client (aref clauses (1- (length clauses)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.5 ~} End of iteration

(defclass end-iteration-directive
    (directive only-colon-mixin
     end-structured-directive-mixin)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\})) directive (end-directive t))
  (change-class directive 'end-iteration-directive))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.4 ~{ Iteration

(defclass iteration-directive
    (directive structured-directive-mixin)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\{)) directive
     (end-directive end-iteration-directive))
  (change-class directive 'iteration-directive))

(defmethod specialize-directive
    ((client standard-client) (char (eql #\{)) directive (end-directive t))
  (error 'unmatched-directive
         :client client
         :directive directive))

(defmethod parameter-specifications
            ((client t) (directive iteration-directive))
   '((:name n :type (or null (integer 0)) :default nil)))

(defmethod calculate-argument-position (position (directive iteration-directive))
  (unless (at-sign-p directive)
    (1+ position)))

(defmethod interpret-item (client (directive iteration-directive) &optional parameters)
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let* ((colon-p (colon-p directive))
         (at-sign-p (at-sign-p directive))
         (iteration-limit (car parameters))
         (items (aref (clauses directive) 0))
         (oncep (colon-p (aref items (1- (length items))))))
    (if (= (length items) 1)
        (let ((control (pop-argument '(or string function))))
          (cond ((and colon-p at-sign-p)
                 ;; The remaining arguments should be lists.  Each argument
                 ;; is used in a different iteration.
                 (with-remaining-arguments ()
                   (if (functionp control)
                       (loop for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             when (or (not oncep) (plusp index))
                               do (funcall *inner-exit-if-exhausted*)
                             do (apply control *format-output* (pop-argument 'list)))
                       (loop with items = (parse-control-string client control)
                             for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             when (or (not oncep) (plusp index))
                               do (funcall *inner-exit-if-exhausted*)
                             do (with-arguments (client (pop-argument))
                                  (interpret-items client items))))))
                (colon-p
                 ;; We use one argument, and that should be a list of sublists.
                 ;; Each sublist is used as arguments for one iteration.
                 (if (functionp control)
                     (let ((arg (pop-argument 'list)))
                       (if (null iteration-limit)
                           (loop for args in arg ; a bit unusual naming perhaps
                                 do (apply control *format-output* args))
                           (loop for args in arg ; a bit unusual naming perhaps
                                 repeat iteration-limit
                                 do (apply control *format-output* args))))
                     (let ((arg (pop-argument 'list)))
                       (flet ((one-iteration (args)
                                (unless (listp args)
                                  (error 'argument-type-error
                                         :expected-type 'list
                                         :datum args))
                                (with-arguments (client args)
                                  (format-with-runtime-arguments client control))))
                         (if (null iteration-limit)
                             (loop for args in arg ; a bit unusual naming perhaps
                                   do (one-iteration args))
                             (loop for args in arg ; a bit unusual naming perhaps
                                   repeat iteration-limit
                                   do (one-iteration args)))))))
                (at-sign-p
                 (if (functionp control)
                     (loop for args = (pop-remaining-arguments)
                             then (apply control *format-output* args)
                           for index from 0
                           finally (go-to-argument (- (length args)))
                           while (and (or (null iteration-limit)
                                          (< index iteration-limit))
                                      (or (and oncep (zerop index)) args)))
                     (with-remaining-arguments ()
                       (loop for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             when (or (not oncep) (plusp index))
                               do (funcall *inner-exit-if-exhausted*)
                             do (format-with-runtime-arguments client control)))))
                (t
                 ;; no modifiers
                 ;; We use one argument, and that should be a list.
                 ;; The elements of that list are used by the iteration.
                 (if (functionp control)
                     (loop for args = (pop-argument 'list)
                             then (apply control *format-output* args)
                           for index from 0
                           while (and (or (null iteration-limit)
                                          (< index iteration-limit))
                                      (or (and oncep (zerop index)) args)))
                     (with-arguments (client (pop-argument))
                       (loop for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             when (or (not oncep) (plusp index))
                               do (funcall *inner-exit-if-exhausted*)
                             do (format-with-runtime-arguments client control)))))))
        (cond ((and colon-p at-sign-p)
               ;; The remaining arguments should be lists.  Each argument
               ;; is used in a different iteration.
               (with-remaining-arguments ()
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (with-arguments (client (pop-argument))
                            (interpret-items client items)))))
              (colon-p
               ;; We use one argument, and that should be a list of sublists.
               ;; Each sublist is used as arguments for one iteration.
               (with-arguments (client (pop-argument))
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (with-arguments (client (pop-argument))
                            (interpret-items client items)))))
              (at-sign-p
               (with-remaining-arguments ()
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (interpret-items client items))))
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
                       do (interpret-items client items))))))))

(defmethod compile-item (client (directive iteration-directive) &optional parameters)
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let* ((colon-p (colon-p directive))
         (at-sign-p (at-sign-p directive))
         (iteration-limit (car parameters))
         (items (aref (clauses directive) 0))
         (oncep (colon-p (aref items (1- (length items))))))
    (if (= (length items) 1)
        (cond ((and colon-p at-sign-p)
               ;; The remaining arguments should be lists.  Each argument
               ;; is used in a different iteration.
               `((let ((iteration-limit ,iteration-limit)
                       (control (pop-argument '(or function string))))
                   (with-remaining-arguments ()
                     (if (functionp control)
                         (loop for index from 0
                               while (or (null iteration-limit)
                                         (< index iteration-limit))
                               ,@(if oncep
                                     `(when (plusp index)
                                        do (funcall *inner-exit-if-exhausted*))
                                     `(do (funcall *inner-exit-if-exhausted*)))
                               do (apply control *format-output* (pop-argument 'list)))
                         (loop with items = (parse-control-string ,(trinsic:client-form client)
                                                                  control)
                               for index from 0
                               while (or (null iteration-limit)
                                         (< index iteration-limit))
                               ,@(if oncep
                                     `(when (plusp index)
                                        do (funcall *inner-exit-if-exhausted*))
                                     `(do (funcall *inner-exit-if-exhausted*)))
                               do (with-arguments (,(trinsic:client-form client) (pop-argument) :outer t)
                                    (interpret-items ,(trinsic:client-form client) items))))))))
              (colon-p
               ;; We use one argument, and that should be a list of sublists.
               ;; Each sublist is used as arguments for one iteration.
               (let ((control-form (pop-argument-form '(or function string)))
                     (args-form (pop-argument-form)))
                 (with-dynamic-arguments ()
                   `((let ((iteration-limit ,iteration-limit)
                           (control ,control-form))
                       (with-arguments (,(trinsic:client-form client) ,args-form)
                         (if (functionp control)
                             (loop for index from 0
                                   while (or (null iteration-limit)
                                             (< index iteration-limit))
                                   ,@(if oncep
                                         `(when (plusp index)
                                            do (funcall *inner-exit-if-exhausted*))
                                         `(do (funcall *inner-exit-if-exhausted*)))
                                   do (apply control *format-output* (pop-argument 'list)))
                             (loop with items = (parse-control-string ,(trinsic:client-form client)
                                                                      ,control-form)
                                   for index from 0
                                   while (or (null iteration-limit)
                                             (< index iteration-limit))
                                   ,@(if oncep
                                         `(when (plusp index)
                                            do (funcall *inner-exit-if-exhausted*))
                                         `(do (funcall *inner-exit-if-exhausted*)))
                                   do (with-arguments (,(trinsic:client-form client) (pop-argument) :outer t)
                                        (interpret-items ,(trinsic:client-form client) items))))))))))
              (at-sign-p
               `((let ((iteration-limit ,iteration-limit)
                       (control (pop-argument '(or function string))))
                   (if (functionp control)
                       (loop for args = (pop-remaining-arguments)
                               then (apply control *format-output* args)
                             for index from 0
                             finally (go-to-argument (- (length args)))
                             while (and (or (null iteration-limit)
                                            (< index iteration-limit))
                                        ,(if oncep
                                             '(or (zerop index) args)
                                             'args)))
                       (with-remaining-arguments ()
                         (loop with items = (parse-control-string ,(trinsic:client-form client)
                                                                  control)
                               for index from 0
                               while (or (null iteration-limit)
                                         (< index iteration-limit))
                               ,@(if oncep
                                     `(when (plusp index)
                                        do (funcall *inner-exit-if-exhausted*))
                                     `(do (funcall *inner-exit-if-exhausted*)))
                               do (interpret-items ,(trinsic:client-form client) items)))))))
              (t
               ;; no modifiers
               ;; We use one argument, and that should be a list.
               ;; The elements of that list are used by the iteration.
               (let ((control-form (pop-argument-form '(or function string)))
                     (args-form (pop-argument-form)))
                 (with-dynamic-arguments ()
                   `((let ((iteration-limit ,iteration-limit)
                           (control ,control-form))
                       (if (functionp control)
                           (loop for args = ,args-form
                                   then (apply control *format-output* args)
                                 for index from 0
                                 while (and (or (null iteration-limit)
                                                (< index iteration-limit))
                                            ,(if oncep
                                                 '(or (zerop index) args)
                                                 'args)))
                           (with-arguments (,(trinsic:client-form client) ,args-form)
                             (loop with items = (parse-control-string ,(trinsic:client-form client)
                                                                      control)
                                   for index from 0
                                   while (or (null iteration-limit)
                                             (< index iteration-limit))
                                   ,@(if oncep
                                         `(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                         `(do (funcall *inner-exit-if-exhausted*)))
                                   do (interpret-items ,(trinsic:client-form client) items))))))))))
        (cond ((and colon-p at-sign-p)
               ;; The remaining arguments should be lists.  Each argument
               ;; is used in a different iteration.
               (let ((compiled-items (compile-items client items)))
                 `((let ((iteration-limit ,iteration-limit))
                     (with-remaining-arguments ()
                       (loop for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             ,@(if oncep
                                   `(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                   `(do (funcall *inner-exit-if-exhausted*)))
                             do (with-arguments (,(trinsic:client-form client) (pop-argument) :outer t)
                                  ,@compiled-items)))))))
              (colon-p
               ;; We use one argument, and that should be a list of sublists.
               ;; Each sublist is used as arguments for one iteration.
               (let ((arg-form (pop-argument-form)))
                 (with-dynamic-arguments ()
                   (let ((compiled-items (compile-items client items)))
                     `((let ((iteration-limit ,iteration-limit))
                         (with-arguments (,(trinsic:client-form client) ,arg-form)
                           (loop for index from 0
                                 while (or (null iteration-limit)
                                           (< index iteration-limit))
                                 ,@(if oncep
                                       `(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                       `(do (funcall *inner-exit-if-exhausted*)))
                                 do (with-arguments (,(trinsic:client-form client) (pop-argument) :outer t)
                                      ,@compiled-items)))))))))
              (at-sign-p
               (let ((compiled-items (compile-items client items)))
                 `((let ((iteration-limit ,iteration-limit))
                     (with-remaining-arguments ()
                       (loop for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             ,@(if oncep
                                   `(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                   `(do (funcall *inner-exit-if-exhausted*)))
                             ,@(when compiled-items
                                 (list* 'do compiled-items))))))))
              (t
               ;; no modifiers
               ;; We use one argument, and that should be a list.
               ;; The elements of that list are used by the iteration.
               (let ((arg-form (pop-argument-form)))
                 (with-dynamic-arguments ()
                   (let ((compiled-items (compile-items client items)))
                     `((let ((iteration-limit ,iteration-limit))
                         (with-arguments (,(trinsic:client-form client) ,arg-form)
                           (loop for index from 0
                                 while (or (null iteration-limit)
                                           (< index iteration-limit))
                                 ,@(if oncep
                                       `(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                       `(do (funcall *inner-exit-if-exhausted*)))
                                 ,@(when compiled-items
                                     (list* 'do compiled-items))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.6 ~? Recursive processing

(defclass recursive-processing-directive
    (directive only-at-sign-mixin)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\?)) directive (end-directive t))
  (change-class directive 'recursive-processing-directive))

(defmethod calculate-argument-position (position (directive recursive-processing-directive))
  (unless (at-sign-p directive)
    (1+ position)))

(defmethod interpret-item (client (directive recursive-processing-directive) &optional parameters)
  (declare (ignore parameters))
  (let ((control (pop-argument 'string)))
    (if (at-sign-p directive)
        ;; reuse the arguments from the parent control-string
        (with-remaining-arguments ()
            (format-with-runtime-arguments client control))
        ;;
        (with-arguments (client (pop-argument))
          (format-with-runtime-arguments client control)))))

(defmethod compile-item (client (directive recursive-processing-directive) &optional parameters)
  (declare (ignore parameters))
  (if (at-sign-p directive)
      ;; reuse the arguments from the parent control-string
      `((with-remaining-arguments ()
          (format-with-runtime-arguments ,(trinsic:client-form client)
                                         (pop-argument 'string))))
      ;;
      `((let ((control ,(pop-argument-form 'string)))
          (with-arguments (,(trinsic:client-form client) ,(pop-argument-form))
            (format-with-runtime-arguments ,(trinsic:client-form client) control))))))
