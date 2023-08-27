;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7 Control-flow operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.1 ~* Go to

(define-directive t #\* go-to-directive t (named-parameters-directive at-most-one-modifier-mixin)
    ((param :type (or null (integer 0)) :default nil)))

(define-format-directive-interpreter go-to-directive
  (cond (colonp
         ;; Back up in the list of arguments.
         ;; The default value for the parameter is 1.
         (go-to-argument (- (or param 1))))
        (at-signp
         ;; Go to an absolute argument number.
         ;; The default value for the parameter is 0.
         (go-to-argument (or param 0) t))
        (t
         ;; Skip the next arguments.
         ;; The default value for the parameter is 1.
         (go-to-argument (or param 1)))))

(define-format-directive-compiler go-to-directive
  (cond (colonp
         ;; Back up in the list of arguments.
         ;; The default value for the parameter is 1.
         `((go-to-argument (- (or param 1)))))
        (at-signp
         ;; Go to an absolute argument number.
         ;; The default value for the parameter is 0.
         `((go-to-argument (or param 0) t)))
        (t
         ;; Skip the next arguments.
         ;; The default value for the parameter is 1.
         `((go-to-argument (or param 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.3 ~] End of conditional expression

(define-directive t #\] end-conditional-directive t (named-parameters-directive no-modifiers-mixin end-structured-directive-mixin) ())

(define-format-directive-interpreter end-conditional-directive
    ;; do nothing
    nil)

(define-format-directive-compiler end-conditional-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.2 ~[ Conditional expression

(define-directive t #\[ conditional-directive end-conditional-directive
  (named-parameters-directive structured-directive-mixin at-most-one-modifier-mixin)
    ((param :type (or null integer) :default nil))
  (%last-clause-is-default-p :initform nil :accessor last-clause-is-default-p))

(defmethod check-directive-syntax progn (client (directive conditional-directive))
  (declare (ignore client))
  ;; Check that, if a parameter is given, then there are
  ;; no modifiers.
  #+(or)(when (and (not (null (parameters directive)))
             (or (colonp directive) (at-signp directive)))
    (error 'modifier-and-parameter
           :directive directive))
  ;; Check that, if a colon modifier was given, then
  ;; there should be a single clause separator (two clauses).
  (when (and (colonp directive)
             (/= (length (clauses directive)) 2))
    (error 'colon-modifier-requires-two-clauses))
  ;; Check that, if an at-sign modifier was given, then
  ;; there should be a no clause separators (a single clause).
  (when (and (at-signp directive)
             (/= (length (clauses directive)) 1))
    (error 'at-sign-modifier-requires-one-clause))
  (let ((pos (position-if (lambda (items)
                            (let ((last (aref items (1- (length items)))))
                              (and (structured-separator-p last)
                                   (colonp last))))
                          (clauses directive))))
    ;; Check that, if a modifier is given, then there should
    ;; be no clause separator with colon modifier.
    (when (and (or (colonp directive) (at-signp directive))
               pos)
      (error 'clause-separator-with-colon-modifier-not-allowed
             :directive directive))
    (when (and pos
               (< pos (- (length (clauses directive)) 2)))
      (error 'illegal-clause-separators
             :directive directive))
    (setf (last-clause-is-default-p directive) (and pos t))))

(define-format-directive-interpreter conditional-directive
  (cond (at-signp
         (when (consume-next-argument t)
           (go-to-argument -1)
           (interpret-items client (aref (clauses directive) 0))))
        (colonp
         (interpret-items client
                          (aref (clauses directive)
                                (if (consume-next-argument t) 1 0))))
        (t
         ;; If a parameter was given, use it,
         ;; else use the next argument.
         (let ((val (or param (consume-next-argument 'integer))))
           (if (or (minusp val)
                   (>= val (length (clauses directive))))
               ;; Then the argument is out of range
               (when (last-clause-is-default-p directive)
                 ;; Then execute the default-clause
                 (interpret-items client
                                  (aref (clauses directive)
                                        (1- (length (clauses directive))))))
               ;; Else, execute the corresponding clause
               (interpret-items client
                                (aref (clauses directive) val)))))))

(define-format-directive-compiler conditional-directive
  (cond (at-signp
         `((when (consume-next-argument t)
           (go-to-argument -1)
           ,@(compile-items client (aref (clauses directive) 0)))))
        (colonp
         `((cond ((consume-next-argument t)
                  ,@(compile-items client (aref (clauses directive) 1)))
                 (t
                  ,@(compile-items client (aref (clauses directive) 0))))))
        (t
         ;; If a parameter was given, use it,
         ;; else use the next argument.
         `((let ((val (or param (consume-next-argument 'integer))))
             (if (or (minusp val)
                     (>= val ,(length (clauses directive))))
                 ;; Then the argument is out of range
                 ,(when (last-clause-is-default-p directive)
                    ;; Then execute the default-clause
                    `(progn ,@(compile-items client
                                             (aref (clauses directive)
                                                   (1- (length (clauses directive)))))))
                 ;; Else, execute the corresponding clause
                 (case val
                   ,@(loop for i from 0
                           for clause across (clauses directive)
                           collect `(,i ,@(compile-items client clause))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.5 ~} End of iteration

(define-directive t #\} end-iteration-directive t (named-parameters-directive only-colon-mixin end-structured-directive-mixin) ())

(define-format-directive-interpreter end-iteration-directive
    ;; do nothing
    nil)

(define-format-directive-compiler end-iteration-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.4 ~{ Iteration

(define-directive t #\{ iteration-directive end-iteration-directive
  (named-parameters-directive structured-directive-mixin)
    ((iteration-limit :type (or null (integer 0)) :default nil)))

(define-format-directive-interpreter iteration-directive
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let* ((items (aref (clauses directive) 0))
         (oncep (colonp (aref items (1- (length items))))))
    (if (= (length items) 1)
        (let ((control (consume-next-argument '(or string function))))
          (cond ((and colonp at-signp)
                 ;; The remaining arguments should be lists.  Each argument
                 ;; is used in a different iteration.
                 (if (functionp control)
                     (catch *inner-tag*
                       (loop with *outer-catch-tag* = *inner-tag*
                             with *outer-exit-if-exhausted* = *inner-exit-if-exhausted*
                             for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             when (or (not oncep) (plusp index))
                               do (funcall *inner-exit-if-exhausted*)
                             do (apply control *destination* (consume-next-argument 'list))))
                     (catch *inner-tag*
                       (loop with *outer-catch-tag* = *inner-tag*
                             with *outer-exit-if-exhausted* = *inner-exit-if-exhausted*
                             with catch-tag = (list nil)
                             for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             when (or (not oncep) (plusp index))
                               do (funcall *inner-exit-if-exhausted*)
                             do (with-arguments (consume-next-argument 'list)
                                  (let ((*inner-tag* catch-tag))
                                    (catch *inner-tag*
                                      (format-with-runtime-arguments client control))))))))
                (colonp
                 ;; We use one argument, and that should be a list of sublists.
                 ;; Each sublist is used as arguments for one iteration.
                 (if (functionp control)
                     (let ((arg (consume-next-argument 'list)))
                       (if (null iteration-limit)
                           (loop for args in arg ; a bit unusual naming perhaps
                                 do (apply control *destination* args))
                           (loop for args in arg ; a bit unusual naming perhaps
                                 repeat iteration-limit
                                 do (apply control *destination* args))))
                     (let ((arg (consume-next-argument 'list)))
                       (flet ((one-iteration (args)
                                (unless (listp args)
                                  (error 'argument-type-error
                                         :expected-type 'list
                                         :datum args))
                                (with-arguments args
                                  (catch *inner-tag*
                                     (format-with-runtime-arguments client control)))))
                         (if (null iteration-limit)
                             (loop for args in arg ; a bit unusual naming perhaps
                                   do (one-iteration args))
                             (loop for args in arg ; a bit unusual naming perhaps
                                   repeat iteration-limit
                                   do (one-iteration args)))))))
                (at-signp
                 (if (functionp control)
                     (loop for args = (consume-remaining-arguments)
                             then (apply control *destination* args)
                           for index from 0
                           finally (go-to-argument (- (length args)))
                           while (and (or (null iteration-limit)
                                          (< index iteration-limit))
                                      (or (and oncep (zerop index)) args)))
                     (catch *inner-tag*
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
                     (loop for args = (consume-next-argument 'list)
                             then (apply control *destination* args)
                           for index from 0
                           while (and (or (null iteration-limit)
                                          (< index iteration-limit))
                                      (or (and oncep (zerop index)) args)))
                     (with-arguments (consume-next-argument 'list)
                       (catch *inner-tag*
                         (loop for index from 0
                               while (or (null iteration-limit)
                                         (< index iteration-limit))
                               when (or (not oncep) (plusp index))
                                 do (funcall *inner-exit-if-exhausted*)
                               do (format-with-runtime-arguments client control))))))))
        (cond ((and colonp at-signp)
               ;; The remaining arguments should be lists.  Each argument
               ;; is used in a different iteration.
               (catch *inner-tag*
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (with-arguments (consume-next-argument 'list)
                            (interpret-items client items)))))
              (colonp
               ;; We use one argument, and that should be a list of sublists.
               ;; Each sublist is used as arguments for one iteration.
               (with-arguments (consume-next-argument 'list)
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (with-arguments (consume-next-argument 'list)
                            (interpret-items client items)))))
              (at-signp
               (catch *inner-tag*
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
               (with-arguments (consume-next-argument 'list)
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *inner-exit-if-exhausted*)
                       do (interpret-items client items))))))))

(define-format-directive-compiler iteration-directive
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let* ((items (aref (clauses directive) 0))
         (oncep (colonp (aref items (1- (length items))))))
    (if (= (length items) 1)
        (cond ((and colonp at-signp)
               ;; The remaining arguments should be lists.  Each argument
               ;; is used in a different iteration.
               `((let ((control (consume-next-argument '(or function string))))
                   (catch *inner-tag*
                     (loop for index from 0
                           while (or (null iteration-limit)
                                     (< index iteration-limit))
                           ,@(if oncep
                                 '(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                 '(do (funcall *inner-exit-if-exhausted*)))
                           if (functionp control)
                             do (apply control *destination* (consume-next-argument 'list))
                           else
                             do (with-arguments (consume-next-argument 'list)
                                  (format-with-runtime-arguments ,(incless:client-form client)
                                                                 control)))))))
              (colonp
               ;; We use one argument, and that should be a list of sublists.
               ;; Each sublist is used as arguments for one iteration.
               `((let ((control (consume-next-argument '(or function string))))
                   (with-arguments (consume-next-argument 'list)
                     (loop for index from 0
                           while (or (null iteration-limit)
                                     (< index iteration-limit))
                           ,@(if oncep
                                 '(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                 '(do (funcall *inner-exit-if-exhausted*)))
                           if (functionp control)
                             do (apply control *destination* (consume-next-argument 'list))
                           else
                             do (with-arguments (consume-next-argument 'list)
                                  (format-with-runtime-arguments ,(incless:client-form client)
                                                                 control)))))))
              (at-signp
               `((let ((control (consume-next-argument '(or function string))))
                   (if (functionp control)
                       (loop for args = (consume-remaining-arguments)
                               then (apply control *destination* args)
                             for index from 0
                             finally (go-to-argument (- (length args)))
                             while (and (or (null iteration-limit)
                                            (< index iteration-limit))
                                        ,(if oncep
                                             '(or (zerop index) args)
                                             'args)))
                       (catch *inner-tag*
                         (loop for index from 0
                               while (or (null iteration-limit)
                                         (< index iteration-limit))
                               ,@(if oncep
                                     '(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                     '(do (funcall *inner-exit-if-exhausted*)))
                               do (format-with-runtime-arguments ,(incless:client-form client)
                                                                 control)))))))
              (t
               ;; no modifiers
               ;; We use one argument, and that should be a list.
               ;; The elements of that list are used by the iteration.
               `((let ((control (consume-next-argument '(or function string))))
                   (if (functionp control)
                       (loop for args = (consume-next-argument 'list)
                               then (apply control *destination* args)
                             for index from 0
                             while (and (or (null iteration-limit)
                                            (< index iteration-limit))
                                        ,(if oncep
                                             '(or (zerop index) args)
                                             'args)))
                       (with-arguments (consume-next-argument 'list)
                         (loop for index from 0
                               while (or (null iteration-limit)
                                         (< index iteration-limit))
                               ,@(if oncep
                                     '(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                     '(do (funcall *inner-exit-if-exhausted*)))
                               do (format-with-runtime-arguments ,(incless:client-form client)
                                                                 control))))))))
        (let ((compiled-items (compile-items client items)))
          (cond ((and colonp at-signp)
                 ;; The remaining arguments should be lists.  Each argument
                 ;; is used in a different iteration.
                 `((catch *inner-tag*
                     (loop for index from 0
                           while (or (null iteration-limit)
                                     (< index iteration-limit))
                           ,@(if oncep
                                 '(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                 '(do (funcall *inner-exit-if-exhausted*)))
                           do (with-arguments (consume-next-argument 'list)
                                ,@compiled-items)))))
                (colonp
                 ;; We use one argument, and that should be a list of sublists.
                 ;; Each sublist is used as arguments for one iteration.
                 `((with-arguments (consume-next-argument 'list)
                     (loop for index from 0
                           while (or (null iteration-limit)
                                     (< index iteration-limit))
                           ,@(if oncep
                                 '(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                 '(do (funcall *inner-exit-if-exhausted*)))
                           do (with-arguments (consume-next-argument 'list)
                                ,@compiled-items)))))
                (at-signp
                 `((catch *inner-tag*
                     (loop for index from 0
                           while (or (null iteration-limit)
                                     (< index iteration-limit))
                           ,@(if oncep
                                 '(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                 '(do (funcall *inner-exit-if-exhausted*)))
                           ,@(when compiled-items
                               (list* 'do compiled-items))))))
                (t
                 ;; no modifiers
                 ;; We use one argument, and that should be a list.
                 ;; The elements of that list are used by the iteration.
                 `((with-arguments (consume-next-argument 'list)
                     (loop for index from 0
                           while (or (null iteration-limit)
                                     (< index iteration-limit))
                           ,@(if oncep
                                 '(when (plusp index) do (funcall *inner-exit-if-exhausted*))
                                 '(do (funcall *inner-exit-if-exhausted*)))
                           ,@(when compiled-items
                               (list* 'do compiled-items)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.6 ~? Recursive processing

(define-directive t #\? recursive-processing-directive t (named-parameters-directive only-at-sign-mixin) ())

(define-format-directive-interpreter recursive-processing-directive
  (if at-signp
      ;; reuse the arguments from the parent control-string
      (format-with-runtime-arguments client
                                     (consume-next-argument 'string))
      ;;
      (apply #'format
             client
             *destination*
             (consume-next-argument 'string)
             (consume-next-argument 'list))))

(define-format-directive-compiler recursive-processing-directive
  (if at-signp
      ;; reuse the arguments from the parent control-string
      `((format-with-runtime-arguments ,(incless:client-form client)
                                       (consume-next-argument 'string)))
      ;;
      `((apply #'format
               ,(incless:client-form client)
               *destination*
               (consume-next-argument 'string)
               (consume-next-argument 'list)))))
