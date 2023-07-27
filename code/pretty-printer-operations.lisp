;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5 Pretty printer operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.1 ~_ Conditional newline

(define-directive #\_ underscore-directive nil (named-parameters-directive) ())

(defmethod layout-requirements ((item underscore-directive))
  (list :logical-block))

(define-format-directive-interpreter underscore-directive
  #-sicl
  (inravina:pprint-newline client *destination*
                           (cond ((and colonp at-signp) :mandatory)
                                 (colonp :fill)
                                 (at-signp :miser)
                                 (t :linear))))

(define-format-directive-compiler underscore-directive
  #-sicl
  `((inravina:pprint-newline ,(incless:client-form client) *destination*
                             ,(cond ((and colonp at-signp) :mandatory)
                                    (colonp :fill)
                                    (at-signp :miser)
                                    (t :linear)))))

(define-directive #\>
    end-logical-block-directive
    nil
    (named-parameters-directive end-structured-directive-mixin)
    ())

(define-format-directive-interpreter end-logical-block-directive
  ;; do nothing
  nil)

(define-format-directive-compiler end-logical-block-directive
    ;; do nothing
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.2 ~< Logical block

(define-directive #\<
    logical-block-directive
    end-logical-block-directive
    (named-parameters-directive structured-directive-mixin)
    ())

(defmethod layout-requirements :around ((item logical-block-directive))
  (merge-layout-requirements (list :logical-block)
                             (call-next-method)
                             t))

(defmethod check-directive-syntax progn ((directive logical-block-directive))
  (flet ((check-fix (items)
           (when (notevery (lambda (item)
                             (or (stringp item)
                                 (structured-end-p item)
                                 (structured-separator-p item)))
                           items)
             (error 'illegal-directive))))
    (when (> (length (clauses directive)) 3)
      (error 'logical-block-only-permits-three-clauses))
    (when (> (length (clauses directive)) 1)
      (check-fix (aref (clauses directive) 0)))
    (when (= (length (clauses directive)) 3)
      (check-fix (aref (clauses directive) 2)))))

(define-format-directive-interpreter logical-block-directive
  #-sicl
  (let* ((last-clause (aref (clauses directive) (1- (length (clauses directive)))))
         (*newline-kind* (if (at-signp (aref last-clause (1- (length last-clause))))
                             :fill
                             nil))
         (prefix (cond ((> (length (clauses directive)) 1)
                        (if (> (length (aref (clauses directive) 0)) 1)
                            (aref (aref (clauses directive) 0) 0)
                            ""))
                       (colonp
                        "(")
                       (t
                        "")))
         (suffix (cond ((> (length (clauses directive)) 2)
                        (if (> (length (aref (clauses directive) 2)) 1)
                            (aref (aref (clauses directive) 2) 0)
                            ""))
                       (colonp
                        ")")
                       (t
                        "")))
         (per-line-prefix-p (and (> (length (clauses directive)) 1)
                                 (at-signp (aref (aref (clauses directive) 0)
                                            (1- (length (aref (clauses directive) 0)))))))
         (object (unless at-signp (consume-next-argument t))))
    (flet ((interpret-body (*destination* escape-hook pop-argument-hook)
             (if at-signp
                 (interpret-items client (aref (clauses directive)
                                               (if (= (length (clauses directive)) 1)
                                                   0
                                                   1)))
                 (let* ((*remaining-argument-count* (dotted-list-length object))
                        (*previous-arguments* (make-array *remaining-argument-count*
                                                          :adjustable t :fill-pointer 0))
                        (*previous-argument-index* 0)
                        (*inner-exit-if-exhausted* escape-hook)
                        (*pop-argument-hook* pop-argument-hook))
                   (interpret-items client (aref (clauses directive)
                                                 (if (= (length (clauses directive)) 1)
                                                     0
                                                     1)))))))
      (inravina:execute-logical-block client *destination*
                                      object #'interpret-body
                                      :prefix prefix
                                      :per-line-prefix-p per-line-prefix-p
                                      :suffix suffix))))

(define-format-directive-compiler logical-block-directive
  #-sicl
  (let* ((last-clause (aref (clauses directive) (1- (length (clauses directive)))))
         (*newline-kind* (if (at-signp (aref last-clause (1- (length last-clause))))
                             :fill
                             nil))
         (prefix (cond ((> (length (clauses directive)) 1)
                        (if (> (length (aref (clauses directive) 0)) 1)
                            (aref (aref (clauses directive) 0) 0)
                            ""))
                       (colonp
                        "(")
                       (t
                        "")))
         (suffix (cond ((> (length (clauses directive)) 2)
                        (if (> (length (aref (clauses directive) 2)) 1)
                            (aref (aref (clauses directive) 2) 0)
                            ""))
                       (colonp
                        ")")
                       (t
                        "")))
         (per-line-prefix-p (and (> (length (clauses directive)) 1)
                                 (at-signp (aref (aref (clauses directive) 0)
                                            (1- (length (aref (clauses directive) 0))))))))
    (if at-signp
        `((inravina:execute-logical-block ,(incless:client-form client) *destination*
                                          nil
                                          (lambda (*destination* escape-hook pop-argument-hook)
                                            (declare (ignore escape-hook pop-argument-hook))
                                            (catch *inner-tag*
                                              ,@(compile-items client (aref (clauses directive)
                                                                            (if (= (length (clauses directive)) 1)
                                                                                0
                                                                                1)))))
                                          :prefix ,prefix :suffix ,suffix
                                          :per-line-prefix-p ,per-line-prefix-p))
        `((let* ((object (consume-next-argument t))
                 (*remaining-argument-count* (dotted-list-length object))
                 (*previous-arguments* (make-array *remaining-argument-count*
                                                   :adjustable t :fill-pointer 0))
                 (*previous-argument-index* 0))
            (inravina:execute-logical-block ,(incless:client-form client) *destination*
                                            object
                                            (lambda (*destination* *inner-exit-if-exhausted* *pop-argument-hook*)

                                              ,@(compile-items client (aref (clauses directive)
                                                                            (if (= (length (clauses directive)) 1)
                                                                                0
                                                                                1))))
                                            :prefix ,prefix :suffix ,suffix
                                            :per-line-prefix-p ,per-line-prefix-p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.3 ~i Indent

(define-directive #\i i-directive nil (named-parameters-directive)
    ((how-many :type integer :default-value 0)))

(defmethod layout-requirements ((item i-directive))
  (list :logical-block))

(define-format-directive-interpreter i-directive
  #-sicl
  (inravina:pprint-indent client *destination*
                          (if colonp :current :block)
                          how-many))

(define-format-directive-compiler i-directive
  #-sicl
  `((inravina:pprint-indent ,(incless:client-form client) *destination*
                            ,(if colonp :current :block)
                            how-many)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.4 ~/ Call function

;;; This directive is particular in two different ways.  First, as
;;; with the "ignored newline" directive, there are characters
;;; belonging to the directive beyond the directive character itself,
;;; which means the standard mechanism of parsing it cannot be used.
;;; Second, this directive takes an arbitrary number of parameters.
;;;
;;; So, define-format-directive-interpreter cannot be used, since its
;;; main purpose is to give lexical access to each parameter by name.

(define-directive #\/ call-function-directive nil (directive)
    ()
  (%function-name :accessor function-name))

(defmethod parse-directive-suffix ((directive-character (eql #\/)) control-string start end)
  (let ((position-of-trailing-slash
          (position #\/ control-string :start start :end end)))
    (when (null position-of-trailing-slash)
      (error 'end-of-control-string-error
             :control-string string
             :tilde-position start
             :why "expected a trailing slash"))
    (1+ position-of-trailing-slash)))

(defmethod check-directive-syntax progn ((directive call-function-directive))
  ;; Check that there is at most one package marker in the function name.
  ;; Also, compute a symbol from the name.
  (with-accessors ((control-string control-string)
                   (start start)
                   (suffix-start suffix-start)
                   (end end)
                   (colonp colonp))
      directive
    ;; The HyperSpec says that all the characters of the function
    ;; name are treated as if they were upper-case.
    (let* ((position-of-package-marker
             (position #\: control-string :start suffix-start :end (1- end)))
           (package-name
             (if (null position-of-package-marker)
                 "COMMON-LISP-USER"
                 (string-upcase
                  (subseq control-string
                          suffix-start
                          position-of-package-marker))))
           (symbol-name
             (string-upcase
              (subseq control-string
                      (cond ((null position-of-package-marker)
                             suffix-start)
                            ((char= #\: (char control-string (1+ position-of-package-marker)))
                             (+ 2 position-of-package-marker))
                            (t
                             (1+ position-of-package-marker)))
                      (1- end))))
           (package (find-package package-name)))
      (when (null package)
        (error 'no-such-package
               :directive directive))
      (setf (function-name directive) (intern symbol-name package)))))

(defmethod interpret-format-directive (client (directive call-function-directive))
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp)
                   (at-signp at-signp)
                   (given-parameters given-parameters)
                   (function-name function-name))
    directive
    (let ((param-args
           (loop for parameter in given-parameters
                 collect (cond ((eq parameter :remaining-argument-count)
                                *remaining-argument-count*)
                               ((eq parameter :argument-reference)
                                (consume-next-argument t))
                               (t parameter)))))
      (apply function-name
             *destination*
             (consume-next-argument t)
             colonp
             at-signp
             param-args))))

;;; This is not quite right.  We should probably look up the
;;; function name at runtime as opposed to compile time.
(defmethod compile-format-directive (client (directive call-function-directive))
  (declare (ignorable client))
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp)
                   (at-signp at-signp)
                   (given-parameters given-parameters)
                   (function-name function-name))
      directive
    `((let ((param-args (list ,@(mapcar (lambda (parameter)
                                          (case parameter
                                            (:remaining-argument-count
                                             '*remaining-argument-count*)
                                            (:argument-reference
                                             '(consume-next-argument t))
                                            (otherwise
                                             parameter)))
                                        given-parameters))))
        (apply ',function-name
               *destination*
               (consume-next-argument t)
               ,colonp
               ,at-signp
               param-args)))))
