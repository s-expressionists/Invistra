;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5 Pretty printer operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.1 ~_ Conditional newline

(define-directive #\_ underscore-directive nil (named-parameters-directive) ())

(define-format-directive-interpreter underscore-directive
  (inravina:pprint-newline client *destination*
                           (cond ((and colonp at-signp) :mandatory)
                                 (colonp :fill)
                                 (at-signp :miser)
                                 (t :linear))))

(define-format-directive-compiler underscore-directive
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

(defmethod check-directive-syntax progn ((directive logical-block-directive))
  (flet ((check-fix (items)
           (when (notevery (lambda (item)
                             (or (stringp item)
                                 (structured-end-p item)
                                 (structured-separator-p item)))
                           items)
             (error "Directives are not allowed in logical block prefix or suffix"))))
    (when (> (length (clauses directive)) 3)
      (error "Logical block only allows three clauses"))
    (when (> (length (clauses directive)) 1)
      (check-fix (aref (clauses directive) 0)))
    (when (= (length (clauses directive)) 3)
      (check-fix (aref (clauses directive) 2)))))

(define-format-directive-interpreter logical-block-directive
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
      (if per-line-prefix-p
          (inravina:execute-pprint-logical-block client *destination*
                                                 object #'interpret-body
                                                 :per-line-prefix prefix :suffix suffix)
          (inravina:execute-pprint-logical-block client *destination*
                                                 object #'interpret-body
                                                 :prefix prefix :suffix suffix)))))

(define-format-directive-compiler logical-block-directive
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
        `((inravina:execute-pprint-logical-block ,(incless:client-form client) *destination*
                                                 nil
                                                 (lambda (*destination* escape-hook pop-argument-hook)
                                                   (declare (ignore escape-hook pop-argument-hook))
                                                   (catch *inner-tag*
                                                     ,@(compile-items client (aref (clauses directive)
                                                                                   (if (= (length (clauses directive)) 1)
                                                                                       0
                                                                                       1)))))
                                                 ,(if per-line-prefix-p :per-line-prefix :prefix) ,prefix
                                                 :suffix ,suffix))
        `((let* ((object (consume-next-argument t))
                 (*remaining-argument-count* (dotted-list-length object))
                 (*previous-arguments* (make-array *remaining-argument-count*
                                                   :adjustable t :fill-pointer 0))
                 (*previous-argument-index* 0))
            (inravina:execute-pprint-logical-block ,(incless:client-form client) *destination*
                                                   object
                                                   (lambda (*destination* *inner-exit-if-exhausted* *pop-argument-hook*)

                                                     ,@(compile-items client (aref (clauses directive)
                                                                                   (if (= (length (clauses directive)) 1)
                                                                                       0
                                                                                       1))))
                                                   ,(if per-line-prefix-p :per-line-prefix :prefix) ,prefix
                                                   :suffix ,suffix))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.3 ~i Indent

(define-directive #\i i-directive nil (named-parameters-directive)
    ((how-many :type (integer 0) :default-value 0)))

(define-format-directive-interpreter i-directive
  (inravina:pprint-indent client *destination*
                          (if colonp :current :block)
                          how-many))

(define-format-directive-compiler i-directive
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

(defmethod check-directive-syntax progn ((directive call-function-directive))
  ;; Check that there is at most one package marker in the function name.
  ;; Also, compute a symbol from the name.
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp))
    directive
    ;; To figure out where the name of the function starts and ends,
    ;; we cannot search from the beginning of the directive, because
    ;; the given parameters can contain arbitrary characters following
    ;; a single quote (indicating a character parameter).  However,
    ;; we know that the last character of the directive is the trailing
    ;; #\/ of the function name, and the one preceding that is the
    ;; #\/ preceding the function name.
    (let ((pos1 (1+ (position #\/ control-string :end (1- end) :from-end t)))
          (pos2 (1- end)))
      (let ((position-of-first-package-marker
             (position #\: control-string :start pos1 :end pos2))
            (position-of-last-package-marker
             (position #\: control-string :start pos1 :end pos2 :from-end t)))
        (when (and (not (null position-of-first-package-marker))
                   (> position-of-last-package-marker
                      (1+ position-of-first-package-marker)))
          (error 'too-many-package-markers
                 :directive directive))
        ;; The HyperSpec says that all the characters of the function
        ;; name are treated as if they were upper-case.  It would
        ;; probably be smarter to follow the readtable-case of the
        ;; current readtable, but that's not what the spec says.
        (let ((package-name
               (if (null position-of-first-package-marker)
                   "COMMON-LISP-USER"
                   (string-upcase
                    (subseq control-string
                            pos1
                            position-of-first-package-marker))))
              (symbol-name
               (string-upcase
                (subseq control-string
                        (if (null position-of-first-package-marker)
                            pos1
                            (1+ position-of-last-package-marker))
                        pos2))))
          (let ((package (find-package package-name)))
            (when (null package)
              (error 'no-such-package
                     :directive directive))
            (setf (function-name directive) (intern symbol-name package))))))))

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
