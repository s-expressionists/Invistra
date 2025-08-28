;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5 Pretty printer operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.1 ~_ Conditional newline

(defclass underscore-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\_)) directive (end-directive t))
  (change-class directive 'underscore-directive))

(defmethod layout-requirements ((item underscore-directive))
  (list :logical-block))

(defmethod interpret-item (client (directive underscore-directive) &optional parameters)
  (declare (ignore parameters)
           (ignorable client))
  #-sicl
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (inravina:pprint-newline client *destination*
                             (cond ((and colon-p at-sign-p) :mandatory)
                                   (colon-p :fill)
                                   (at-sign-p :miser)
                                   (t :linear)))))

(defmethod compile-item (client (directive underscore-directive) &optional parameters)
  (declare (ignore parameters)
           (ignorable client))
  #-sicl
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    `((inravina:pprint-newline ,(trinsic:client-form client) *destination*
                               ,(cond ((and colon-p at-sign-p) :mandatory)
                                      (colon-p :fill)
                                      (at-sign-p :miser)
                                      (t :linear))))))

(defclass end-logical-block-directive
    (directive end-structured-directive-mixin) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.2 ~< Logical block

(defclass logical-block-directive
    (directive structured-directive-mixin) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\<)) directive
     (end-directive end-logical-block-directive))
  (change-class directive 'logical-block-directive))

(defmethod specialize-directive
    ((client t) (char (eql #\<)) directive (end-directive t))
  (error 'unmatched-directive
         :directive directive
         :control-string (control-string directive)
         :tilde-position (start directive)))

(defmethod layout-requirements :around ((item logical-block-directive))
  (merge-layout-requirements (list :logical-block)
                             (call-next-method)
                             t))

(defmethod check-directive-syntax progn (client (directive logical-block-directive))
  (declare (ignore client))
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

(defmethod interpret-item (client (directive logical-block-directive) &optional parameters)
  (declare (ignore parameters)
           (ignorable client))
  #-sicl
  (let* ((last-clause (aref (clauses directive) (1- (length (clauses directive)))))
         (colon-p (colon-p directive))
         (at-sign-p (at-sign-p directive))
         (*newline-kind* (if (at-sign-p (aref last-clause (1- (length last-clause))))
                             :fill
                             nil))
         (prefix (cond ((> (length (clauses directive)) 1)
                        (if (> (length (aref (clauses directive) 0)) 1)
                            (aref (aref (clauses directive) 0) 0)
                            ""))
                       (colon-p
                        "(")
                       (t
                        "")))
         (suffix (cond ((> (length (clauses directive)) 2)
                        (if (> (length (aref (clauses directive) 2)) 1)
                            (aref (aref (clauses directive) 2) 0)
                            ""))
                       (colon-p
                        ")")
                       (t
                        "")))
         (per-line-prefix-p (and (> (length (clauses directive)) 1)
                                 (at-sign-p (aref (aref (clauses directive) 0)
                                            (1- (length (aref (clauses directive) 0)))))))
         (object (unless at-sign-p (pop-argument))))
    (flet ((interpret-body (*destination* escape-hook pop-argument-hook)
             (if at-sign-p
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

(defmethod compile-item (client (directive logical-block-directive) &optional parameters)
  (declare (ignore parameters)
           (ignorable client))
  #-sicl
  (let* ((last-clause (aref (clauses directive) (1- (length (clauses directive)))))
         (colon-p (colon-p directive))
         (at-sign-p (at-sign-p directive))
         (*newline-kind* (if (at-sign-p (aref last-clause (1- (length last-clause))))
                             :fill
                             nil))
         (prefix (cond ((> (length (clauses directive)) 1)
                        (if (> (length (aref (clauses directive) 0)) 1)
                            (aref (aref (clauses directive) 0) 0)
                            ""))
                       (colon-p
                        "(")
                       (t
                        "")))
         (suffix (cond ((> (length (clauses directive)) 2)
                        (if (> (length (aref (clauses directive) 2)) 1)
                            (aref (aref (clauses directive) 2) 0)
                            ""))
                       (colon-p
                        ")")
                       (t
                        "")))
         (per-line-prefix-p (and (> (length (clauses directive)) 1)
                                 (at-sign-p (aref (aref (clauses directive) 0)
                                            (1- (length (aref (clauses directive) 0))))))))
    (if at-sign-p
        `((inravina:execute-logical-block ,(trinsic:client-form client) *destination*
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
        `((let* ((object (pop-argument))
                 (*remaining-argument-count* (dotted-list-length object))
                 (*previous-arguments* (make-array *remaining-argument-count*
                                                   :adjustable t :fill-pointer 0))
                 (*previous-argument-index* 0))
            (inravina:execute-logical-block ,(trinsic:client-form client) *destination*
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

(defclass i-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\I)) directive (end-directive t))
  (change-class directive 'i-directive))

(defmethod parameter-specifications ((client t) (directive i-directive))
  '((:type integer :default 0)))

(defmethod layout-requirements ((item i-directive))
  (list :logical-block))

(defmethod interpret-item (client (directive i-directive) &optional parameters)
  (declare (ignorable client parameters))
  #-sicl
  (inravina:pprint-indent client *destination*
                          (if (colon-p directive) :current :block)
                          (car parameters)))

(defmethod compile-item (client (directive i-directive) &optional parameters)
  (declare (ignorable client parameters))
  #-sicl
  `((inravina:pprint-indent ,(trinsic:client-form client) *destination*
                            ,(if (colon-p directive) :current :block)
                            ,(car parameters))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.4 ~/ Call function

;;; This directive is particular in two different ways.  First, as
;;; with the "ignored newline" directive, there are characters
;;; belonging to the directive beyond the directive character itself,
;;; which means the standard mechanism of parsing it cannot be used.
;;; Second, this directive takes an arbitrary number of parameters.

(defclass call-function-directive (directive)
  ((%function-name :accessor function-name)))

(defmethod specialize-directive
    ((client t) (char (eql #\/)) directive (end-directive t))
  (change-class directive 'call-function-directive))

(defmethod parameter-specifications (client (directive call-function-directive))
  (declare (ignore client))
  '((:type (or null character integer) :default nil :rest t)))

(defmethod parse-directive-suffix ((directive-character (eql #\/)) control-string start end)
  (let ((position-of-trailing-slash
          (position #\/ control-string :start start :end end)))
    (when (null position-of-trailing-slash)
      (error 'end-of-control-string-error
             :control-string control-string
             :tilde-position start
             :why "expected a trailing slash"))
    (1+ position-of-trailing-slash)))

(defmethod check-directive-syntax progn (client (directive call-function-directive))
  (declare (ignore client))
  ;; Check that there is at most one package marker in the function name.
  ;; Also, compute a symbol from the name.
  (with-accessors ((control-string control-string)
                   (start start)
                   (suffix-start suffix-start)
                   (end end)
                   (colon-p colon-p))
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

(defmethod interpret-item (client (directive call-function-directive) &optional parameters)
  (apply (coerce-function-designator client (function-name directive))
         *destination*
         (pop-argument)
         (colon-p directive)
         (at-sign-p directive)
         parameters))

(defmethod compile-item (client (directive call-function-directive) &optional parameters)
  `((funcall (coerce-function-designator ,(trinsic:client-form client) ',(function-name directive))
             *destination*
             (pop-argument)
             ,(colon-p directive)
             ,(at-sign-p directive)
             ,@parameters)))
