;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5 Pretty printer operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.1 ~_ Conditional newline

(defclass conditional-newline-directive (directive) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\_)) directive (end-directive t))
  (change-class directive 'conditional-newline-directive))

(defmethod check-item-syntax :around
    ((client standard-client) (directive conditional-newline-directive) global-layout
     local-layout parent &optional group position)
  (call-next-method client directive global-layout
                    (merge-layout client directive global-layout local-layout :logical-block t)
                    parent group position))

(defmethod interpret-item
    ((client standard-client) (directive conditional-newline-directive) &optional parameters)
  (declare (ignore parameters)
           (ignorable client))
  #-sicl
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (inravina:pprint-newline client *format-output*
                             (cond ((and colon-p at-sign-p) :mandatory)
                                   (colon-p :fill)
                                   (at-sign-p :miser)
                                   (t :linear)))))

(defmethod compile-item
    ((client standard-client) (directive conditional-newline-directive) &optional parameters)
  (declare (ignore parameters)
           (ignorable client))
  #-sicl
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    `((inravina:pprint-newline ,(trinsic:client-form client) *format-output*
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
    ((client standard-client) (char (eql #\<)) directive
     (end-directive end-logical-block-directive))
  (change-class directive 'logical-block-directive))

(defmethod specialize-directive
    ((client standard-client) (char (eql #\<)) directive (end-directive t))
  (signal-missing-end-logical-block-or-end-justification client directive))

(defmethod calculate-argument-position (position (directive logical-block-directive))
  (setf position (call-next-method))
  (cond ((at-sign-p directive)
         (reduce #'calculate-argument-position (aref (clauses directive)
                                                     (if (< (length (clauses directive)) 2)
                                                         0
                                                         1))
                 :initial-value position))
        (position
         (1+ position))))

(defmethod check-item-syntax :around
    ((client standard-client) (directive logical-block-directive) global-layout local-layout
     parent &optional group position)
  (call-next-method client directive global-layout
                    (merge-layout client directive global-layout local-layout :logical-block t)
                    parent group position))

(defmethod check-item-syntax progn
    ((client standard-client) (directive directive) global-layout local-layout
     (parent logical-block-directive) &optional group position)
  (declare (ignore global-layout local-layout position))
  (when (and (> (length (clauses parent)) 1)
             (or (eql group 0)
                 (eql group 2))
             (not (structured-end-p directive))
             (not (structured-separator-p directive)))
    (signal-illegal-fix-directive client directive)))

(defmethod check-item-syntax progn
    ((client standard-client) (directive logical-block-directive) global-layout local-layout
     parent &optional group position)
  (declare (ignore global-layout local-layout parent group position))
  (check-clause-count client directive 1 3))

(defmethod interpret-item
    ((client standard-client) (directive logical-block-directive) &optional parameters)
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
         (object (if at-sign-p (pop-remaining-arguments) (pop-argument))))
    (inravina:execute-logical-block
     client *format-output*
     object
     (lambda (*format-output* inner-exit-if-exhausted-hook pop-argument-hook more-arguments-p-hook)
       (let* ((argument-count (dotted-list-length object))
              (previous-arguments (make-array argument-count
                                              :adjustable t :fill-pointer 0))
              (position 0)
              (*argument-index*
                (lambda () position))
              (*remaining-argument-count*
                (lambda () (- argument-count position)))
              (my-pop-argument-hook
                (lambda (&optional (type t))
                  (let (value)
                    (if (< position (length previous-arguments))
                        (setf value (aref previous-arguments position))
                        (vector-push-extend (setf value (funcall pop-argument-hook))
                                            previous-arguments))
                    (unless (typep value type)
                      (error 'type-error :datum value :expected-type type))
                    (incf position)
                    value)))
              (*pop-argument* my-pop-argument-hook)
              (*pop-remaining-arguments*
                (lambda () nil))
              (*more-arguments-p*
                (lambda ()
                  (or (< position (length previous-arguments))
                      (funcall more-arguments-p-hook))))
              (*inner-exit-if-exhausted*
                (lambda ()
                  (unless (< position (length previous-arguments))
                    (funcall inner-exit-if-exhausted-hook))))
              (*go-to-argument*
                (lambda (index absolutep)
                  (unless absolutep
                    (incf index position))
                  (cond ((not (< -1 index argument-count))
                         (error 'go-to-out-of-bounds
                                :argument-position index
                                :argument-count argument-count))
                        ((<= index position)
                         (setf position index))
                        (t
                         (tagbody
                          next
                            (when (< position index)
                              (funcall my-pop-argument-hook)
                              (go next))))))))
         (interpret-items client (aref (clauses directive)
                                       (if (= (length (clauses directive)) 1)
                                           0
                                           1)))))
     :prefix prefix
     :per-line-prefix-p per-line-prefix-p
     :suffix suffix)))

(defmethod compile-item
    ((client standard-client) (directive logical-block-directive) &optional parameters)
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
         (arg-form (if at-sign-p (pop-remaining-arguments-form) (pop-argument-form))))
    (with-dynamic-arguments ()
      `((let* ((object ,arg-form)
               (argument-count (dotted-list-length object))
               (previous-arguments (make-array argument-count
                                               :adjustable t :fill-pointer 0))
               (position 0)
               (*remaining-argument-count*
                 (lambda ()
                   (- argument-count position)))
               (*argument-index*
                 (lambda ()
                   position))
               (*pop-remaining-arguments*
                 (lambda () nil)))
          (inravina:execute-logical-block
           ,(trinsic:client-form client) *format-output*
           object
           (lambda
               (*format-output* inner-exit-if-exhausted-hook pop-argument-hook
                more-arguments-p-hook)
             (let* ((my-pop-argument-hook
                      (lambda (&optional (type t))
                        (let (value)
                          (if (< position (length previous-arguments))
                              (setf value (aref previous-arguments position))
                              (vector-push-extend (setf value (funcall pop-argument-hook))
                                                  previous-arguments))
                          (unless (typep value type)
                            (error 'type-error :datum value :expected-type type))
                          (incf position)
                          value)))
                    (*pop-argument* my-pop-argument-hook)
                    (*more-arguments-p*
                      (lambda ()
                        (or (< position (length previous-arguments))
                            (funcall more-arguments-p-hook))))
                    (*inner-exit-if-exhausted*
                      (lambda ()
                        (unless (< position (length previous-arguments))
                          (funcall inner-exit-if-exhausted-hook))))
                    (*go-to-argument*
                      (lambda (index absolutep)
                        (unless absolutep
                          (incf index position))
                        (cond ((not (< -1 index argument-count))
                               (error 'go-to-out-of-bounds
                                      :argument-position index
                                      :argument-count argument-count))
                              ((<= index position)
                               (setf position index))
                              (t
                               (tagbody
                                next
                                  (when (< position index)
                                    (funcall my-pop-argument-hook)
                                    (go next))))))))
               ,@(compile-items client (aref (clauses directive)
                                             (if (= (length (clauses directive)) 1)
                                                 0
                                                 1)))))
           :prefix ,prefix :suffix ,suffix
           :per-line-prefix-p ,per-line-prefix-p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.3 ~i Indent

(defclass indent-directive (directive) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\I)) directive (end-directive t))
  (change-class directive 'indent-directive))

(defmethod parameter-specifications ((client t) (directive indent-directive))
  '((:type integer :default 0)))

(defmethod check-item-syntax :around
    ((client standard-client) (directive indent-directive) global-layout local-layout parent
     &optional group position)
  (call-next-method client directive global-layout
                    (merge-layout client directive global-layout local-layout :logical-block t)
                    parent group position))

(defmethod interpret-item ((client standard-client) (directive indent-directive) &optional parameters)
  (declare (ignorable client parameters))
  #-sicl
  (inravina:pprint-indent client *format-output*
                          (if (colon-p directive) :current :block)
                          (car parameters)))

(defmethod compile-item ((client standard-client) (directive indent-directive) &optional parameters)
  (declare (ignorable client parameters))
  #-sicl
  `((inravina:pprint-indent ,(trinsic:client-form client) *format-output*
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
    ((client standard-client) (char (eql #\/)) directive (end-directive t))
  (change-class directive 'call-function-directive))

(defmethod parameter-specifications (client (directive call-function-directive))
  (declare (ignore client))
  '((:type (or null character integer) :default nil :rest t)))

(defmethod parse-suffix ((client standard-client) directive (directive-character (eql #\/)))
  (with-accessors ((control-string control-string)
                   (end end))
      directive
    (setf end (1+ (or (position #\/ control-string :start end)
                      (signal-end-of-control-string client directive))))))

(defmethod check-item-syntax progn
    ((client standard-client) (directive call-function-directive) global-layout local-layout parent
     &optional group position)
  (declare (ignore global-layout local-layout parent group position))
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
           (internalp (or (null position-of-package-marker)
                          (char= #\: (char control-string (1+ position-of-package-marker)))))
           (symbol-start (cond ((null position-of-package-marker)
                                suffix-start)
                               (internalp
                                (+ 2 position-of-package-marker))
                               (t
                                (1+ position-of-package-marker))))
           (symbol-end (1- end))
           (symbol-name (string-upcase (subseq control-string symbol-start symbol-end)))
           (package (find-package package-name)))
      (when (null package)
        (signal-no-such-package client directive package-name suffix-start position-of-package-marker))
      (if (eq *package* package)
          (setf (function-name directive) (intern symbol-name package))
          (multiple-value-bind (symbol status)
              (find-symbol symbol-name package)
            (when (null symbol)
              (signal-no-such-symbol client directive symbol-name symbol-start symbol-end))
            (when (and (not internalp)
                       (not (eql status :external)))
              (signal-symbol-not-external client directive symbol symbol-start symbol-end))
            (setf (function-name directive) symbol))))))

(defmethod interpret-item
    ((client standard-client) (directive call-function-directive) &optional parameters)
  (apply (coerce-function-designator client (function-name directive))
         *format-output*
         (pop-argument)
         (colon-p directive)
         (at-sign-p directive)
         parameters))

(defmethod compile-item
    ((client standard-client) (directive call-function-directive) &optional parameters)
  `((funcall (coerce-function-designator ,(trinsic:client-form client)
                                         ',(function-name directive))
             *format-output*
             ,(pop-argument-form)
             ,(colon-p directive)
             ,(at-sign-p directive)
             ,@parameters)))
