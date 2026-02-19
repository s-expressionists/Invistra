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

(defmethod layout-requirements ((item conditional-newline-directive))
  (list :logical-block))

(defmethod interpret-item (client (directive conditional-newline-directive) &optional parameters)
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

(defmethod compile-item (client (directive conditional-newline-directive) &optional parameters)
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
  (error 'unmatched-directive
         :client client
         :directive directive))

(defmethod calculate-argument-position (position (directive logical-block-directive))
  (setf position (call-next-method))
  (cond ((at-sign-p directive)
         (reduce #'calculate-argument-position (aref (clauses directive)
                                                     (if (< (length (clauses directive)) 2) 0 1))
                 :initial-value position))
        (position
         (1+ position))))

(defmethod layout-requirements :around ((item logical-block-directive))
  (merge-layout-requirements (list :logical-block)
                             (call-next-method)
                             t))

(defmethod check-directive-nesting progn ((client standard-client) child (parent logical-block-directive) &optional group position)
  (when (and (> (length (clauses parent)) 1)
             (or (eql group 0)
                 (eql group 2))
             (not (stringp child))
             (not (structured-end-p child))
             (not (structured-separator-p child)))
    (error 'illegal-fix-directive
           :client client
           :directive child)))

(defmethod check-directive-syntax progn (client (directive logical-block-directive))
  (when (> (length (clauses directive)) 3)
    (error 'invalid-clause-count
           :client client
           :directive directive
           :minimum-count 1
           :maximum-count 3)))

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
    (flet ((interpret-body (*format-output* *inner-exit-if-exhausted* pop-argument-hook *more-arguments-p*)
             (if at-sign-p
                 (interpret-items client (aref (clauses directive)
                                               (if (= (length (clauses directive)) 1)
                                                   0
                                                   1)))
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
                            (let ((value (if (< position (length previous-arguments))
                                             (aref previous-arguments position)
                                             (vector-push-extend (funcall pop-argument-hook)
                                                                 previous-arguments))))
                              (unless (typep value type)
                                (error 'type-error :datum value :expected-type type))
                              (incf position))))
                        (*pop-argument* my-pop-argument-hook)
                        (*pop-remaining-arguments*
                          (lambda () nil))
                        (*go-to-argument*
                          (lambda (index absolutep)
                            (unless absolutep
                              (incf index position))
                            (cond ((not (< -1 index argument-count))
                                   (error 'go-to-out-of-bounds
                                          :what-argument index
                                          :max-arguments argument-count))
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
                                                     1)))))))
      (inravina:execute-logical-block client *format-output*
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
        `((inravina:execute-logical-block ,(trinsic:client-form client) *format-output*
                                          nil
                                          (lambda (*format-output* escape-hook pop-argument-hook more-arguments-p-hook)
                                            (declare (ignore escape-hook pop-argument-hook))
                                            ,@(compile-items client (aref (clauses directive)
                                                                          (if (= (length (clauses directive)) 1)
                                                                              0
                                                                              1))))
                                          :prefix ,prefix :suffix ,suffix
                                          :per-line-prefix-p ,per-line-prefix-p))
        (let ((arg-form (pop-argument-form)))
          (with-dynamic-arguments ()
            `((let* ((object ,arg-form)
                     (argument-count (dotted-list-length object))
                     (*previous-arguments* (make-array argument-count
                                                       :adjustable t :fill-pointer 0))
                     (*previous-argument-index* 0))
                (inravina:execute-logical-block ,(trinsic:client-form client) *format-output*
                                                object
                                                (lambda (*format-output* *inner-exit-if-exhausted* pop-argument-hook *more-arguments-p*)
                                                  (let* ((position 0)
                                                         (*remaining-argument-count* (lambda ()
                                                                                            (- argument-count position)))
                                                         (*argument-index* (lambda ()
                                                                                  position))
                                                         (*pop-argument* (lambda (&optional (type t))
                                                                                (let ((value (funcall pop-argument-hook)))
                                                                                  (unless (typep value type)
                                                                                    (error 'type-error :datum value :expected-type type))
                                                                                  value))))

                                                    ,@(compile-items client (aref (clauses directive)
                                                                                  (if (= (length (clauses directive)) 1)
                                                                                      0
                                                                                      1)))))
                                                :prefix ,prefix :suffix ,suffix
                                                :per-line-prefix-p ,per-line-prefix-p))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.3 ~i Indent

(defclass indent-directive (directive) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\I)) directive (end-directive t))
  (change-class directive 'indent-directive))

(defmethod parameter-specifications ((client t) (directive indent-directive))
  '((:type integer :default 0)))

(defmethod layout-requirements ((item indent-directive))
  (list :logical-block))

(defmethod interpret-item (client (directive indent-directive) &optional parameters)
  (declare (ignorable client parameters))
  #-sicl
  (inravina:pprint-indent client *format-output*
                          (if (colon-p directive) :current :block)
                          (car parameters)))

(defmethod compile-item (client (directive indent-directive) &optional parameters)
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
                      (error 'end-of-control-string
                             :positions (list end)))))))

(defmethod check-directive-syntax progn (client (directive call-function-directive))
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
        (error 'no-such-package
               :client client
               :directive directive
               :positions (loop for i from suffix-start below position-of-package-marker
                                collect i)
               :package-name package-name))
      (if (eq *package* package)
          (setf (function-name directive) (intern symbol-name package))
          (multiple-value-bind (symbol status)
              (find-symbol symbol-name package)
            (when (null symbol)
              (error 'no-such-symbol
                     :client client
                     :directive directive
                     :positions (loop for i from symbol-start below symbol-end
                                      collect i)
                     :symbol-name symbol-name))
            (when (and (not internalp)
                       (not (eql status :external)))
              (error 'symbol-not-external
                     :client client
                     :directive directive
                     :positions (loop for i from symbol-start below symbol-end
                                      collect i)
                     :symbol symbol))
            (setf (function-name directive) symbol))))))

(defmethod interpret-item (client (directive call-function-directive) &optional parameters)
  (apply (coerce-function-designator client (function-name directive))
         *format-output*
         (pop-argument)
         (colon-p directive)
         (at-sign-p directive)
         parameters))

(defmethod compile-item (client (directive call-function-directive) &optional parameters)
  `((funcall (coerce-function-designator ,(trinsic:client-form client) ',(function-name directive))
             *format-output*
             ,(pop-argument-form)
             ,(colon-p directive)
             ,(at-sign-p directive)
             ,@parameters)))
