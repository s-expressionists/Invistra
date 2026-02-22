;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9 Miscellaneous pseudo-operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.1 ~; Clause separator

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.1 ~; Clause separator

(defclass clause-separator-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\;)) directive (end-directive t))
  (change-class directive 'clause-separator-directive))

(defmethod parameter-specifications
    ((client t) (directive clause-separator-directive))
  '((:name *extra-space*
     :type (or null integer)
     :bind nil)
    (:name *line-length*
     :type (or null integer)
     :bind nil)))

(defmethod structured-separator-p ((directive clause-separator-directive))
  t)

(defmethod check-item-syntax progn
    ((client standard-client) (directive clause-separator-directive)
     global-layout local-layout (parent iteration-directive)
     &optional group position)
  (declare (ignore global-layout local-layout group position))
  (signal-illegal-clause-separator client directive))

(defmethod check-item-syntax progn
    ((client standard-client) (directive clause-separator-directive)
     global-layout local-layout (parent case-conversion-directive)
     &optional group position)
  (declare (ignore global-layout local-layout group position))
  (signal-illegal-clause-separator client directive))

(defmethod interpret-item
    ((client standard-client) (directive clause-separator-directive) &optional parameters)
  (let ((extra-space (car parameters))
        (line-length (cadr parameters)))
    (when extra-space
      (setf *extra-space* extra-space))
    (when line-length
      (setf *line-length* line-length))))

(defmethod compile-item
    ((client standard-client) (directive clause-separator-directive) &optional parameters)
  (let ((extra-space (car parameters))
        (line-length (cadr parameters)))
    `(,@(cond ((numberp extra-space)
               `((setf *extra-space* ,extra-space)))
              (extra-space
               `((setf *extra-space* (or ,extra-space
                                         *extra-space*)))))
      ,@(cond ((numberp line-length)
               `((setf *line-length* ,line-length)))
              (line-length
               `((setf *line-length* (or ,line-length
                                         *line-length*))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.2 ~^ Escape upward

(defclass escape-upward-directive (directive) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\^)) directive (end-directive t))
  (change-class directive 'escape-upward-directive))

(defmethod parameter-specifications
    ((client t) (directive escape-upward-directive))
  '((:name p1 :type (or null character integer))
    (:name p2 :type (or null character integer))
    (:name p3 :type (or null character integer))))

(defmethod check-item-syntax progn
    ((client standard-client) (directive escape-upward-directive) global-layout local-layout parent
     &optional group position)
  (declare (ignore global-layout local-layout group position))
  (when (and (colon-p directive)
             (or (not (typep parent 'iteration-directive))
                 (not (colon-p parent))))
    (signal-illegal-outer-modifier client directive)))

(defmethod interpret-item
    ((client standard-client) (directive escape-upward-directive) &optional parameters)
  (with-accessors ((colon-p colon-p))
      directive
    (destructuring-bind (p1 p2 p3)
        parameters
      (cond ((and (null p1) (null p2) (null p3))
             (funcall (if colon-p *outer-exit-if-exhausted* *inner-exit-if-exhausted*)))
            ((or (and p3
                      (<= p1 p2 p3))
                 (and (null p3)
                      (or (and (null p2)
                               (eql p1 0))
                          (and p2
                               (eql p1 p2)))))
             (funcall (if colon-p *outer-exit* *inner-exit*)))))))

(defmethod compile-item
    ((client standard-client) (directive escape-upward-directive) &optional parameters)
  (with-accessors ((colon-p colon-p))
      directive
    (destructuring-bind (p1 p2 p3)
        parameters
      (let ((exit-forms (if colon-p
                            (outer-exit-forms)
                            (inner-exit-forms)))
            (exit-if-exhausted-forms (if colon-p
                                         (outer-exit-if-exhausted-forms)
                                         (inner-exit-if-exhausted-forms))))
        (cond ((and (null p1) (null p2) (null p3))
               exit-if-exhausted-forms)
              ((and (null p2) (null p3))
               `((cond ((null ,p1)
                        ,@exit-if-exhausted-forms)
                       ((eql 0 ,p1)
                        ,@exit-forms))))
              ((null p3)
               `((cond ((and (null ,p1) (null ,p2))
                        ,@exit-if-exhausted-forms)
                       ((or (and (null ,p2)
                                 (eql ,p1 0))
                            (and ,p2
                                 (eql ,p1 ,p2)))
                        ,@exit-forms))))
              (t
               `((cond ((and (null ,p1) (null ,p2) (null ,p3))
                        ,@exit-if-exhausted-forms)
                       ((or (and ,p3
                                 (<= ,p1 ,p2 ,p3))
                            (and (null ,p3)
                                 (or (and (null ,p2)
                                          (eql ,p1 0))
                                     (and ,p2
                                          (eql ,p1 ,p2)))))
                        ,@exit-forms)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.3 ~Newline Igored newline

(defclass ignored-newline-directive
    (directive at-most-one-modifier-mixin) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\Newline)) directive (end-directive t))
  (change-class directive 'ignored-newline-directive))

(defmethod whitespace-char-p ((client standard-client) ch)
  #+ccl (ccl::whitespacep ch)
  #+clasp
    (eq (core:syntax-type *readtable* ch) :whitespace)
  #+cmucl (lisp::whitespacep ch)
  #+(and ecl (not bytecode))
    (ffi::c-inline (ch) (t) :bool
                   "ecl_readtable_get(ecl_current_readtable(), ECL_CHAR_CODE(#0), NULL) == cat_whitespace"
                            :one-liner t)
  #+sicl (eq (eclector.readtable:syntax-type *readtable* ch) :whitespace)
  #+sbcl (sb-impl::whitespace[2]p ch *readtable*)
  #-(or ccl clasp cmucl (and ecl (not bytecode)) sbcl)
    (and (member ch '(#\tab #\newline #\linefeed #\page #\return #\space))
         t))

(defmethod parse-suffix
    ((client standard-client) directive (directive-character (eql #\Newline)))
  (with-accessors ((control-string control-string)
                   (end end))
      directive
    (loop while (and (< end (length control-string))
                     (char/= #\newline (char control-string end))
                     (whitespace-char-p client (char control-string end)))
          do (incf end))))

(defmethod interpret-item
    ((client standard-client) (directive ignored-newline-directive) &optional parameters)
  (declare (ignore parameters))
  (cond ((colon-p directive)
         ;; Remove the newline but print the following whitespace.
         (write-string (subseq (control-string directive) (suffix-start directive)
                               (end directive)) *format-output*))
        ((at-sign-p directive)
         ;; Print the newline, but remove the following whitespace.
         (terpri *format-output*))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))

(defmethod compile-item
    ((client standard-client) (directive ignored-newline-directive) &optional parameters)
  (declare (ignore parameters))
  (cond ((colon-p directive)
         ;; Remove the newline but print the following whitespace.
         `((write-string ,(subseq (control-string directive) (suffix-start directive)
                                  (end directive)) *format-output*)))
        ((at-sign-p directive)
         ;; Print the newline, but remove the following whitespace.
         `((terpri *format-output*)))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))
