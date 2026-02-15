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

(defmethod valid-nesting-p ((client standard-client) (child clause-separator-directive) (parent conditional-expression-directive))
  t)

(defmethod valid-nesting-p ((client standard-client) (child clause-separator-directive) (parent justification-directive))
  t)

(defmethod valid-nesting-p ((client standard-client) (child clause-separator-directive) (parent logical-block-directive))
  t)

(defmethod valid-nesting-p ((client standard-client) (child clause-separator-directive) parent)
  (declare (ignore parent))
  nil)

(defmethod interpret-item ((client standard-client) (directive clause-separator-directive) &optional parameters)
  (let ((extra-space (car parameters))
        (line-length (cadr parameters)))
    (when extra-space
      (setf *extra-space* extra-space))
    (when line-length
      (setf *line-length* line-length))))

(defmethod compile-item ((client standard-client) (directive clause-separator-directive) &optional parameters)
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

(defmethod valid-nesting-p ((client standard-client) (child escape-upward-directive) parent)
  (not (colon-p child)))

(defmethod valid-nesting-p ((client standard-client) (child escape-upward-directive) (parent iteration-directive))
  (or (not (colon-p child))
      (colon-p parent)))

(defmethod check-directive-syntax progn
    (client (directive escape-upward-directive))
  (declare (ignore client))
  (let ((parameters (parameters directive)))
    (when (and (second parameters) (not (first parameters)))
      (error 'parameter-omitted
             :parameter1 1
             :parameter2 2))
    (when (and (third parameters) (not (second parameters)))
      (error 'parameter-omitted
             :parameter2 2
             :parameter3 3))))

(defmethod interpret-item (client (directive escape-upward-directive) &optional parameters)
  (with-accessors ((colon-p colon-p))
      directive
    (destructuring-bind (p1 p2 p3)
        parameters
      (cond ((and (null p1) (null p2) (null p3))
             (funcall (if colon-p *outer-exit-if-exhausted* *inner-exit-if-exhausted*)))
            ((or (and (eql p1 0) (null p2) (null p3))
                 (and (null p1) (eql p2 0) (null p3))
                 (and (null p1) (null p2) (eql p3 0))
                 (and (null p1) p2 p3 (eql p2 p3))
                 (and (null p2) p1 p3 (eql p1 p3))
                 (and (null p3) p1 p2 (eql p1 p2))
                 (and p1 p2 p3 (<= p1 p2 p3)))
             (funcall (if colon-p *outer-exit* *inner-exit*)))))))

(defmethod compile-item (client (directive escape-upward-directive) &optional parameters)
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
        (cond ((null p1)
               exit-if-exhausted-forms)
              ((null p2)
               `((cond ((null ,p1)
                        ,@exit-if-exhausted-forms)
                       ((eql 0 ,p1)
                        ,@exit-forms))))
            ((null p3)
             `((cond ((and (null ,p1) (null ,p2))
                      ,@exit-if-exhausted-forms)
                     ((or (and (null ,p1) (eql 0 ,p2))
                          (and (eql 0 ,p1) (null ,p2))
                          (and ,p1 ,p2 (eql ,p1 ,p2)))
                      ,@exit-forms))))
            (t
             `((cond ((and (null ,p1) (null ,p2) (null ,p3))
                      ,@exit-if-exhausted-forms)
                     ((or (and (null ,p1) (null ,p2) (eql 0 ,p3))
                          (and (null ,p1) (eql 0 ,p2) (null ,p3))
                          (and (eql 0 ,p1) (null ,p2) (null ,p3))
                          (and (null ,p1) ,p2 ,p3 (eql ,p2 ,p3))
                          (and (null ,p2) ,p1 ,p3 (eql ,p1 ,p3))
                          (and (null ,p3) ,p1 ,p2 (eql ,p1 ,p2))
                          (and ,p1 ,p2 ,p3 (<= ,p1 ,p2 ,p3)))
                      ,@exit-forms)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.3 ~Newline Igored newline

(defclass ignored-newline-directive
    (directive at-most-one-modifier-mixin) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\Newline)) directive (end-directive t))
  (change-class directive 'ignored-newline-directive))

(defmethod parse-suffix ((client standard-client) directive (directive-character (eql #\Newline)) control-string)
  (setf (end directive)
        (or (position-if (lambda (char)
                           (not (find char #(#\Space #\Tab #\Page #\Return))))
                         control-string :start (end directive))
            (length control-string))))

(defmethod interpret-item (client (directive ignored-newline-directive) &optional parameters)
  (declare (ignore parameters))
  (cond ((colon-p directive)
         ;; Remove the newline but print the following whitespace.
         (write-string (subseq (control-string directive) (suffix-start directive) (end directive)) *format-output*))
        ((at-sign-p directive)
         ;; Print the newline, but remove the following whitespace.
         (terpri *format-output*))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))

(defmethod compile-item (client (directive ignored-newline-directive) &optional parameters)
  (declare (ignore parameters))
  (cond ((colon-p directive)
         ;; Remove the newline but print the following whitespace.
         `((write-string ,(subseq (control-string directive) (suffix-start directive) (end directive)) *format-output*)))
        ((at-sign-p directive)
         ;; Print the newline, but remove the following whitespace.
         `((terpri *format-output*)))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))
