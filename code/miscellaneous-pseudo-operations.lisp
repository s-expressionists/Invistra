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

(defclass semicolon-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\;)) directive (end-directive t))
  (change-class directive 'semicolon-directive))

(defmethod parameter-specifications
    ((client t) (directive semicolon-directive))
  '((:name *extra-space*
     :type (or null integer)
     :bind nil)
    (:name *line-length*
     :type (or null integer)
     :bind nil)))

(defmethod structured-separator-p ((directive semicolon-directive))
  t)

(defmethod interpret-item (client (directive semicolon-directive) &optional parameters)
  (let ((extra-space (car parameters))
        (line-length (cadr parameters)))
    (when extra-space
      (setf *extra-space* extra-space))
    (when line-length
      (setf *line-length* line-length))))

(defmethod compile-item (client (directive semicolon-directive) &optional parameters)
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

(defclass circumflex-directive (directive) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\^)) directive (end-directive t))
  (change-class directive 'circumflex-directive))

(defmethod parameter-specifications
    ((client t) (directive circumflex-directive))
  '((:name p1 :type (or null character integer))
    (:name p2 :type (or null character integer))
    (:name p3 :type (or null character integer))))

(defmethod check-directive-syntax progn
    (client (directive circumflex-directive))
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

(defmethod interpret-item (client (directive circumflex-directive) &optional parameters)
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
             (funcall (if colon-p *outer-exit* *inner-exit*) nil))))))

(defmethod compile-item (client (directive circumflex-directive) &optional parameters)
  (with-accessors ((colon-p colon-p))
      directive
    (destructuring-bind (p1 p2 p3)
        parameters
      (cond ((null p1)
             `((funcall ,(if colon-p '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*))))
            ((null p2)
             `((cond ((null ,p1)
                      (funcall ,(if colon-p '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*)))
                     ((eql 0 ,p1)
                      (funcall ,(if colon-p '*outer-exit* '*inner-exit*) nil)))))
            ((null p3)
             `((cond ((and (null ,p1) (null ,p2))
                      (funcall ,(if colon-p '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*)))
                     ((or (and (null ,p1) (eql 0 ,p2))
                          (and (eql 0 ,p1) (null ,p2))
                          (and ,p1 ,p2 (eql ,p1 ,p2)))
                      (funcall ,(if colon-p '*outer-exit* '*inner-exit*) nil)))))
            (t
             `((cond ((and (null ,p1) (null ,p2) (null ,p3))
                      (funcall ,(if colon-p '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*)))
                     ((or (and (null ,p1) (null ,p2) (eql 0 ,p3))
                          (and (null ,p1) (eql 0 ,p2) (null ,p3))
                          (and (eql 0 ,p1) (null ,p2) (null ,p3))
                          (and (null ,p1) ,p2 ,p3 (eql ,p2 ,p3))
                          (and (null ,p2) ,p1 ,p3 (eql ,p1 ,p3))
                          (and (null ,p3) ,p1 ,p2 (eql ,p1 ,p2))
                          (and ,p1 ,p2 ,p3 (<= ,p1 ,p2 ,p3)))
                      (funcall ,(if colon-p '*outer-exit* '*inner-exit*) nil)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.3 ~Newline Igored newline

(defclass newline-directive
    (directive at-most-one-modifier-mixin) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\Newline)) directive (end-directive t))
  (change-class directive 'newline-directive))

(defmethod parse-directive-suffix ((directive-character (eql #\Newline)) control-string start end)
  (or (position-if (lambda (char)
                     (not (find char #(#\Space #\Tab #\Page #\Return))))
                   control-string :start start :end end)
      end))

(defmethod interpret-item (client (directive newline-directive) &optional parameters)
  (declare (ignore parameters))
  (cond ((colon-p directive)
         ;; Remove the newline but print the following whitespace.
         (write-string (subseq (control-string directive) (suffix-start directive) (end directive)) *destination*))
        ((at-sign-p directive)
         ;; Print the newline, but remove the following whitespace.
         (terpri *destination*))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))

(defmethod compile-item (client (directive newline-directive) &optional parameters)
  (declare (ignore parameters))
  (cond ((colon-p directive)
         ;; Remove the newline but print the following whitespace.
         `((write-string ,(subseq (control-string directive) (suffix-start directive) (end directive)) *destination*)))
        ((at-sign-p directive)
         ;; Print the newline, but remove the following whitespace.
         `((terpri *destination*)))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))
