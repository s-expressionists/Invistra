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
  '((:type (or null integer) :default nil)
    (:type (or null integer) :default nil)))

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
    ((client t) (char (eql #\^)) directive (end-directive t))
  (change-class directive 'circumflex-directive))

(defmethod parameter-specifications
    ((client t) (directive circumflex-directive))
  '((:type (or null character integer))
    (:type (or null character integer))
    (:type (or null character integer))))

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
  (let ((p1 (car parameters))
        (p2 (cadr parameters))
        (p3 (caddr parameters))
        (colonp (colonp directive)))
    (cond ((and (null p1) (null p2) (null p3))
           (funcall (if colonp *outer-exit-if-exhausted* *inner-exit-if-exhausted*)))
          ((or (and (eql p1 0) (null p2) (null p3))
               (and (null p1) (eql p2 0) (null p3))
               (and (null p1) (null p2) (eql p3 0))
               (and (null p1) p2 p3 (eql p2 p3))
               (and (null p2) p1 p3 (eql p1 p3))
               (and (null p3) p1 p2 (eql p1 p2))
               (and p1 p2 p3 (<= p1 p2 p3)))
           (funcall (if colonp *outer-exit* *inner-exit*) nil)))))

(defmethod compile-item (client (directive circumflex-directive) &optional parameters)
  (let ((p1 (car parameters))
        (p2 (cadr parameters))
        (p3 (caddr parameters))
        (colonp (colonp directive)))
    (cond ((null p1)
           `((funcall ,(if colonp '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*))))
          ((null p2)
           `((let ((p1 ,p1))
               (cond ((null p1)
                      (funcall ,(if colonp '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*)))
                     ((eql 0 p1)
                      (funcall ,(if colonp '*outer-exit* '*inner-exit*) nil))))))
          ((null p3)
           `((let ((p1 ,p1)
                   (p2 ,p2))
               (cond ((and (null p1) (null p2))
                      (funcall ,(if colonp '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*)))
                     ((or (and (null p1) (eql 0 p2))
                          (and (eql 0 p1) (null p2))
                          (and p1 p2 (eql p1 p2)))
                      (funcall ,(if colonp '*outer-exit* '*inner-exit*) nil))))))
          (t
           `((let ((p1 ,p1)
                   (p2 ,p2)
                   (p3 ,p3))
               (cond ((and (null p1) (null p2) (null p3))
                      (funcall ,(if colonp '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*)))
                     ((or (and (null p1) (null p2) (eql 0 p3))
                          (and (null p1) (eql 0 p2) (null p3))
                          (and (eql 0 p1) (null p2) (null p3))
                          (and (null p1) p2 p3 (eql p2 p3))
                          (and (null p2) p1 p3 (eql p1 p3))
                          (and (null p3) p1 p2 (eql p1 p2))
                          (and p1 p2 p3 (<= p1 p2 p3)))
                      (funcall ,(if colonp '*outer-exit* '*inner-exit*) nil)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.3 ~Newline Igored newline

(defclass newline-directive
    (directive at-most-one-modifier-mixin) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\Newline)) directive (end-directive t))
  (change-class directive 'newline-directive))

(defmethod parse-directive-suffix ((directive-character (eql #\Newline)) control-string start end)
  (or (position-if (lambda (char)
                     (not (find char #(#\Space #\Tab #\Page #\Return))))
                   control-string :start start :end end)
      end))

(defmethod interpret-item (client (directive newline-directive) &optional parameters)
  (declare (ignore parameters))
  (cond ((colonp directive)
         ;; Remove the newline but print the following whitespace.
         (write-string (subseq (control-string directive) (suffix-start directive) (end directive)) *destination*))
        ((at-signp directive)
         ;; Print the newline, but remove the following whitespace.
         (write-char #\Newline *destination*))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))

(defmethod compile-item (client (directive newline-directive) &optional parameters)
  (declare (ignore parameters))
  (cond ((colonp directive)
         ;; Remove the newline but print the following whitespace.
         `((write-string ,(subseq (control-string directive) (suffix-start directive) (end directive)) *destination*)))
        ((at-signp directive)
         ;; Print the newline, but remove the following whitespace.
         `((write-char #\Newline *destination*)))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))
