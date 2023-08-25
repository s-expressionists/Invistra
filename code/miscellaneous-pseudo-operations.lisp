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

(define-directive t #\;
    semicolon-directive
    t
    (named-parameters-directive)
    ((extra-space :type (or null integer) :default-value nil)
     (line-length :type (or null integer) :default-value nil)))

(defmethod structured-separator-p ((directive semicolon-directive))
  t)

(define-format-directive-interpreter semicolon-directive
  (when extra-space
    (setf *extra-space* extra-space))
  (when line-length
    (setf *line-length* line-length)))

(define-format-directive-compiler semicolon-directive
  `((when extra-space
      (setf *extra-space* extra-space))
    (when line-length
      (setf *line-length* line-length))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.2 ~^ Escape upward

(define-directive t #\^ circumflex-directive t (named-parameters-directive)
    ((p1 :type (or character integer))
     (p2 :type (or character integer))
     (p3 :type (or character integer))))

(defmethod check-directive-syntax progn
    (client (directive circumflex-directive))
  (declare (ignore client))
  (let ((parameters (given-parameters directive)))
    (when (and (second parameters) (not (first parameters)))
      (error 'parameter-omitted
             :parameter1 1
             :parameter2 2))
    (when (and (third parameters) (not (second parameters)))
      (error 'parameter-omitted
             :parameter2 2
             :parameter3 3))))

(define-format-directive-interpreter circumflex-directive
  (cond ((and (null p1) (null p2) (null p3))
         (funcall (if colonp *outer-exit-if-exhausted* *inner-exit-if-exhausted*)))
        ((or (and (eql p1 0) (null p2) (null p3))
             (and (null p1) (eql p2 0) (null p3))
             (and (null p1) (null p2) (eql p3 0))
             (and (null p1) p2 p3 (eql p2 p3))
             (and (null p2) p1 p3 (eql p1 p3))
             (and (null p3) p1 p2 (eql p1 p2))
             (and p1 p2 p3 (<= p1 p2 p3)))
         (funcall (if colonp *outer-exit* *inner-exit*) nil))))

(define-format-directive-compiler circumflex-directive
  (cond ((null p1)
         `((funcall ,(if colonp '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*))))
        ((null p2)
         `((cond ((null p1)
                  (funcall ,(if colonp '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*)))
                 ((eql 0 p1)
                  (funcall ,(if colonp '*outer-exit* '*inner-exit*) nil)))))
        ((null p3)
         `((cond ((and (null p1) (null p2))
                  (funcall ,(if colonp '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*)))
                 ((or (and (null p1) (eql 0 p2))
                      (and (eql 0 p1) (null p2))
                      (and p1 p2 (eql p1 p2)))
                  (funcall ,(if colonp '*outer-exit* '*inner-exit*) nil)))))
        (t
         `((cond ((and (null p1) (null p2) (null p3))
                  (funcall ,(if colonp '*outer-exit-if-exhausted* '*inner-exit-if-exhausted*)))
                 ((or (and (null p1) (null p2) (eql 0 p3))
                      (and (null p1) (eql 0 p2) (null p3))
                      (and (eql 0 p1) (null p2) (null p3))
                      (and (null p1) p2 p3 (eql p2 p3))
                      (and (null p2) p1 p3 (eql p1 p3))
                      (and (null p3) p1 p2 (eql p1 p2))
                      (and p1 p2 p3 (<= p1 p2 p3)))
                  (funcall ,(if colonp '*outer-exit* '*inner-exit*) nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.3 ~Newline Igored newline

(define-directive t #\Newline newline-directive t (named-parameters-directive at-most-one-modifier-mixin) ())

(defmethod parse-directive-suffix ((directive-character (eql #\Newline)) control-string start end)
  (or (position-if (lambda (char)
                     (not (find char #(#\Space #\Tab #\Page #\Return))))
                   control-string :start start :end end)
      end))

(define-format-directive-interpreter newline-directive
  (cond (colonp
         ;; Remove the newline but print the following whitespace.
         (write-string (subseq control-string suffix-start end) *destination*))
        (at-signp
         ;; Print the newline, but remove the following whitespace.
         (write-char #\Newline *destination*))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))

(define-format-directive-compiler newline-directive
  (cond (colonp
         ;; Remove the newline but print the following whitespace.
         `((write-string ,(subseq control-string suffix-start end) *destination*)))
        (at-signp
         ;; Print the newline, but remove the following whitespace.
         `((write-char #\Newline *destination*)))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))
