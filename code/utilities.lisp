(cl:in-package #:invistra)

(defun unique-name (&rest args)
  (gensym (apply #'concatenate 'string
                 (mapcar (lambda (arg)
                           (typecase arg
                             (symbol (symbol-name arg))
                             (string arg)
                             (otherwise (write-to-string arg))))
                         args))))

#+(or)(define-compiler-macro (&whole form &rest args)
  (if (and (consp name)
           (eq (car name) 'quote)
           (symbolp (cadr name)))
      `(gensym ,(symbol-name (cadr name)))
      form))

(defmacro with-unique-names (names &body body)
  `(let ,(mapcar (lambda (name)
                   `(,name (unique-name ',name)))
                 names)
     ,@body))

(defun dotted-list-length (list)
  (do ((n 0 (+ n 2))
       (y list (cddr y))
       (z list (cdr z)))
      ((or (null y)
           (and (eq y z) (plusp n)))
       n)
    (when (or (not (consp y))
              (null (cdr y)))
      (return (1+ n)))
    (when (not (consp (cdr y)))
      (return (+ 2 n)))))

(defclass case-conversion-stream
    (ngray:fundamental-character-output-stream)
  ((target :reader target
           :initarg :target)))

(defmethod ngray:stream-finish-output ((stream case-conversion-stream))
  (finish-output (target stream)))

(defmethod ngray:stream-force-output ((stream case-conversion-stream))
  (force-output (target stream)))

(defmethod ngray:stream-clear-output ((stream case-conversion-stream))
  (clear-output (target stream)))

(defmethod ngray:stream-terpri ((stream case-conversion-stream))
  (terpri (target stream)))

(defmethod ngray:stream-fresh-line ((stream case-conversion-stream))
  (fresh-line (target stream)))

(defclass upcase-stream (case-conversion-stream)
  ())

(defmethod ngray:stream-write-char ((stream upcase-stream) char)
  (write-char (char-upcase char) (target stream)))

(defclass downcase-stream (case-conversion-stream)
  ())

(defmethod ngray:stream-write-char ((stream downcase-stream) char)
  (write-char (char-downcase char) (target stream)))

(defclass capitalize-stream (case-conversion-stream)
  ((capitalize-next :accessor capitalize-next
                    :initform t)))

(defmethod ngray:stream-write-char ((stream capitalize-stream) char)
  (with-accessors ((capitalize-next capitalize-next))
      stream
    (let ((an (alphanumericp char)))
      (cond ((and capitalize-next an)
             (setf capitalize-next nil)
             (write-char (char-upcase char) (target stream)))
            (an
             (write-char (char-downcase char) (target stream)))
            (t
             (setf capitalize-next t)
             (write-char char (target stream)))))))

(defclass first-capitalize-stream (capitalize-stream)
  ())

(defmethod ngray:stream-write-char ((stream first-capitalize-stream) char)
  (with-accessors ((capitalize-next capitalize-next))
      stream
    (let ((an (alphanumericp char)))
      (cond ((and capitalize-next an)
             (setf capitalize-next nil)
             (write-char (char-upcase char) (target stream)))
            (an
             (write-char (char-downcase char) (target stream)))
            (t
             (write-char char (target stream)))))))

(defun write-string-with-padding (string stream pad-left-p mincol colinc minpad padchar)
  (let ((pad-length (max minpad (* colinc (ceiling (- mincol (length string)) colinc)))))
    (if pad-left-p
        (loop repeat pad-length
              finally (write-string string stream)
              do (write-char padchar stream))
        (loop repeat pad-length
                initially (write-string string stream)
              do (write-char padchar stream)))))
