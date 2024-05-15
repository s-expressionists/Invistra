(cl:in-package #:invistra)

;;; For certain common types used by FORMAT, return a string
;;; explaining in English what the type means.  For other
;;; types, return a string "an object of type <type>"
(defun type-name (type)
  (cond ((symbolp type)
         (case type
           (integer "an integer")
           (character "a character")
           (list "a list")
           (t (cl:format nil "an object of type ~s" type))))
        ((and (consp type) (eq (car type) 'integer))
         (case (length type)
           (1 "an integer")
           (2 (case (second type)
                (0 "a nonnegative integer")
                (1 "a strictly positive integer")
                (t (cl:format nil "an integer greater than or equal to ~d" (second type)))))
           (3 (cl:format nil "an integer between ~d and ~d" (second type) (third type)))
           (t (cl:format nil "an object of type ~s" type))))
        (t (cl:format nil "an object of type ~s" type))))

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

(defun write-string-with-padding (string pad-left-p mincol colinc minpad padchar)
  (let ((pad-length (max minpad (* colinc (ceiling (- mincol (length string)) colinc)))))
    (if pad-left-p
        (loop repeat pad-length
              finally (write-string string *destination*)
              do (write-char padchar *destination*))
        (loop repeat pad-length
                initially (write-string string *destination*)
              do (write-char padchar *destination*)))))
