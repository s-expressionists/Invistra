;;;; 22.3.2 Radix control

(in-package #:invistra)

(defclass base-radix-directive (directive)
  ())

(defmethod parameter-specifications ((client standard-client) (directive base-radix-directive))
  '((:name mincol
     :type integer
     :bind nil
     :default 0)
    (:name padchar
     :type character
     :bind nil
     :default #\Space)
    (:name commachar
     :type character
     :bind nil
     :default #\,)
    (:name comma-interval
     :type integer
     :bind nil
     :default 3)))

(defmethod calculate-argument-position (position (directive base-radix-directive))
  (setf position (call-next-method))
  (when position
    (1+ position)))

(defun format-radix-numeral
    (client colon-p at-sign-p radix mincol padchar commachar comma-interval value)
  (if (integerp value)
      (let* ((magnitude (abs value))
             (digit-count (quaviver.math:count-digits radix magnitude)))
        (loop repeat (max 0
                          (- mincol digit-count
                             (if colon-p
                                 (1- (ceiling digit-count comma-interval))
                                 0)
                             (if (or (minusp value) at-sign-p) 1 0)))
              do (write-char padchar *format-output*))
        (cond ((minusp value)
               (write-char #\- *format-output*))
              (at-sign-p
               (write-char #\+ *format-output*)))
        (if colon-p
            (quaviver:write-digits radix magnitude *format-output*
                                   :digit-grouping (vector comma-interval)
                                   :group-marker commachar)
            (quaviver:write-digits radix magnitude *format-output*)))
      (let ((*print-base* radix)
            (*print-escape* nil)
            (*print-readably* nil))
        (incless:write-object client value *format-output*))))

;;; 22.3.2.1 ~r Radix.

(defclass radix-directive (base-radix-directive) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\R)) directive (end-directive t))
  (change-class directive 'radix-directive))

(defmethod parameter-specifications ((client standard-client) (directive radix-directive))
  (list* '(:name radix
           :type (or null (integer 2 36))
           :bind nil
           :default nil)
         (call-next-method)))

(defparameter *roman-digits*
  '("I" "V" "X" "L" "C" "D" "M"))

(deftype roman-number (&optional count)
  (if count
      (multiple-value-bind (q r)
          (floor count 2)
        `(integer 1
                  ,(1- (if (zerop r)
                           (* (expt 10 (1- q)) 9)
                           (* (expt 10 q) 4)))))
      '(integer 1)))

(deftype old-roman-number (&optional count)
  (if count
      (multiple-value-bind (q r)
          (floor count 2)
        `(integer 1
                  ,(1- (* (expt 10 q)
                          (if (zerop r) 1 5)))))
      '(integer 1)))

(defun format-roman-numeral (value)
  (declare (type (roman-number 7) value))
  (labels ((write-digit (value digits)
             (multiple-value-bind (q r)
                 (floor value 10)
               (unless (zerop q)
                 (write-digit q (cddr digits)))
               (case r
                 (9
                  (write-string (car digits) *format-output*)
                  (write-string (caddr digits) *format-output*))
                 (4
                  (write-string (car digits) *format-output*)
                  (write-string (cadr digits) *format-output*))
                 (otherwise
                  (multiple-value-bind (q1 r1)
                      (floor r 5)
                    (unless (zerop q1)
                      (write-string (cadr digits) *format-output*))
                    (loop repeat r1
                          do (write-string (car digits) *format-output*))))))))
    (write-digit value *roman-digits*)))

(defun format-old-roman-numeral (value)
  (declare (type (old-roman-number 7) value))
  (labels ((write-digit (value digits)
             (multiple-value-bind (q r)
                 (floor value 10)
               (unless (zerop q)
                 (write-digit q (cddr digits)))
               (multiple-value-bind (q1 r1)
                   (floor r 5)
                 (unless (zerop q1)
                   (write-string (cadr digits) *format-output*))
                 (loop repeat r1
                       do (write-string (car digits) *format-output*))))))
    (write-digit value *roman-digits*)))

(defparameter *cardinal-ones*
  #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defparameter *cardinal-teens*
  #("ten" "eleven" "twelve" "thirteen" "fourteen"
    "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(defparameter *cardinal-tens*
  #(nil nil "twenty" "thirty" "forty"
    "fifty" "sixty" "seventy" "eighty" "ninety"))

(defparameter *scale-names*
  #(nil "thousand" "million" "billion" "trillion" "quadrillion"
    "quintillion" "sextillion" "septillion" "octillion" "nonillion"
    "decillion" "undecillion" "duodecillion" "tredecillion"
    "quattuordecillion" "quindecillion" "sexdecillion"
    "septendecillion" "octodecillion" "novemdecillion" "vigintillion"))

;;; Print a cardinal number between 1 and 99.
(defun write-cardinal-tenths (n)
  (multiple-value-bind (tens ones)
      (floor n 10)
    (case tens
      (0
       (write-string (aref *cardinal-ones* ones) *format-output*))
      (1
       (write-string (aref *cardinal-teens* ones) *format-output*))
      (otherwise
       (write-string (aref *cardinal-tens* tens) *format-output*)
       (unless (zerop ones)
         (write-char #\- *format-output*)
         (write-string (aref *cardinal-ones* ones) *format-output*))))))

;;; Print a cardinal number between 1 and 999.
(defun write-cardinal-hundreds (n)
  (multiple-value-bind (hundreds rest)
      (floor n 100)
    (case hundreds
      (0
       (write-cardinal-tenths rest))
      (otherwise
       (write-string (aref *cardinal-ones* hundreds) *format-output*)
       (write-string " hundred" *format-output*)
       (unless (zerop rest)
         (write-char #\Space *format-output*)
         (write-cardinal-tenths rest))))))

;;; Print a cardinal number n such that 0 < n < 10^65.
(defun write-cardinal-non-zero (n magnitude)
  (multiple-value-bind (thousands rest)
      (floor n 1000)
    (unless (zerop thousands)
      (write-cardinal-non-zero thousands (1+ magnitude)))
    (unless (or (zerop thousands) (zerop rest))
      (write-char #\Space *format-output*))
    (unless (zerop rest)
      (write-cardinal-hundreds rest)
      (unless (zerop magnitude)
        (write-char #\Space *format-output*)
        (write-string (aref *scale-names* magnitude) *format-output*)))))

(deftype english-number ()
  `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))

;;; Print a cardinal number n such that - 10^65 < n < 10^65.
(defun format-cardinal-numeral (n)
  (declare (type english-number n))
  (cond ((minusp n)
         (write-string "negative " *format-output*)
         (write-cardinal-non-zero (- n) 0))
        ((zerop n)
         (write-string "zero" *format-output*))
        (t
         (write-cardinal-non-zero n 0))))

(defparameter *ordinal-ones*
  #(nil "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"))

(defparameter *ordinal-teens*
  #("tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
    "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))

(defparameter *ordinal-tens*
  #(nil nil "twentieth" "thirtieth" "fortieth"
    "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

;;; Print an ordinal number between 1 and 99.
(defun write-ordinal-tenths (n)
  (multiple-value-bind (tens ones)
      (floor n 10)
    (case tens
      (0
       (write-string (aref *ordinal-ones* ones) *format-output*))
      (1
       (write-string (aref *ordinal-teens* ones) *format-output*))
      (otherwise
       (cond ((zerop ones)
              (write-string (aref *ordinal-tens* tens) *format-output*))
             (t
              (write-string (aref *cardinal-tens* tens) *format-output*)
              (write-char #\- *format-output*)
              (write-string (aref *ordinal-ones* ones) *format-output*)))))))

;;; Print an ordinal number n such that 0 < n < 1000.
(defun write-ordinal-hundreds (n)
  (multiple-value-bind (hundreds rest)
      (floor n 100)
    (case hundreds
      (0
       (write-ordinal-tenths rest))
      (otherwise
       (write-string (aref *cardinal-ones* hundreds) *format-output*)
       (write-string " hundred" *format-output*)
       (cond ((zerop rest)
              (write-string "th" *format-output*))
             (t
              (write-char #\Space *format-output*)
              (write-ordinal-tenths rest)))))))

;;; Print an ordinal number n such that 0 < n < 10^65.
(defun write-ordinal-non-zero (n)
  (multiple-value-bind (hundreds rest)
      (floor n 100)
    (cond ((zerop rest)
           ;; Hudreds is nonzero.
           (write-cardinal-non-zero n 0)
           (write-string "th" *format-output*))
          ((zerop hundreds)
           (write-ordinal-hundreds rest))
          (t
           ;; They are both nonzero.
           (write-cardinal-non-zero (* 100 hundreds) 0)
           (write-char #\Space *format-output*)
           (write-ordinal-tenths rest)))))

;;; Print an ordinal number n such that - 10^65 < n < 10^65.
(defun format-ordinal-numeral (n)
  (declare (type english-number n))
  (cond ((minusp n)
         (write-string "negative " *format-output*)
         (write-ordinal-non-zero (- n)))
        ((zerop n)
         (write-string "zeroth" *format-output*))
        (t
         (write-ordinal-non-zero n))))

(defun format-numeral
    (client colon-p at-sign-p radix mincol padchar commachar comma-interval value)
  (cond (radix
         (format-radix-numeral client colon-p at-sign-p radix mincol padchar commachar
                               comma-interval value))
        ((and at-sign-p colon-p)
         (format-old-roman-numeral value))
        (at-sign-p
         (format-roman-numeral value))
        (colon-p
         (format-ordinal-numeral value))
        (t
         (format-cardinal-numeral value))))

(defmethod interpret-item
    ((client standard-client) (directive radix-directive) &optional parameters)
  (multiple-value-call #'format-numeral
    client (colon-p directive) (at-sign-p directive) (values-list parameters) (pop-argument)))

(defmethod compile-item
    ((client standard-client) (directive radix-directive) &optional parameters)
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (let ((radix (car parameters))
          (arg-form (pop-argument-form)))
      (cond ((numberp radix)
             `((format-radix-numeral ,(trinsic:client-form client) ,colon-p ,at-sign-p
                                     ,@parameters ,arg-form)))
            ((or radix
                 (notevery #'constantp parameters))
             `((format-numeral ,(trinsic:client-form client) ,colon-p ,at-sign-p ,@parameters
                               ,arg-form)))
            ((and at-sign-p colon-p)
             `((format-old-roman-numeral ,arg-form)))
            (at-sign-p
             `((format-roman-numeral ,arg-form)))
            (colon-p
             `((format-ordinal-numeral ,arg-form)))
            (t
             `((format-cardinal-numeral ,arg-form)))))))

;;; 22.3.2.2 ~d Decimal.

(defclass decimal-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\D)) directive (end-directive t))
  (change-class directive 'decimal-directive))

(defmethod interpret-item
    ((client standard-client) (directive decimal-directive) &optional parameters)
  (multiple-value-call #'format-radix-numeral
    client (colon-p directive) (at-sign-p directive) 10 (values-list parameters)
    (pop-argument)))

(defmethod compile-item
    ((client standard-client) (directive decimal-directive) &optional parameters)
  `((format-radix-numeral ,(trinsic:client-form client) ,(colon-p directive)
                          ,(at-sign-p directive) 10 ,@parameters ,(pop-argument-form))))

;;; 22.3.2.3 ~b Binary.

(defclass binary-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\B)) directive (end-directive t))
  (change-class directive 'binary-directive))

(defmethod interpret-item
    ((client standard-client) (directive binary-directive) &optional parameters)
  (multiple-value-call #'format-radix-numeral
    client (colon-p directive) (at-sign-p directive) 2 (values-list parameters) (pop-argument)))

(defmethod compile-item
    ((client standard-client) (directive binary-directive) &optional parameters)
  `((format-radix-numeral ,(trinsic:client-form client) ,(colon-p directive)
                          ,(at-sign-p directive) 2 ,@parameters ,(pop-argument-form))))

;;; 22.3.2.4 ~o Octal.

(defclass octal-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\O)) directive (end-directive t))
  (change-class directive 'octal-directive))

(defmethod interpret-item
    ((client standard-client) (directive octal-directive) &optional parameters)
  (multiple-value-call #'format-radix-numeral
    client (colon-p directive) (at-sign-p directive) 8 (values-list parameters) (pop-argument)))

(defmethod compile-item
    ((client standard-client) (directive octal-directive) &optional parameters)
  `((format-radix-numeral ,(trinsic:client-form client) ,(colon-p directive)
                          ,(at-sign-p directive) 8 ,@parameters ,(pop-argument-form))))

;;; 22.3.2.5 ~x Hexadecimal.

(defclass hexadecimal-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\X)) directive (end-directive t))
  (change-class directive 'hexadecimal-directive))

(defmethod interpret-item
    ((client standard-client) (directive hexadecimal-directive) &optional parameters)
  (multiple-value-call #'format-radix-numeral
    client (colon-p directive) (at-sign-p directive) 16 (values-list parameters)
    (pop-argument)))

(defmethod compile-item
    ((client standard-client) (directive hexadecimal-directive) &optional parameters)
  `((format-radix-numeral ,(trinsic:client-form client) ,(colon-p directive)
                          ,(at-sign-p directive) 16 ,@parameters ,(pop-argument-form))))
