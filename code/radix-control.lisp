(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2 Radix control

(defclass base-radix-directive (directive)
  ())

(defmethod parameter-specifications (client (directive base-radix-directive))
  (declare (ignore client))
  '((:name mincol
     :type integer
     :default 0)
    (:name padchar
     :type character
     :default #\Space)
    (:name commachar
     :type character
     :default #\,)
    (:name comma-interval
     :type integer
     :default 3)))

(defmethod calculate-argument-position (position (directive base-radix-directive))
  (setf position (call-next-method))
  (when position
    (1+ position)))

(defun write-radix-numeral
    (client value colon-p at-sign-p radix mincol padchar commachar comma-interval)
  (if (not (integerp value))
      (let ((*print-base* radix)
            (*print-escape* nil)
            (*print-readably* nil))
        (incless:write-object client value *format-output*))
      (let* ((string (let ((*print-base* radix)
                           (*print-radix* nil)
                           (*print-escape* nil)
                           (*print-readably* nil))
                       (with-output-to-string (stream)
                         (incless:write-object client (abs value) stream))))
             (comma-length (if colon-p
                               (max 0 (floor (1- (length string)) comma-interval))
                               0))
             (sign-length (if (or at-sign-p (minusp value)) 1 0))
             (total-length (+ (length string) comma-length sign-length))
             (pad-length (max 0 (- mincol total-length))))
        ;; Print the padding.
        (loop repeat pad-length
              do (write-char padchar *format-output*))
        ;; Possibliy print a sign.
        (cond ((minusp value)
               (write-char #\- *format-output*))
              (at-sign-p
               (write-char #\+ *format-output*))
              (t nil))
        ;; Print the string in reverse order
        (loop for index downfrom (1- (length string)) to 0
              for c across string
              do (write-char c *format-output*)
              do (when (and colon-p
                            (plusp index)
                            (zerop (mod index comma-interval)))
                   (write-char commachar *format-output*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.1 ~r Radix.

(defclass radix-directive (base-radix-directive) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\R)) directive (end-directive t))
  (change-class directive 'radix-directive))

(defmethod parameter-specifications ((client t) (directive radix-directive))
  (list* '(:name radix
           :type (or null (integer 2 36))
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

(defun write-roman-numeral (value)
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

(defun write-old-roman-numeral (value)
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

(defparameter *groups-of-three*
  #(nil "thousand" "million" "billion" "trillion" "quadrillion"
    "quintillion" "sextillion" "septillion" "octillion" "nonillion"
    "decillion" "undecillion" "duodecillion" "tredecillion"
    "quattuordecillion" "quindecillion" "sexdecillion"
    "septendecillion" "octodecillion" "novemdecillion" "vigintillion"))

;;; Print a cardinal number between 1 and 99.
(defun write-cardinal-tenths (n)
  (cond ((< n 10)
         (write-string (aref *cardinal-ones* n) *format-output*))
        ((< n 20)
         (write-string (aref *cardinal-teens* (- n 10)) *format-output*))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (write-string (aref *cardinal-tens* tens) *format-output*)
           (unless (zerop ones)
             (write-char #\- *format-output*)
             (write-string (aref *cardinal-ones* ones) *format-output*))))))

;;; Print a cardinal number between 1 and 999.
(defun write-cardinal-hundreds (n)
  (cond ((< n 100)
         (write-cardinal-tenths n))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (write-string (aref *cardinal-ones* hundreds) *format-output*)
           (write-string " hundred" *format-output*)
           (unless (zerop rest)
             (write-char #\Space *format-output*)
             (write-cardinal-tenths rest))))))

;;; Print a cardinal number n such that 0 < n < 10^65.
(defun write-cardinal-non-zero (n magnitude)
  (multiple-value-bind (thousands rest) (floor n 1000)
    (unless (zerop thousands)
      (write-cardinal-non-zero thousands (1+ magnitude)))
    (unless (or (zerop thousands) (zerop rest))
      (write-char #\Space *format-output*))
    (unless (zerop rest)
      (write-cardinal-hundreds rest)
      (unless (zerop magnitude)
        (write-char #\Space *format-output*)
        (write-string (aref *groups-of-three* magnitude) *format-output*)))))

(deftype english-number ()
  `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))

;;; Print a cardinal number n such that - 10^65 < n < 10^65.
(defun write-cardinal-numeral (n)
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
  (cond ((< n 10)
         (write-string (aref *ordinal-ones* n) *format-output*))
        ((< n 20)
         (write-string (aref *ordinal-teens* (- n 10)) *format-output*))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (cond ((zerop ones)
                  (write-string (aref *ordinal-tens* tens) *format-output*))
                 (t
                  (write-string (aref *cardinal-tens* tens) *format-output*)
                  (write-char #\- *format-output*)
                  (write-string (aref *ordinal-ones* ones) *format-output*)))))))

;;; Print an ordinal number n such that 0 < n < 1000.
(defun write-ordinal-hundreds (n)
  (cond ((< n 100)
         (write-ordinal-tenths n))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (write-string (aref *cardinal-ones* hundreds) *format-output*)
           (write-string " hundred" *format-output*)
           (cond ((zerop rest)
                  (write-string "th" *format-output*))
                 (t
                  (write-char #\Space *format-output*)
                  (write-ordinal-tenths rest)))))))

;;; Print an ordinal number n such that 0 < n < 10^65.
(defun write-ordinal-non-zero (n)
  (multiple-value-bind (hundreds rest) (floor n 100)
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
(defun write-ordinal-numeral (n)
  (declare (type english-number n))
  (cond ((minusp n)
         (write-string "negative " *format-output*)
         (write-ordinal-non-zero (- n)))
        ((zerop n)
         (write-string "zeroth" *format-output*))
        (t
         (write-ordinal-non-zero n))))

(defmethod interpret-item (client (directive radix-directive) &optional parameters)
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (let ((radix (car parameters)))
      (cond (radix
             (apply #'write-radix-numeral client (pop-argument) colon-p at-sign-p parameters))
            ((and at-sign-p colon-p)
             (write-old-roman-numeral (pop-argument)))
            (at-sign-p
             (write-roman-numeral (pop-argument)))
            (colon-p
             (write-ordinal-numeral (pop-argument)))
            (t
             (write-cardinal-numeral (pop-argument)))))))

(defmethod compile-item (client (directive radix-directive) &optional parameters)
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (let ((radix (car parameters))
          (arg-form (pop-argument-form)))
      (cond ((numberp radix)
             `((write-radix-numeral ,(trinsic:client-form client) ,arg-form
                                    ,colon-p ,at-sign-p ,@parameters)))
            ((null radix)
             (cond ((and at-sign-p colon-p)
                    `((write-old-roman-numeral ,arg-form)))
                   (at-sign-p
                    `((write-roman-numeral ,arg-form)))
                   (colon-p
                    `((write-ordinal-numeral ,arg-form)))
                   (t
                    `((write-cardinal-numeral ,arg-form)))))
            (t
             `((if ,radix
                   (write-radix-numeral ,(trinsic:client-form client) ,arg-form
                                        ,colon-p ,at-sign-p ,@parameters)
                   ,(cond ((and at-sign-p colon-p)
                           `(write-old-roman-numeral ,arg-form))
                          (at-sign-p
                           `(write-roman-numeral ,arg-form))
                          (colon-p
                           `(write-ordinal-numeral ,arg-form))
                          (t
                           `(write-cardinal-numeral ,arg-form))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.2 ~d Decimal.

(defclass decimal-radix-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\D)) directive (end-directive t))
  (change-class directive 'decimal-radix-directive))

(defmethod interpret-item (client (directive decimal-radix-directive) &optional parameters)
  (apply #'write-radix-numeral client (pop-argument) (colon-p directive) (at-sign-p directive) 10 parameters))

(defmethod compile-item (client (directive decimal-radix-directive) &optional parameters)
  `((write-radix-numeral ,(trinsic:client-form client) ,(pop-argument-form) ,(colon-p directive) ,(at-sign-p directive) 10 ,@parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.3 ~b Binary.

(defclass binary-radix-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\B)) directive (end-directive t))
  (change-class directive 'binary-radix-directive))

(defmethod interpret-item (client (directive binary-radix-directive) &optional parameters)
  (apply #'write-radix-numeral client (pop-argument) (colon-p directive) (at-sign-p directive) 2 parameters))

(defmethod compile-item (client (directive binary-radix-directive) &optional parameters)
  `((write-radix-numeral ,(trinsic:client-form client) ,(pop-argument-form) ,(colon-p directive) ,(at-sign-p directive) 2 ,@parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.4 ~o Octal.

(defclass octal-radix-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\O)) directive (end-directive t))
  (change-class directive 'octal-radix-directive))

(defmethod interpret-item (client (directive octal-radix-directive) &optional parameters)
  (apply #'write-radix-numeral client (pop-argument) (colon-p directive) (at-sign-p directive) 8 parameters))

(defmethod compile-item (client (directive octal-radix-directive) &optional parameters)
  `((write-radix-numeral ,(trinsic:client-form client) ,(pop-argument-form) ,(colon-p directive) ,(at-sign-p directive) 8 ,@parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.5 ~x Hexadecimal.

(defclass hexadecimal-radix-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\X)) directive (end-directive t))
  (change-class directive 'hexadecimal-radix-directive))

(defmethod interpret-item
    (client (directive hexadecimal-radix-directive) &optional parameters)
  (apply #'write-radix-numeral client (pop-argument) (colon-p directive) (at-sign-p directive) 16 parameters))

(defmethod compile-item
    (client (directive hexadecimal-radix-directive) &optional parameters)
  `((write-radix-numeral ,(trinsic:client-form client) ,(pop-argument-form) ,(colon-p directive) ,(at-sign-p directive) 16 ,@parameters)))
