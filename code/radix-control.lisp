(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2 Radix control

(defclass base-radix-directive (directive)
  ())

(defmethod parameter-specifications (client (directive base-radix-directive))
  (declare (ignore client))
  '((:name radix :type integer :default 0)
    (:type character :default #\Space)
    (:type character :default #\,)
    (:type integer :default 3)))

(defun print-radix-arg (client colon-p at-sign-p radix mincol padchar commachar comma-interval)
  (let ((argument (pop-argument)))
    (if (not (integerp argument))
        (let ((*print-base* radix)
              (*print-escape* nil)
              (*print-readably* nil))
          (incless:write-object client argument *destination*))
        (let* ((string (let ((*print-base* radix)
                             (*print-radix* nil)
                             (*print-escape* nil)
                             (*print-readably* nil))
                         (with-output-to-string (stream)
                           (incless:write-object client (abs argument) stream))))
               (comma-length (if colon-p
                                 (max 0 (floor (1- (length string)) comma-interval))
                                 0))
               (sign-length (if (or at-sign-p (minusp argument)) 1 0))
               (total-length (+ (length string) comma-length sign-length))
               (pad-length (max 0 (- mincol total-length))))
          ;; Print the padding.
          (loop repeat pad-length
                do (write-char padchar *destination*))
          ;; Possibliy print a sign.
          (cond ((minusp argument)
                 (write-char #\- *destination*))
                (at-sign-p
                 (write-char #\+ *destination*))
                (t nil))
          ;; Print the string in reverse order
          (loop for index downfrom (1- (length string)) to 0
                for c across string
                do (write-char c *destination*)
                do (when (and colon-p
                              (plusp index)
                              (zerop (mod index comma-interval)))
                     (write-char commachar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.1 ~r Radix.

(defclass radix-directive (base-radix-directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\R)) directive (end-directive t))
  (change-class directive 'radix-directive))

(defmethod parameter-specifications ((client t) (directive radix-directive))
  (list* '(:type (or null (integer 2 36)) :default nil)
         (call-next-method)))

(defparameter *roman-digits*
  '("I" "V" "X" "L" "C" "D" "M"))

(defun print-roman-arg ()
  (labels ((write-digit (value digits)
             (multiple-value-bind (q r)
                 (floor value 10)
               (unless (zerop q)
                 (write-digit q (cddr digits)))
               (case r
                 (9
                  (write-string (car digits) *destination*)
                  (write-string (caddr digits) *destination*))
                 (4
                  (write-string (car digits) *destination*)
                  (write-string (cadr digits) *destination*))
                 (otherwise
                  (multiple-value-bind (q1 r1)
                      (floor r 5)
                    (unless (zerop q1)
                      (write-string (cadr digits) *destination*))
                    (loop repeat r1
                          do (write-string (car digits) *destination*))))))))
    (let ((digit-count (list-length *roman-digits*)))
      (write-digit (pop-argument
                    (if digit-count
                        (multiple-value-bind (q r)
                            (floor digit-count 2)
                          `(integer 1
                                    ,(1- (if (zerop r)
                                             (* (expt 10 (1- q)) 9)
                                             (* (expt 10 q) 4)))))
                        '(integer 1)))
                   *roman-digits*))))

(defun print-old-roman-arg ()
  (labels ((write-digit (value digits)
             (multiple-value-bind (q r)
                 (floor value 10)
               (unless (zerop q)
                 (write-digit q (cddr digits)))
               (multiple-value-bind (q1 r1)
                   (floor r 5)
                 (unless (zerop q1)
                   (write-string (cadr digits) *destination*))
                 (loop repeat r1
                       do (write-string (car digits) *destination*))))))
    (let ((digit-count (list-length *roman-digits*)))
      (write-digit (pop-argument
                    (if digit-count
                        (multiple-value-bind (q r)
                            (floor digit-count 2)
                          `(integer 1
                                    ,(1- (* (expt 10 q)
                                            (if (zerop r) 1 5)))))
                        '(integer 1)))
                   *roman-digits*))))

#+(or)(defun print-roman-arg (old-roman-p)
  (labels ((write-digit (value digits)
             (multiple-value-bind (q r)
                 (floor value 10)
               (unless (zerop q)
                 (write-digit q (cddr digits)))
               (cond ((and (not old-roman-p) (= r 9))
                      (write-string (car digits) *destination*)
                      (write-string (caddr digits) *destination*))
                     ((and (not old-roman-p) (= r 4))
                      (write-string (car digits) *destination*)
                      (write-string (cadr digits) *destination*))
                     (t
                      (multiple-value-bind (q1 r1)
                          (floor r 5)
                        (unless (zerop q1)
                          (write-string (cadr digits) *destination*))
                        (loop repeat r1
                              do (write-string (car digits) *destination*))))))))
    (let ((digit-count (list-length *roman-digits*)))
      (write-digit (pop-argument
                    (if digit-count
                        (multiple-value-bind (q r)
                            (floor digit-count 2)
                          `(integer 1
                                    ,(1- (cond ((and (zerop r) old-roman-p)
                                                (expt 10 q))
                                               ((zerop r)
                                                (* (expt 10 (1- q)) 9))
                                               (old-roman-p
                                                (* (expt 10 q) 5))
                                               (t
                                                (* (expt 10 q) 4))))))
                        '(integer 1)))
                   *roman-digits*))))

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
(defun print-cardinal-tenths (n)
  (cond ((< n 10)
         (write-string (aref *cardinal-ones* n) *destination*))
        ((< n 20)
         (write-string (aref *cardinal-teens* (- n 10)) *destination*))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (write-string (aref *cardinal-tens* tens) *destination*)
           (unless (zerop ones)
             (write-char #\- *destination*)
             (write-string (aref *cardinal-ones* ones) *destination*))))))

;;; Print a cardinal number between 1 and 999.
(defun print-cardinal-hundreds (n)
  (cond ((< n 100)
         (print-cardinal-tenths n))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (write-string (aref *cardinal-ones* hundreds) *destination*)
           (write-string " hundred" *destination*)
           (unless (zerop rest)
             (write-char #\Space *destination*)
             (print-cardinal-tenths rest))))))

;;; Print a cardinal number n such that 0 < n < 10^65.
(defun print-cardinal-non-zero (n magnitude)
  (multiple-value-bind (thousands rest) (floor n 1000)
    (unless (zerop thousands)
      (print-cardinal-non-zero thousands (1+ magnitude)))
    (unless (or (zerop thousands) (zerop rest))
      (write-char #\Space *destination*))
    (unless (zerop rest)
      (print-cardinal-hundreds rest)
      (unless (zerop magnitude)
        (write-char #\Space *destination*)
        (write-string (aref *groups-of-three* magnitude) *destination*)))))

;;; Print a cardinal number n such that - 10^65 < n < 10^65.
(defun print-cardinal-arg ()
  (let ((n (pop-argument `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))))
    (cond ((minusp n)
           (write-string "negative " *destination*)
           (print-cardinal-non-zero (- n) 0))
          ((zerop n)
           (write-string "zero" *destination*))
          (t
           (print-cardinal-non-zero n 0)))))

(defparameter *ordinal-ones*
  #(nil "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"))

(defparameter *ordinal-teens*
  #("tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
    "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))

(defparameter *ordinal-tens*
  #(nil nil "twentieth" "thirtieth" "fortieth"
    "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

;;; Print an ordinal number between 1 and 99.
(defun print-ordinal-tenths (n)
  (cond ((< n 10)
         (write-string (aref *ordinal-ones* n) *destination*))
        ((< n 20)
         (write-string (aref *ordinal-teens* (- n 10)) *destination*))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (cond ((zerop ones)
                  (write-string (aref *ordinal-tens* tens) *destination*))
                 (t
                  (write-string (aref *cardinal-tens* tens) *destination*)
                  (write-char #\- *destination*)
                  (write-string (aref *ordinal-ones* ones) *destination*)))))))

;;; Print an ordinal number n such that 0 < n < 1000.
(defun print-ordinal-hundreds (n)
  (cond ((< n 100)
         (print-ordinal-tenths n))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (write-string (aref *cardinal-ones* hundreds) *destination*)
           (write-string " hundred" *destination*)
           (cond ((zerop rest)
                  (write-string "th" *destination*))
                 (t
                  (write-char #\Space *destination*)
                  (print-ordinal-tenths rest)))))))

;;; Print an ordinal number n such that 0 < n < 10^65.
(defun print-ordinal-non-zero (n)
  (multiple-value-bind (hundreds rest) (floor n 100)
    (cond ((zerop rest)
           ;; Hudreds is nonzero.
           (print-cardinal-non-zero n 0)
           (write-string "th" *destination*))
          ((zerop hundreds)
           (print-ordinal-hundreds rest))
          (t
           ;; They are both nonzero.
           (print-cardinal-non-zero (* 100 hundreds) 0)
           (write-char #\Space *destination*)
           (print-ordinal-tenths rest)))))

;;; Print an ordinal number n such that - 10^65 < n < 10^65.
(defun print-ordinal-arg ()
  (let ((n (pop-argument `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))))
    (cond ((minusp n)
           (write-string "negative " *destination*)
           (print-ordinal-non-zero (- n)))
          ((zerop n)
           (write-string "zeroth" *destination*))
          (t
           (print-ordinal-non-zero n)))))

(defmethod interpret-item (client (directive radix-directive) &optional parameters)
  (let ((radix (car parameters))
        (colon-p (colon-p directive))
        (at-sign-p (at-sign-p directive)))
    (cond (radix
           (apply #'print-radix-arg client colon-p at-sign-p parameters))
          ((and at-sign-p colon-p)
           (print-old-roman-arg))
          (at-sign-p
           (print-roman-arg))
          (colon-p
           (print-ordinal-arg))
          (t
           (print-cardinal-arg)))))

(defmethod compile-item (client (directive radix-directive) &optional parameters)
  (let ((colon-p (colon-p directive))
        (at-sign-p (at-sign-p directive)))
    (cond ((numberp (car parameters))
           `((print-radix-arg ,(incless:client-form client)
                              ,colon-p ,at-sign-p ,@parameters)))
          ((null (car parameters))
           (cond ((and at-sign-p colon-p)
                  `((print-old-roman-arg)))
                 (at-sign-p
                  `((print-roman-arg)))
                 (colon-p
                  `((print-ordinal-arg)))
                 (t
                  `((print-cardinal-arg)))))
          (t
           `((if ,(car parameters)
                 (print-radix-arg ,(incless:client-form client)
                                  ,colon-p ,at-sign-p ,@parameters)
                 ,(cond ((and at-sign-p colon-p)
                         `(print-old-roman-arg))
                        (at-sign-p
                         `(print-roman-arg))
                        (colon-p
                         `(print-ordinal-arg))
                        (t
                         `(print-cardinal-arg)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.2 ~d Decimal.

(defclass decimal-radix-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client t) (char (eql #\D)) directive (end-directive t))
  (change-class directive 'decimal-radix-directive))

(defmethod interpret-item (client (directive decimal-radix-directive) &optional parameters)
  (apply #'print-radix-arg client (colon-p directive) (at-sign-p directive) 10 parameters))

(defmethod compile-item (client (directive decimal-radix-directive) &optional parameters)
  `((print-radix-arg ,(incless:client-form client) ,(colon-p directive) ,(at-sign-p directive) 10 ,@parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.3 ~b Binary.

(defclass binary-radix-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client t) (char (eql #\B)) directive (end-directive t))
  (change-class directive 'binary-radix-directive))

(defmethod interpret-item (client (directive binary-radix-directive) &optional parameters)
  (apply #'print-radix-arg client (colon-p directive) (at-sign-p directive) 2 parameters))

(defmethod compile-item (client (directive binary-radix-directive) &optional parameters)
  `((print-radix-arg ,(incless:client-form client) ,(colon-p directive) ,(at-sign-p directive) 2 ,@parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.4 ~o Octal.

(defclass octal-radix-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client t) (char (eql #\O)) directive (end-directive t))
  (change-class directive 'octal-radix-directive))

(defmethod interpret-item (client (directive octal-radix-directive) &optional parameters)
  (apply #'print-radix-arg client (colon-p directive) (at-sign-p directive) 8 parameters))

(defmethod compile-item (client (directive octal-radix-directive) &optional parameters)
  `((print-radix-arg ,(incless:client-form client) ,(colon-p directive) ,(at-sign-p directive) 8 ,@parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.5 ~x Hexadecimal.

(defclass hexadecimal-radix-directive (base-radix-directive)
  ())

(defmethod specialize-directive
    ((client t) (char (eql #\X)) directive (end-directive t))
  (change-class directive 'hexadecimal-radix-directive))

(defmethod interpret-item
    (client (directive hexadecimal-radix-directive) &optional parameters)
  (apply #'print-radix-arg client (colon-p directive) (at-sign-p directive) 16 parameters))

(defmethod compile-item
    (client (directive hexadecimal-radix-directive) &optional parameters)
  `((print-radix-arg ,(incless:client-form client) ,(colon-p directive) ,(at-sign-p directive) 16 ,@parameters)))
