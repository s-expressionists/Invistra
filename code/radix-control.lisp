(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2 Radix control

(defun print-radix-arg (client radix colonp at-signp mincol padchar commachar comma-interval)
  (let ((argument (consume-next-argument t)))
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
               (comma-length (if colonp
                                 (max 0 (floor (1- (length string)) comma-interval))
                                 0))
               (sign-length (if (or at-signp (minusp argument)) 1 0))
               (total-length (+ (length string) comma-length sign-length))
               (pad-length (max 0 (- mincol total-length))))
          ;; Print the padding.
          (loop repeat pad-length
                do (write-char padchar *destination*))
          ;; Possibliy print a sign.
          (cond ((minusp argument)
                 (write-char #\- *destination*))
                (at-signp
                 (write-char #\+ *destination*))
                (t nil))
          ;; Print the string in reverse order
          (loop for index downfrom (1- (length string)) to 0
                for c across string
                do (write-char c *destination*)
                do (when (and colonp
                              (plusp index)
                              (zerop (mod index comma-interval)))
                     (write-char commachar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.1 ~r Radix.

(define-directive t #\r r-directive t (named-parameters-directive)
    ((radix :type (or null (integer 2 36)) :default nil)
     (mincol :type integer :default 0)
     (padchar :type character :default #\Space)
     (commachar :type character :default #\,)
     (comma-interval :type (integer 1) :default 3)))

;;; Print an integer as roman numerals to the stream.
;;; The integer must be strictly greater than zero,
;;; and strictly less than 4000.
(defun print-as-roman (integer stream)
  (declare (type (integer 1) integer))
  (multiple-value-bind (thousands rest) (floor integer 1000)
    (loop repeat thousands
          do (write-char #\M stream))
    (multiple-value-bind (hundreds rest) (floor rest 100)
      (write-string (case hundreds
                      (0 "") (1 "C") (2 "CC") (3 "CCC") (4 "CD")
                      (5 "D" ) (6 "DC") (7 "DCC") (8 "DCCC") (9 "CM"))
                    stream)
      (multiple-value-bind (tenths rest) (floor rest 10)
        (write-string (case tenths
                        (0 "") (1 "X") (2 "XX") (3 "XXX") (4 "XL")
                        (5 "L" ) (6 "LX") (7 "LXX") (8 "LXXX") (9 "XC"))
                      stream)
        (write-string (case rest
                        (0 "") (1 "I") (2 "II") (3 "III") (4 "IV")
                        (5 "V" ) (6 "VI") (7 "VII") (8 "VIII") (9 "IX"))
                      stream)))))

;;; Print an integer as old roman numerals to the stream.
;;; The integer must be strictly greater than zero,
;;; and strictly less than 4000.
(defun print-as-old-roman (integer stream)
  (declare (type (integer 1) integer))
  (multiple-value-bind (thousands rest) (floor integer 1000)
    (loop repeat thousands
          do (write-char #\M stream))
    (multiple-value-bind (hundreds rest) (floor rest 100)
      (write-string (case hundreds
                      (0 "") (1 "C") (2 "CC") (3 "CCC") (4 "CCCC")
                      (5 "D" ) (6 "DC") (7 "DCC") (8 "DCCC") (9 "DCCCC"))
                    stream)
      (multiple-value-bind (tenths rest) (floor rest 10)
        (write-string (case tenths
                        (0 "") (1 "X") (2 "XX") (3 "XXX") (4 "XXXX")
                        (5 "L" ) (6 "LX") (7 "LXX") (8 "LXXX") (9 "LXXXX"))
                      stream)
        (write-string (case rest
                        (0 "") (1 "I") (2 "II") (3 "III") (4 "IIII")
                        (5 "V" ) (6 "VI") (7 "VII") (8 "VIII") (9 "VIIII"))
                      stream)))))

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
(defun print-cardinal-tenths (n stream)
  (cond ((< n 10)
         (write-string (aref *cardinal-ones* n) stream))
        ((< n 20)
         (write-string (aref *cardinal-teens* (- n 10)) stream))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (write-string (aref *cardinal-tens* tens) stream)
           (unless (zerop ones)
             (write-char #\- stream)
             (write-string (aref *cardinal-ones* ones) stream))))))

;;; Print a cardinal number between 1 and 999.
(defun print-cardinal-hundreds (n stream)
  (cond ((< n 100)
         (print-cardinal-tenths n stream))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (write-string (aref *cardinal-ones* hundreds) stream)
           (write-string " hundred" stream)
           (unless (zerop rest)
             (write-char #\Space stream)
             (print-cardinal-tenths rest stream))))))

;;; Print a cardinal number n such that 0 < n < 10^65.
(defun print-cardinal-non-zero (n stream magnitude)
  (multiple-value-bind (thousands rest) (floor n 1000)
    (unless (zerop thousands)
      (print-cardinal-non-zero thousands stream (1+ magnitude)))
    (unless (or (zerop thousands) (zerop rest))
      (write-char #\Space stream))
    (unless (zerop rest)
      (print-cardinal-hundreds rest stream)
      (unless (zerop magnitude)
        (write-char #\Space stream)
        (write-string (aref *groups-of-three* magnitude) stream)))))

;;; Print a cardinal number n such that - 10^65 < n < 10^65.
(defun print-cardinal-number (n stream)
  (cond ((minusp n)
         (write-string "negative " stream)
         (print-cardinal-non-zero (- n) stream 0))
        ((zerop n)
         (write-string "zero" stream))
        (t
         (print-cardinal-non-zero n stream 0))))

(defparameter *ordinal-ones*
  #(nil "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"))

(defparameter *ordinal-teens*
  #("tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
    "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))

(defparameter *ordinal-tens*
  #(nil nil "twentieth" "thirtieth" "fortieth"
    "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

;;; Print an ordinal number between 1 and 99.
(defun print-ordinal-tenths (n stream)
  (cond ((< n 10)
         (write-string (aref *ordinal-ones* n) stream))
        ((< n 20)
         (write-string (aref *ordinal-teens* (- n 10)) stream))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (cond ((zerop ones)
                  (write-string (aref *ordinal-tens* tens) stream))
                 (t
                  (write-string (aref *cardinal-tens* tens) stream)
                  (write-char #\- stream)
                  (write-string (aref *ordinal-ones* ones) stream)))))))

;;; Print an ordinal number n such that 0 < n < 1000.
(defun print-ordinal-hundreds (n stream)
  (cond ((< n 100)
         (print-ordinal-tenths n stream))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (write-string (aref *cardinal-ones* hundreds) stream)
           (write-string " hundred" stream)
           (cond ((zerop rest)
                  (write-string "th" stream))
                 (t
                  (write-char #\Space stream)
                  (print-ordinal-tenths rest stream)))))))

;;; Print an ordinal number n such that 0 < n < 10^65.
(defun print-ordinal-non-zero (n stream)
  (multiple-value-bind (hundreds rest) (floor n 100)
    (cond ((zerop rest)
           ;; Hudreds is nonzero.
           (print-cardinal-non-zero n stream 0)
           (write-string "th" stream))
          ((zerop hundreds)
           (print-ordinal-hundreds rest stream))
          (t
           ;; They are both nonzero.
           (print-cardinal-non-zero (* 100 hundreds) stream 0)
           (write-char #\Space stream)
           (print-ordinal-tenths rest stream)))))

;;; Print an ordinal number n such that - 10^65 < n < 10^65.
(defun print-ordinal-number (n stream)
  (cond ((minusp n)
         (write-string "negative " stream)
         (print-ordinal-non-zero (- n) stream))
        ((zerop n)
         (write-string "zeroth" stream))
        (t
         (print-ordinal-non-zero n stream))))

(define-format-directive-interpreter r-directive
  (cond ((not (null radix))
         (print-radix-arg client radix colonp at-signp mincol padchar commachar comma-interval))
        ((and colonp at-signp)
         (print-as-old-roman (consume-next-argument '(integer 1))
                             *destination*))
        (at-signp
         (print-as-roman (consume-next-argument '(integer 1))
                         *destination*))
        (colonp
         (print-ordinal-number (consume-next-argument
                                `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                               *destination*))
        (t
         (print-cardinal-number (consume-next-argument
                                 `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                                *destination*))))

(define-format-directive-compiler r-directive
  (let ((print-number-radix `(print-radix-arg ,(incless:client-form client)
                                              radix ,colonp ,at-signp mincol
                                              padchar commachar comma-interval))
        (print-null-radix (cond ((and colonp at-signp)
                                 `(print-as-old-roman (consume-next-argument '(integer 1))
                                                      *destination*))
                                (at-signp
                                 `(print-as-roman (consume-next-argument '(integer 1))
                                                  *destination*))
                                (colonp
                                 `(print-ordinal-number (consume-next-argument
                                                         `(integer ,(1+ (- (expt 10 65)))
                                                                   ,(1- (expt 10 65))))
                                                        *destination*))
                                (t
                                 `(print-cardinal-number (consume-next-argument
                                                          `(integer ,(1+ (- (expt 10 65)))
                                                                    ,(1- (expt 10 65))))
                                                         *destination*)))))
  (cond ((numberp radix)
         (list print-number-radix))
        (t
         `((if (null radix)
               ,print-null-radix
               ,print-number-radix))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.2 ~d Decimal.

(define-directive t #\d d-directive t (named-parameters-directive)
    ((mincol :type integer :default 0)
     (padchar :type character :default #\Space)
     (commachar :type character :default #\,)
     (comma-interval :type (integer 1) :default 3)))

(define-format-directive-interpreter d-directive
  (print-radix-arg client 10 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler d-directive
  `((print-radix-arg ,(incless:client-form client) 10 ,colonp ,at-signp mincol padchar commachar comma-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.3 ~b Binary.

(define-directive t #\b b-directive t (named-parameters-directive)
    ((mincol :type integer :default 0)
     (padchar :type character :default #\Space)
     (commachar :type character :default #\,)
     (comma-interval :type (integer 1) :default 3)))

(define-format-directive-interpreter b-directive
  (print-radix-arg client 2 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler b-directive
  `((print-radix-arg ,(incless:client-form client) 2 ,colonp ,at-signp mincol padchar commachar comma-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.4 ~o Octal.

(define-directive t #\o o-directive t (named-parameters-directive)
    ((mincol :type integer :default 0)
     (padchar :type character :default #\Space)
     (commachar :type character :default #\,)
     (comma-interval :type (integer 1) :default 3)))

(define-format-directive-interpreter o-directive
  (print-radix-arg client 8 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler o-directive
  `((print-radix-arg ,(incless:client-form client) 8 ,colonp ,at-signp mincol padchar commachar comma-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.5 ~x Hexadecimal.

(define-directive t #\x x-directive t (named-parameters-directive)
    ((mincol :type integer :default 0)
     (padchar :type character :default #\Space)
     (commachar :type character :default #\,)
     (comma-interval :type (integer 1) :default 3)))

(define-format-directive-interpreter x-directive
  (print-radix-arg client 16 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler x-directive
  `((print-radix-arg ,(incless:client-form client) 16 ,colonp ,at-signp mincol padchar commachar comma-interval)))
