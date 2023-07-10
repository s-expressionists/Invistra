(cl:in-package #:invistra-extrinsic/test)

(defun format-eval (&rest args)
  (apply #'invistra-extrinsic:format args))

(defmacro my-with-standard-io-syntax (&body body)
  `(let ((*print-array* t)
         (*print-base* 10)
         (*print-case* :upcase)
         (*print-circle* nil)
         (*print-escape* t)
         (*print-gensym* t)
         (*print-length* nil)
         (*print-level* nil)
         (*print-lines* nil)
         (*print-miser-width* nil)
         (*print-pretty* nil)
         (*print-radix* nil)
         (*print-readably* t)
         (*print-right-margin* nil))
     ,@body))

(defmacro define-equal-test (name expected form)
  `(define-test ,name
     (my-with-standard-io-syntax
       (macrolet ((fmt (destination control-string &rest args)
                    `(funcall (invistra-extrinsic:formatter ,control-string) ,destination ,@args)))
         (is equal
             ,expected
             ,form))
       (macrolet ((fmt (destination control-string &rest args)
                    `(invistra-extrinsic:format ,destination (progn ,control-string) ,@args)))
         (is equal
             ,expected
             ,form)))))

(defmacro define-fail-test (name form)
  `(define-test ,name
     :compile-at :execute
     (fail-compile (macrolet ((fmt (destination control-string &rest args)
                                `(invistra-extrinsic:format ,destination ,control-string ,@args)))
                     ,form))
     (fail (macrolet ((fmt (destination control-string &rest args)
                        `(invistra-extrinsic:format ,destination (progn ,control-string) ,@args)))
             ,form))))

;; multiple occurrences of a modifier
;; should signal an error.
(define-fail-test general.1
  (fmt nil "~::d" 0))

(define-fail-test general.2
  (fmt nil "~@@d" 0))

(define-fail-test general.3
  (fmt nil "~@:@d" 0))

(define-fail-test general.4
  (fmt nil "~:@:d" 0))

;;; When the second argument to format is a constant
;;; string, a compiler macro replaces the call
;;; to format with a compiled version of the call.
;;; To test the interpreter, we must therefore make
;;; sure that the second argument is not a constant
;;; string.  Do that by replacing the constant string
;;; "..." by (progn "...").
(defmacro expand-format (expression)
  `(progn ,expression
          ,(mapcar (lambda (subexpr)
                     (if (and (consp subexpr)
                              (eq (car subexpr) 'format))
                         `(fmt ,(cadr subexpr)
                                  (progn ,(caddr subexpr))
                                  ,@(cdddr subexpr))
                         subexpr))
                   expression)))

(define-equal-test nesting.1
  "ABCD"
  (fmt nil "~{~a~}~{~a~}" '(a b) '(c d)))

#+(or)(define-test character
  ;; without a : modifier, do what write-char does
  (loop for code from 0 to 1000
        for char = (code-char code)
        do (is equal
               (with-output-to-string (stream)
                 (write-char char stream))
               (fmt nil "~c" char)))
  ;; with the : modifier, if it is a printing
  ;; character, do what write-char does, and if
  ;; not do what char-name does
  #+(or)(loop for code from 0 to 1000
        for char = (code-char code)
        do (if (and (graphic-char-p char)
                    (not (eql char #\Space)))
               (expand-format (assert-equal
                               (with-output-to-string (stream)
                                 (write-char char stream))
                               (fmt nil "~:c" char)))
               (expand-format (assert-equal
                               (char-name char)
                               (fmt nil "~:c" char)))))
  ;; with the @ modifier, do what prin1 does
  #+(or)(loop for code from 0 to 1000
        for char = (code-char code)
        do (expand-format (assert-equal
                           (prin1-to-string char)
                           (fmt nil "~@c" char))))
  ;; using ~c with something other than a
  ;; character should signal an error
  #+(or)(loop for thing in '(1 "string" 'symbol *standard-output*)
        do (expand-format (assert-error 'error (fmt nil "~c" thing))))
  ;; the character directive does not take any parameters
  #+(or)(expand-format (assert-error 'error (fmt nil "~2c" #\a)))
  #+(or)(expand-format (assert-error 'error (fmt nil "~'2c" #\a)))
  #+(or)(expand-format (assert-error 'error (fmt nil "~#c" #\a)))
  #+(or)(expand-format (assert-error 'error (fmt nil "~vc" #\a))))

;; without any parameters, output a newline
(define-equal-test newline.01
  "
"
  (fmt nil "~%"))

;; also with a pameter of 1
(define-equal-test newline.02
  "
"
  (fmt nil "~1%"))

(define-equal-test newline.03
  ""
  (fmt nil "~0%"))

(define-equal-test newline.04
  "

"
  (fmt nil "~2%"))

;; The newline directive takes a single optional
;; numeric parameter
(define-fail-test newline.05
  (fmt nil "~'a%"))

(define-fail-test newline.06
  (fmt nil "~1,2%"))

;; The newline directive takes no modifiers
(define-fail-test newline.07
  (fmt nil "~:%"))

(define-fail-test newline.08
  (fmt nil "~@%"))

;; without any parameters, does nothing to a string
(define-equal-test fresh-line.01
  ""
  (fmt nil "~&"))

;; same thing for parameter values of 0 and 1
(define-equal-test fresh-line.02
  ""
  (fmt nil "~0&"))

(define-equal-test fresh-line.03
  ""
  (fmt nil "~1&"))

;; for a parameter value of 2, outputs a newline
(define-equal-test fresh-line.04
  "
"
  (fmt nil "~2&"))

;; The fresh-line directive takes a single optional
;; numeric parameter
(define-fail-test fresh-line.05
  (fmt nil "~'a&"))

(define-fail-test fresh-line.06
  (fmt nil "~1,2&"))

;; The fresh-line directive takes no modifiers
(define-fail-test fresh-line.07
  (fmt nil "~:&"))

(define-fail-test fresh-line.08
  (fmt nil "~@&"))

;; without any parameters, outputs a page separator
(define-equal-test page.01
  ""
  (fmt nil "~|"))

;; same thing for a parameter value of 1
(define-equal-test page.02
  ""
  (fmt nil "~1|"))

;; with a parameter value of 0, does nothing
(define-equal-test page.03
  ""
  (fmt nil "~0|"))

;; for a parameter value of 2, outputs two page separators
(define-equal-test page.04
  ""
  (fmt nil "~2|"))

;; The page directive takes a single optional
;; numeric parameter
(define-fail-test page.05
  (fmt nil "~'a|"))

(define-fail-test page.06
  (fmt nil "~1,2|"))

;; The page directive takes no modifiers
(define-fail-test page.07
  (fmt nil "~:|"))

(define-fail-test page.08
  (fmt nil "~@|"))

;; without any parameters, outputs a tilde
(define-equal-test tilde.01
  "~"
  (fmt nil "~~"))

;; same thing for a parameter value of 1
(define-equal-test tilde.02
  "~"
  (fmt nil "~1~"))

;; with a parameter value of 0, does nothing
(define-equal-test tilde.03
  ""
  (fmt nil "~0~"))

;; for a parameter value of 2, outputs two tildes
(define-equal-test tilde.04
  "~~"
  (fmt nil "~2~"))

;; The tilde directive takes a single optional
;; numeric parameter

(define-fail-test tilde.05
    (fmt nil "~'a~"))

(define-fail-test tilde.06
    (fmt nil "~1,2~"))

;; The tilde directive takes no modifiers
(define-fail-test tilde.07
    (fmt nil "~:~"))

(define-fail-test tilde.08
    (fmt nil "~@~"))

;; English cardinal numbers
(define-equal-test radix.cardinal.01
  "zero"
  (fmt nil "~r" 0))

(define-equal-test radix.cardinal.02
  "one"
  (fmt nil "~r" 1))

(define-equal-test radix.cardinal.03
  "two"
  (fmt nil "~r" 2))

(define-equal-test radix.cardinal.04
  "three"
  (fmt nil "~r" 3))

(define-equal-test radix.cardinal.05
  "four"
  (fmt nil "~r" 4))

(define-equal-test radix.cardinal.06
  "five"
  (fmt nil "~r" 5))

(define-equal-test radix.cardinal.07
  "six"
  (fmt nil "~r" 6))

(define-equal-test radix.cardinal.08
  "seven"
  (fmt nil "~r" 7))

(define-equal-test radix.cardinal.09
  "eight"
  (fmt nil "~r" 8))

(define-equal-test radix.cardinal.10
  "nine"
  (fmt nil "~r" 9))

(define-equal-test radix.cardinal.11
  "ten"
  (fmt nil "~r" 10))

(define-equal-test radix.cardinal.12
  "eleven"
  (fmt nil "~r" 11))

(define-equal-test radix.cardinal.13
  "twelve"
  (fmt nil "~r" 12))

(define-equal-test radix.cardinal.14
  "thirteen"
  (fmt nil "~r" 13))

(define-equal-test radix.cardinal.15
  "fourteen"
  (fmt nil "~r" 14))

(define-equal-test radix.cardinal.16
  "fifteen"
  (fmt nil "~r" 15))

(define-equal-test radix.cardinal.17
  "sixteen"
  (fmt nil "~r" 16))

(define-equal-test radix.cardinal.18
  "seventeen"
  (fmt nil "~r" 17))

(define-equal-test radix.cardinal.19
  "eighteen"
  (fmt nil "~r" 18))

(define-equal-test radix.cardinal.20
  "nineteen"
  (fmt nil "~r" 19))

(define-equal-test radix.cardinal.21
  "twenty"
  (fmt nil "~r" 20))

(define-equal-test radix.cardinal.22
  "twenty-one"
  (fmt nil "~r" 21))

(define-equal-test radix.cardinal.23
  "thirty"
  (fmt nil "~r" 30))

(define-equal-test radix.cardinal.24
  "fourty"
  (fmt nil "~r" 40))

(define-equal-test radix.cardinal.25
  "fifty"
  (fmt nil "~r" 50))

(define-equal-test radix.cardinal.26
  "sixty"
  (fmt nil "~r" 60))

(define-equal-test radix.cardinal.27
  "seventy"
  (fmt nil "~r" 70))

(define-equal-test radix.cardinal.28
  "eighty"
  (fmt nil "~r" 80))

(define-equal-test radix.cardinal.29
  "ninety"
  (fmt nil "~r" 90))

(define-equal-test radix.cardinal.30
  "one hundred"
  (fmt nil "~r" 100))

(define-equal-test radix.cardinal.31
  "two hundred four"
  (fmt nil "~r" 204))

(define-equal-test radix.cardinal.32
  "three hundred sixteen"
  (fmt nil "~r" 316))

(define-equal-test radix.cardinal.33
  "four hundred thirty-six"
  (fmt nil "~r" 436))

(define-equal-test radix.cardinal.34
  "two thousand"
  (fmt nil "~r" 2000))

(define-equal-test radix.cardinal.35
  "three thousand five"
  (fmt nil "~r" 3005))

(define-equal-test radix.cardinal.36
  "four thousand twelve"
  (fmt nil "~r" 4012))

(define-equal-test radix.cardinal.37
  "five thousand two hundred"
  (fmt nil "~r" 5200))

(define-equal-test radix.cardinal.38
  "eighty thousand"
  (fmt nil "~r" 80000))

(define-equal-test radix.cardinal.39
  "five hundred thousand"
  (fmt nil "~r" 500000))

(define-equal-test radix.cardinal.40
  "two million"
  (fmt nil "~r" 2000000))

(define-equal-test radix.cardinal.41
  "three million six"
  (fmt nil "~r" 3000006))

(define-equal-test radix.cardinal.42
  "four million two thousand"
  (fmt nil "~r" 4002000))

;; English ordinal numbers
(define-equal-test radix.ordinal.01
  "zeroth"
  (fmt nil "~:r" 0))

(define-equal-test radix.ordinal.02
  "first"
  (fmt nil "~:r" 1))

(define-equal-test radix.ordinal.03
  "second"
  (fmt nil "~:r" 2))

(define-equal-test radix.ordinal.04
  "third"
  (fmt nil "~:r" 3))

(define-equal-test radix.ordinal.05
  "fourth"
  (fmt nil "~:r" 4))

(define-equal-test radix.ordinal.06
  "fifth"
  (fmt nil "~:r" 5))

(define-equal-test radix.ordinal.07
  "sixth"
  (fmt nil "~:r" 6))

(define-equal-test radix.ordinal.08
  "seventh"
  (fmt nil "~:r" 7))

(define-equal-test radix.ordinal.09
  "eighth"
  (fmt nil "~:r" 8))

(define-equal-test radix.ordinal.10
  "ninth"
  (fmt nil "~:r" 9))

(define-equal-test radix.ordinal.11
  "tenth"
  (fmt nil "~:r" 10))

(define-equal-test radix.ordinal.12
  "eleventh"
  (fmt nil "~:r" 11))

(define-equal-test radix.ordinal.13
  "twelvth"
  (fmt nil "~:r" 12))

(define-equal-test radix.ordinal.14
  "thirteenth"
  (fmt nil "~:r" 13))

(define-equal-test radix.ordinal.15
  "fourteenth"
  (fmt nil "~:r" 14))

(define-equal-test radix.ordinal.16
  "fifteenth"
  (fmt nil "~:r" 15))

(define-equal-test radix.ordinal.17
  "sixteenth"
  (fmt nil "~:r" 16))

(define-equal-test radix.ordinal.18
  "seventeenth"
  (fmt nil "~:r" 17))

(define-equal-test radix.ordinal.19
  "eighteenth"
  (fmt nil "~:r" 18))

(define-equal-test radix.ordinal.20
  "nineteenth"
  (fmt nil "~:r" 19))

(define-equal-test radix.ordinal.21
  "twentieth"
  (fmt nil "~:r" 20))

(define-equal-test radix.ordinal.22
  "twenty-first"
  (fmt nil "~:r" 21))

(define-equal-test radix.ordinal.23
  "thirtieth"
  (fmt nil "~:r" 30))

(define-equal-test radix.ordinal.24
  "fourtieth"
  (fmt nil "~:r" 40))

(define-equal-test radix.ordinal.25
  "fiftieth"
  (fmt nil "~:r" 50))

(define-equal-test radix.ordinal.26
  "sixtieth"
  (fmt nil "~:r" 60))

(define-equal-test radix.ordinal.27
  "seventieth"
  (fmt nil "~:r" 70))

(define-equal-test radix.ordinal.28
  "eightieth"
  (fmt nil "~:r" 80))

(define-equal-test radix.ordinal.29
  "ninetieth"
  (fmt nil "~:r" 90))

(define-equal-test radix.ordinal.30
  "one hundredth"
  (fmt nil "~:r" 100))

(define-equal-test radix.ordinal.31
  "two hundred fourth"
  (fmt nil "~:r" 204))

(define-equal-test radix.ordinal.32
  "three hundred sixteenth"
  (fmt nil "~:r" 316))

(define-equal-test radix.ordinal.33
  "four hundred thirty-sixth"
  (fmt nil "~:r" 436))

(define-equal-test radix.ordinal.34
  "two thousandth"
  (fmt nil "~:r" 2000))

(define-equal-test radix.ordinal.35
  "three thousand fifth"
  (fmt nil "~:r" 3005))

(define-equal-test radix.ordinal.36
  "four thousand twelvth"
  (fmt nil "~:r" 4012))

(define-equal-test radix.ordinal.37
  "five thousand two hundredth"
  (fmt nil "~:r" 5200))

(define-equal-test radix.ordinal.38
  "eighty thousandth"
  (fmt nil "~:r" 80000))

(define-equal-test radix.ordinal.39
  "five hundred thousandth"
  (fmt nil "~:r" 500000))

(define-equal-test radix.ordinal.40
  "two millionth"
  (fmt nil "~:r" 2000000))

(define-equal-test radix.ordinal.41
  "three million sixth"
  (fmt nil "~:r" 3000006))

(define-equal-test radix.ordinal.42
  "four million two thousandth"
  (fmt nil "~:r" 4002000))

;; Roman numerals
(define-equal-test radix.roman.01
  "I"
  (fmt nil "~@r" 1))

(define-equal-test radix.roman.02
  "II"
  (fmt nil "~@r" 2))

(define-equal-test radix.roman.03
  "III"
  (fmt nil "~@r" 3))

(define-equal-test radix.roman.04
  "IV"
  (fmt nil "~@r" 4))

(define-equal-test radix.roman.05
  "V"
  (fmt nil "~@r" 5))

(define-equal-test radix.roman.06
  "VI"
  (fmt nil "~@r" 6))

(define-equal-test radix.roman.07
  "VII"
  (fmt nil "~@r" 7))

(define-equal-test radix.roman.08
  "VIII"
  (fmt nil "~@r" 8))

(define-equal-test radix.roman.09
  "IX"
  (fmt nil "~@r" 9))

(define-equal-test radix.roman.10
  "X"
  (fmt nil "~@r" 10))

(define-equal-test radix.roman.11
  "XI"
  (fmt nil "~@r" 11))

(define-equal-test radix.roman.12
  "XII"
  (fmt nil "~@r" 12))

(define-equal-test radix.roman.13
  "XIII"
  (fmt nil "~@r" 13))

(define-equal-test radix.roman.14
  "XIV"
  (fmt nil "~@r" 14))

(define-equal-test radix.roman.15
  "XV"
  (fmt nil "~@r" 15))

(define-equal-test radix.roman.16
  "XVI"
  (fmt nil "~@r" 16))

(define-equal-test radix.roman.17
  "XVII"
  (fmt nil "~@r" 17))

(define-equal-test radix.roman.18
  "XVIII"
  (fmt nil "~@r" 18))

(define-equal-test radix.roman.19
  "XIX"
  (fmt nil "~@r" 19))

(define-equal-test radix.roman.20
  "XX"
  (fmt nil "~@r" 20))

(define-equal-test radix.roman.21
  "XXX"
  (fmt nil "~@r" 30))

(define-equal-test radix.roman.22
  "XL"
  (fmt nil "~@r" 40))

(define-equal-test radix.roman.23
  "L"
  (fmt nil "~@r" 50))

(define-equal-test radix.roman.24
  "LXIV"
  (fmt nil "~@r" 64))

(define-equal-test radix.roman.25
  "XCIX"
  (fmt nil "~@r" 99))

(define-equal-test radix.roman.26
  "C"
  (fmt nil "~@r" 100))

(define-equal-test radix.roman.27
  "CXLVII"
  (fmt nil "~@r" 147))

(define-equal-test radix.roman.28
  "CDLXXXIX"
  (fmt nil "~@r" 489))

(define-equal-test radix.roman.29
  "DCCCXXXI"
  (fmt nil "~@r" 831))

(define-equal-test radix.roman.30
  "M"
  (fmt nil "~@r" 1000))

(define-equal-test radix.roman.31
  "MMXL"
  (fmt nil "~@r" 2040))

(define-equal-test radix.roman.32
  "MMMXC"
  (fmt nil "~@r" 3090))

;; Old Roman numerals

(define-equal-test radix.old-roman.01
  "I"
  (fmt nil "~:@r" 1))

(define-equal-test radix.old-roman.02
  "II"
  (fmt nil "~:@r" 2))

(define-equal-test radix.old-roman.03
  "III"
  (fmt nil "~:@r" 3))

(define-equal-test radix.old-roman.04
  "IIII"
  (fmt nil "~:@r" 4))

(define-equal-test radix.old-roman.05
  "V"
  (fmt nil "~:@r" 5))

(define-equal-test radix.old-roman.06
  "VI"
  (fmt nil "~:@r" 6))

(define-equal-test radix.old-roman.07
  "VII"
  (fmt nil "~:@r" 7))

(define-equal-test radix.old-roman.08
  "VIII"
  (fmt nil "~:@r" 8))

(define-equal-test radix.old-roman.09
  "VIIII"
  (fmt nil "~:@r" 9))

(define-equal-test radix.old-roman.10
  "X"
  (fmt nil "~:@r" 10))

(define-equal-test radix.old-roman.11
  "XI"
  (fmt nil "~:@r" 11))

(define-equal-test radix.old-roman.12
  "XII"
  (fmt nil "~:@r" 12))

(define-equal-test radix.old-roman.13
  "XIII"
  (fmt nil "~:@r" 13))

(define-equal-test radix.old-roman.14
  "XIIII"
  (fmt nil "~:@r" 14))

(define-equal-test radix.old-roman.15
  "XV"
  (fmt nil "~:@r" 15))

(define-equal-test radix.old-roman.16
  "XVI"
  (fmt nil "~:@r" 16))

(define-equal-test radix.old-roman.17
  "XVII"
  (fmt nil "~:@r" 17))

(define-equal-test radix.old-roman.18
  "XVIII"
  (fmt nil "~:@r" 18))

(define-equal-test radix.old-roman.19
  "XVIIII"
  (fmt nil "~:@r" 19))

(define-equal-test radix.old-roman.20
  "XX"
  (fmt nil "~:@r" 20))

(define-equal-test radix.old-roman.21
  "XXX"
  (fmt nil "~:@r" 30))

(define-equal-test radix.old-roman.22
  "XXXX"
  (fmt nil "~:@r" 40))

(define-equal-test radix.old-roman.23
  "L"
  (fmt nil "~:@r" 50))

(define-equal-test radix.old-roman.24
  "LXIIII"
  (fmt nil "~:@r" 64))

(define-equal-test radix.old-roman.25
  "LXXXXVIIII"
  (fmt nil "~:@r" 99))

(define-equal-test radix.old-roman.26
  "C"
  (fmt nil "~:@r" 100))

(define-equal-test radix.old-roman.27
  "CXXXXVII"
  (fmt nil "~:@r" 147))

(define-equal-test radix.old-roman.28
  "CCCCLXXXVIIII"
  (fmt nil "~:@r" 489))

(define-equal-test radix.old-roman.29
  "DCCCXXXI"
  (fmt nil "~:@r" 831))

(define-equal-test radix.old-roman.30
  "M"
  (fmt nil "~:@r" 1000))

(define-equal-test radix.old-roman.31
  "MMXXXX"
  (fmt nil "~:@r" 2040))

(define-equal-test radix.old-roman.32
  "MMMLXXXX"
  (fmt nil "~:@r" 3090))

;; test the use of different values of the radix
(define-equal-test radix.arbitrary.01
    nil
    (loop for radix from 2 to 36
          do (loop repeat 1000
                   for value = (random (expt 10 100))
                   for expected = (write-to-string value)
                   for actual = (fmt nil "~vr" radix value)
                   unless (equal expected actual)
                     collect `(:radix ,radix :expected ,expected :actual ,actual))))

;; test that the mincol parameter is taken into account
(define-equal-test radix.parameter.01
  "123"
  (fmt nil "~10,1r" 123))

(define-equal-test radix.parameter.02
  "123"
  (fmt nil "~10,2r" 123))

(define-equal-test radix.parameter.03
  "123"
  (fmt nil "~10,3r" 123))

(define-equal-test radix.parameter.04
  " 123"
  (fmt nil "~10,4r" 123))

(define-equal-test radix.parameter.05
  "  123"
  (fmt nil "~10,5r" 123))

;; test that the padchar parameter is taken into account
(define-equal-test radix.parameter.06
  "xx123"
  (fmt nil "~10,5,'xr" 123))

;; test the : modifier
(define-equal-test radix.parameter.07
  "xx123"
  (fmt nil "~10,5,'x:r" 123))

(define-equal-test radix.parameter.08
  "xx1,234"
  (fmt nil "~10,7,'x:r" 1234))

(define-equal-test radix.parameter.09
  "xx551,234"
  (fmt nil "~10,9,'x:r" 551234))

(define-equal-test radix.parameter.10
  "xx66,551,234"
  (fmt nil "~10,12,'x:r" 66551234))

;; test the commachar parameter is taken into account
(define-equal-test radix.parameter.11
  "xx66a551a234"
  (fmt nil "~10,12,'x,'a:r" 66551234))

;; test that the mincol parameter is taken into account
(define-equal-test decimal.01
  "123"
  (fmt nil "~1d" 123))

(define-equal-test decimal.02
  "123"
  (fmt nil "~2d" 123))

(define-equal-test decimal.03
  "123"
  (fmt nil "~3d" 123))

(define-equal-test decimal.04
  " 123"
  (fmt nil "~4d" 123))

(define-equal-test decimal.05
  "  123"
  (fmt nil "~5d" 123))

;; test that the padchar parameter is taken into account
(define-equal-test decimal.06
  "xx123"
  (fmt nil "~5,'xd" 123))

;; test the : modifier
(define-equal-test decimal.07
  "xx123"
  (fmt nil "~5,'x:d" 123))

(define-equal-test decimal.08
  "xx1,234"
  (fmt nil "~7,'x:d" 1234))

(define-equal-test decimal.09
  "xx551,234"
  (fmt nil "~9,'x:d" 551234))

(define-equal-test decimal.10
  "xx66,551,234"
  (fmt nil "~12,'x:d" 66551234))

;; test the commachar parameter is taken into account
(define-equal-test decimal.11
  "xx66a551a234"
  (fmt nil "~12,'x,'a:d" 66551234))

;; test that the mincol parameter is taken into account
(define-equal-test octal.01
  "123"
  (fmt nil "~1o" #o123))

(define-equal-test octal.02
  "123"
  (fmt nil "~2o" #o123))

(define-equal-test octal.03
  "123"
  (fmt nil "~3o" #o123))

(define-equal-test octal.04
  " 123"
  (fmt nil "~4o" #o123))

(define-equal-test octal.05
  "  123"
  (fmt nil "~5o" #o123))

;; test that the padchar parameter is taken into account
(define-equal-test octal.06
  "xx123"
  (fmt nil "~5,'xo" #o123))

;; test the : modifier
(define-equal-test octal.07
  "xx123"
  (fmt nil "~5,'x:o" #o123))

(define-equal-test octal.08
  "xx1,234"
  (fmt nil "~7,'x:o" #o1234))

(define-equal-test octal.09
  "xx551,234"
  (fmt nil "~9,'x:o" #o551234))

(define-equal-test octal.10
  "xx66,551,234"
  (fmt nil "~12,'x:o" #o66551234))

;; test the commachar parameter is taken into account
(define-equal-test octal.11
  "xx66a551a234"
  (fmt nil "~12,'x,'a:o" #o66551234))

;; test that the mincol parameter is taken into account
(define-equal-test binary.01
  "101"
  (fmt nil "~1b" #b101))

(define-equal-test binary.02
  "101"
  (fmt nil "~2b" #b101))

(define-equal-test binary.03
  "101"
  (fmt nil "~3b" #b101))

(define-equal-test binary.04
  " 101"
  (fmt nil "~4b" #b101))

(define-equal-test binary.05
  "  101"
  (fmt nil "~5b" #b101))

;; test that the padchar parameter is taken into account
(define-equal-test binary.06
  "xx101"
  (fmt nil "~5,'xb" #b101))

;; test the : modifier
(define-equal-test binary.07
  "xx101"
  (fmt nil "~5,'x:b" #b101))

(define-equal-test binary.08
  "xx1,011"
  (fmt nil "~7,'x:b" #b1011))

(define-equal-test binary.09
  "xx111,011"
  (fmt nil "~9,'x:b" #b111011))

(define-equal-test binary.10
  "xx10,111,011"
  (fmt nil "~12,'x:b" #b10111011))

;; test the commachar parameter is taken into account
(define-equal-test binary.11
  "xx10a111a011"
  (fmt nil "~12,'x,'a:b" #b10111011))

#+(or)(define-test aesthetic
  ;; Test that objects are printed as with princ
  (loop for obj in '(234 -10 1.5 'abc "hello" #\x #\Space nil)
        do (expand-format
            (assert-equal (princ-to-string obj)
                          (fmt nil "~a" obj))))
  ;; Test that with a `:' modifier, NIL will print as ()
  (expand-format
   (assert-equal "()"
                 (fmt nil "~:a" nil)))
  ;; Test that the mincol argument is taken into account
  (expand-format
   (assert-equal "hello  "
                 (fmt nil "~7a" "hello")))
  ;; Test that the `@' modifier is taken into account
  (expand-format
   (assert-equal "  hello"
                 (fmt nil "~7@a" "hello")))
  ;; Test that the colinc parameter is taken into account
  (expand-format
   (assert-equal "hello                     "
                 (fmt nil "~21,7a" "hello")))
  ;; Test that the minpad parameter is taken into account
  (expand-format
   (assert-equal "hello   "
                 (fmt nil "~,,3a" "hello")))
  ;; Test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "helloxxx"
                 (fmt nil "~0,1,3,'xa" "hello"))))

#+(or)(define-test standard
  ;; Test that objects are printed as with princ
  (loop for obj in '(234 -10 1.5 'abc "hello" #\x #\Space nil)
        do (expand-format
            (assert-equal (prin1-to-string obj)
                          (fmt nil "~s" obj))))
  ;; Test that with a `:' modifier, NIL will print as ()
  (expand-format
   (assert-equal "()"
                 (fmt nil "~:s" nil)))
  ;; Test that the mincol argument is taken into account
  (expand-format
   (assert-equal "12345  "
                 (fmt nil "~7s" 12345)))
  ;; Test that the `@' modifier is taken into account
  (expand-format
   (assert-equal "  12345"
                 (fmt nil "~7@s" 12345)))
  ;; Test that the colinc parameter is taken into account
  (expand-format
   (assert-equal "12345                     "
                 (fmt nil "~21,7s" 12345)))
  ;; Test that the minpad parameter is taken into account
  (expand-format
   (assert-equal "12345   "
                 (fmt nil "~,,3s" 12345)))
  ;; Test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "12345xxx"
                 (fmt nil "~0,1,3,'xs" 12345))))

(define-equal-test write.01
  "234"
  (fmt nil "~w" 234))

(define-equal-test write.02
  "-10"
  (fmt nil "~w" -10))

(define-equal-test write.03
  "1.5"
  (fmt nil "~w" 1.5))

(define-equal-test write.04
  "ABC"
  (fmt nil "~w" 'abc))

(define-equal-test write.05
  "\"hello\""
  (fmt nil "~w" "hello"))

(define-equal-test write.06
  "#\\x"
  (fmt nil "~w" #\x))

(define-equal-test write.07
  " "
  (fmt nil "~w" #\Space))

(define-equal-test write.08
  "NIL"
  (fmt nil "~w" nil))

;; test that it can handle circular lists
(define-equal-test write.09
  "#1=(1 . #1#)"
  (let ((*print-circle* t))
    (fmt nil "~w"
         (let ((l (list 1)))
           (setf (cdr l) l)))))

;; test that this directive reports an error
;; if a parameter is given
(define-fail-test write.10
  (fmt nil "~1w" 234))

(define-fail-test write.11
  (assert-error 'error (fmt nil "~'aw" 234)))

(define-equal-test go-to.01
  "ac"
  (fmt nil "~c~*~c" #\a #\b #\c #\d))

(define-equal-test go-to.02
  "ab"
  (fmt nil "~c~0*~c" #\a #\b #\c #\d))

(define-equal-test go-to.03
  "ac"
  (fmt nil "~c~1*~c" #\a #\b #\c #\d))

(define-equal-test go-to.04
  "ad"
  (fmt nil "~c~2*~c" #\a #\b #\c #\d))

(define-equal-test go-to.05
  "aa"
  (fmt nil "~c~:*~c" #\a #\b #\c #\d))

(define-equal-test go-to.06
  "ab"
  (fmt nil "~c~0:*~c" #\a #\b #\c #\d))

(define-equal-test go-to.07
  "aa"
  (fmt nil "~c~1:*~c" #\a #\b #\c #\d))

(define-equal-test go-to.08
  "aba"
  (fmt nil "~c~c~2:*~c" #\a #\b #\c #\d))

(define-equal-test go-to.09
  "aba"
  (fmt nil "~c~c~@*~c" #\a #\b #\c #\d))

(define-equal-test go-to.10
  "abb"
  (fmt nil "~c~c~1@*~c" #\a #\b #\c #\d))

(define-equal-test go-to.11
  "abd"
  (fmt nil "~c~c~3@*~c" #\a #\b #\c #\d))

;; Test that going beyond the first or last argument
;; gives an error.
(define-fail-test go-to.12
  (fmt nil "~c~c~*" #\a #\b))

(define-fail-test go-to.13
  (fmt nil "~c~c~2*~:2*~c" #\a #\b #\c))

(define-fail-test go-to.14
  (fmt nil "~c~:2*~2*~c" #\a #\b #\c))

(define-fail-test go-to.15
  (fmt nil "~c~-1@*~0@*~c" #\a #\b #\c))

(define-fail-test go-to.16
  (fmt nil "~c~4@*~0@*~c" #\a #\b #\c))

(define-equal-test conditional.01
  "abc"
  (fmt nil "~[xyz~;abc~;def~]" 1))

(define-equal-test conditional.02
  "xyz"
  (fmt nil "~[xyz~;abc~;def~]" 0))

(define-equal-test conditional.03
  ""
  (fmt nil "~[xyz~;abc~;def~]" 3))

;; test the default clause
(define-equal-test conditional.04
  "abc"
  (fmt nil "~[xyz~;abc~:;def~]" 1))

(define-equal-test conditional.05
  "xyz"
  (fmt nil "~[xyz~;abc~:;def~]" 0))

(define-equal-test conditional.06
  "def"
  (fmt nil "~[xyz~;abc~:;def~]" 3))

(define-equal-test conditional.07
  "abc"
  (fmt nil "~:[xyz~;abc~]" nil))

(define-equal-test conditional.08
  "xyz"
  (fmt nil "~:[xyz~;abc~]" 24))

(define-equal-test conditional.09
  "xyz23"
  (fmt nil "~@[xyz~]~d" 23))

(define-equal-test conditional.10
  "23"
  (fmt nil "~@[xyz~]~d" nil 23))

;; test the use of the parameter instead of the argument
(define-equal-test conditional.11
  "abc"
  (fmt nil "~#[xyz~;abc~;def~]" 10))

(define-equal-test conditional.12
  "xyz"
  (fmt nil "~#[xyz~;abc~;def~]"))

(define-equal-test conditional.13
  ""
  (fmt nil "~#[xyz~;abc~;def~]" 10 10 10))

(define-equal-test conditional.14
  "abc"
  (fmt nil "~v[xyz~;abc~;def~]" 1))

(define-equal-test conditional.15
  "xyz"
  (fmt nil "~v[xyz~;abc~;def~]" 0))

(define-equal-test conditional.16
  ""
  (fmt nil "~v[xyz~;abc~;def~]" 3))

(define-equal-test conditional.17
  "abc"
  (fmt nil "~1[xyz~;abc~;def~]" 10))

(define-equal-test conditional.18
  "xyz"
  (fmt nil "~0[xyz~;abc~;def~]" 10))

(define-equal-test conditional.19
  ""
  (fmt nil "~3[xyz~;abc~;def~]" 10))

;; test that giving the : modifier fails if there
;; are not exactly two clauses
(define-fail-test conditional.20
  (fmt nil "~:[xyz~;abc~;def~]" nil))

(define-fail-test conditional.21
  (fmt nil "~:[xyz~]" nil))

;; test that giving the @ modifier fails if there
;; is not exactly one clause
(define-fail-test conditional.22
  (fmt nil "~@[xyz~;abc~]~d" nil 23))

;; test that giving no clauses fails
(define-fail-test conditional.23
  (fmt nil "~[~]" nil 23))

;; test that giving both modifiers gives an error.
(define-fail-test conditional.24
  (fmt nil "~:@[xyz~;abc~;def~]" 1 2 3))

;; test that giving the : modifier to a clause separator
;; other than the last gives an error
(define-fail-test conditional.25
  (fmt nil "~[xyz~:;abc~:;def~]" 3))

;; test that giving the modifiers to ~] gives an error
;; test that giving parameters to ~; or ~] gives an error
(define-fail-test conditional.26
  (fmt nil "~[xyz~;abc~;def~2]" 3))

(define-fail-test conditional.27
  (fmt nil "~[xyz~;abc~2;def~]" 3))

(define-fail-test conditional.28
  (fmt nil "~[xyz~;abc~;def~#]" 3))

(define-fail-test conditional.29
  (fmt nil "~[xyz~;abc~#;def~]" 3))

(define-fail-test conditional.30
  (fmt nil "~[xyz~;abc~;def~v]" 3))

(define-fail-test conditional.31
  (fmt nil "~[xyz~;abc~v;def~]" 3))

(define-equal-test iteration.01
  "ABCDE"
  (fmt nil "~{~a~a~}~a" '(a b c d) 'e))

;; test that, with a parameter, at most that many
;; iterations are done.
(define-equal-test iteration.02
  "ABE"
  (fmt nil "~1{~a~a~}~a" '(a b c d) 'e))

(define-equal-test iteration.03
  "E"
  (fmt nil "~0{~a~a~}~a" '(a b c d) 'e))

;; test that the `:' modifier is taken into account
(define-equal-test iteration.04
  "ABCDE"
  (fmt nil "~:{~a~a~}~a" '((a b 1) (c d 2)) 'e))

(define-equal-test iteration.05
  "ABE"
  (fmt nil "~1:{~a~a~}~a" '((a b 1) (c d 2)) 'e))

(define-equal-test iteration.06
  "E"
  (fmt nil "~0:{~a~a~}~a" '((a b 1) (c d 2)) 'e))

;; test that the `@' modifier is taken into account
(define-equal-test iteration.07
  "ABCD"
  (fmt nil "~@{~a~a~}" 'a 'b 'c 'd))

(define-equal-test iteration.08
  "ABC"
  (fmt nil "~1@{~a~a~}~a" 'a 'b 'c 'd 'e))

(define-equal-test iteration.09
  "A"
  (fmt nil "~0@{~a~a~}~a" 'a 'b 'c 'd 'e))

;; test that using both modifiers is taken into account
(define-equal-test iteration.10
  "ABCD"
  (fmt nil "~:@{~a~a~}" '(a b) '(c d)))

(define-equal-test iteration.11
  "ABE"
  (fmt nil "~1:@{~a~a~}~a" '(a b) 'e))

(define-equal-test iteration.12
  "E"
  (fmt nil "~0:@{~a~a~}~a" 'e))
