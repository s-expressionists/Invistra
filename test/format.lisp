(cl:in-package #:invistra/test)

(define-test general
  ;; multiple occurrences of a modifier
  ;; should signal an error.
  (assert-error 'error (invistra:format nil "~::d" 0))
  (assert-error 'error (invistra:format nil "~@@d" 0))
  (assert-error 'error (invistra:format nil "~@:@d" 0))
  (assert-error 'error (invistra:format nil "~:@:d" 0)))

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
                         `(invistra:format ,(cadr subexpr)
                                  (progn ,(caddr subexpr))
                                  ,@(cdddr subexpr))
                         subexpr))
                   expression)))

(define-test nesting
  (expand-format
   (assert-equal "ABCD"
                 (invistra:format nil "~{~a~}~{~a~}" '(a b) '(c d)))))

(define-test character
  ;; without a : modifier, do what write-char does
  (loop for code from 0 to 1000
        for char = (code-char code)
        do (expand-format (assert-equal
                           (with-output-to-string (stream)
                             (write-char char stream))
                           (invistra:format nil "~c" char))))
  ;; with the : modifier, if it is a printing
  ;; character, do what write-char does, and if
  ;; not do what char-name does
  (loop for code from 0 to 1000
        for char = (code-char code)
        do (if (and (graphic-char-p char)
                    (not (eql char #\Space)))
               (expand-format (assert-equal
                               (with-output-to-string (stream)
                                 (write-char char stream))
                               (invistra:format nil "~:c" char)))
               (expand-format (assert-equal
                               (char-name char)
                               (invistra:format nil "~:c" char)))))
  ;; with the @ modifier, do what prin1 does
  (loop for code from 0 to 1000
        for char = (code-char code)
        do (expand-format (assert-equal
                           (prin1-to-string char)
                           (invistra:format nil "~@c" char))))
  ;; using ~c with something other than a
  ;; character should signal an error
  (loop for thing in '(1 "string" 'symbol *standard-output*)
        do (expand-format (assert-error 'error (invistra:format nil "~c" thing))))
  ;; the character directive does not take any parameters
  (expand-format (assert-error 'error (invistra:format nil "~2c" #\a)))
  (expand-format (assert-error 'error (invistra:format nil "~'2c" #\a)))
  (expand-format (assert-error 'error (invistra:format nil "~#c" #\a)))
  (expand-format (assert-error 'error (invistra:format nil "~vc" #\a))))

(define-test newline
  ;; without any parameters, output a newline
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Newline stream))
                  (invistra:format nil "~%")))
  ;; also with a pameter of 1
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Newline stream))
                  (invistra:format nil "~1%")))
  (expand-format (assert-equal
                  ""
                  (invistra:format nil "~0%")))
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Newline stream)
                    (write-char #\Newline stream))
                  (invistra:format nil "~2%")))
  ;; The newline directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (invistra:format nil "~'a%"))
  (assert-error 'error (invistra:format nil "~1,2%"))
  ;; The newline directive takes no modifiers
  (assert-error 'error (invistra:format nil "~:%"))
  (assert-error 'error (invistra:format nil "~@%")))

(define-test fresh-line
  ;; without any parameters, does nothing to a string
  (expand-format (assert-equal
                  ""
                  (invistra:format nil "~&")))
  ;; same thing for parameter values of 0 and 1
  (expand-format (assert-equal
                  ""
                  (invistra:format nil "~0&")))
  (expand-format (assert-equal
                  ""
                  (invistra:format nil "~1&")))
  ;; for a parameter value of 2, outputs a newline
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Newline stream))
                  (invistra:format nil "~2&")))
  ;; The fresh-line directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (invistra:format nil "~'a&"))
  (assert-error 'error (invistra:format nil "~1,2&"))
  ;; The fresh-line directive takes no modifiers
  (assert-error 'error (invistra:format nil "~:&"))
  (assert-error 'error (invistra:format nil "~@&")))

(define-test page
  ;; without any parameters, outputs a page separator
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Page stream))
                  (invistra:format nil "~|")))
  ;; same thing for a parameter value of 1
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Page stream))
                  (invistra:format nil "~1|")))
  ;; with a parameter value of 0, does nothing
  (expand-format (assert-equal
                  ""
                  (invistra:format nil "~0|")))
  ;; for a parameter value of 2, outputs two page separators
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Page stream)
                    (write-char #\Page stream))
                  (invistra:format nil "~2|")))
  ;; The page directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (invistra:format nil "~'a|"))
  (assert-error 'error (invistra:format nil "~1,2|"))
  ;; The page directive takes no modifiers
  (assert-error 'error (invistra:format nil "~:|"))
  (assert-error 'error (invistra:format nil "~@|")))

(define-test tilde
  ;; without any parameters, outputs a tilde
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\~ stream))
                  (invistra:format nil "~~")))
  ;; same thing for a parameter value of 1
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\~ stream))
                  (invistra:format nil "~1~")))
  ;; with a parameter value of 0, does nothing
  (expand-format (assert-equal
                  ""
                  (invistra:format nil "~0~")))
  ;; for a parameter value of 2, outputs two tildes
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\~ stream)
                    (write-char #\~ stream))
                  (invistra:format nil "~2~")))
  ;; The tilde directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (invistra:format nil "~'a~"))
  (assert-error 'error (invistra:format nil "~1,2~"))
  ;; The tilde directive takes no modifiers
  (assert-error 'error (invistra:format nil "~:~"))
  (assert-error 'error (invistra:format nil "~@~")))

(define-test radix
  ;; English cardinal numbers
  (expand-format (assert-equal "zero" (invistra:format nil "~r" 0)))
  (expand-format (assert-equal "one" (invistra:format nil "~r" 1)))
  (expand-format (assert-equal "two" (invistra:format nil "~r" 2)))
  (expand-format (assert-equal "three" (invistra:format nil "~r" 3)))
  (expand-format (assert-equal "four" (invistra:format nil "~r" 4)))
  (expand-format (assert-equal "five" (invistra:format nil "~r" 5)))
  (expand-format (assert-equal "six" (invistra:format nil "~r" 6)))
  (expand-format (assert-equal "seven" (invistra:format nil "~r" 7)))
  (expand-format (assert-equal "eight" (invistra:format nil "~r" 8)))
  (expand-format (assert-equal "nine" (invistra:format nil "~r" 9)))
  (expand-format (assert-equal "ten" (invistra:format nil "~r" 10)))
  (expand-format (assert-equal "eleven" (invistra:format nil "~r" 11)))
  (expand-format (assert-equal "twelve" (invistra:format nil "~r" 12)))
  (expand-format (assert-equal "thirteen" (invistra:format nil "~r" 13)))
  (expand-format (assert-equal "fourteen" (invistra:format nil "~r" 14)))
  (expand-format (assert-equal "fifteen" (invistra:format nil "~r" 15)))
  (expand-format (assert-equal "sixteen" (invistra:format nil "~r" 16)))
  (expand-format (assert-equal "seventeen" (invistra:format nil "~r" 17)))
  (expand-format (assert-equal "eighteen" (invistra:format nil "~r" 18)))
  (expand-format (assert-equal "nineteen" (invistra:format nil "~r" 19)))
  (expand-format (assert-equal "twenty" (invistra:format nil "~r" 20)))
  (expand-format (assert-equal "twenty-one" (invistra:format nil "~r" 21)))
  (expand-format (assert-equal "thirty" (invistra:format nil "~r" 30)))
  (expand-format (assert-equal "fourty" (invistra:format nil "~r" 40)))
  (expand-format (assert-equal "fifty" (invistra:format nil "~r" 50)))
  (expand-format (assert-equal "sixty" (invistra:format nil "~r" 60)))
  (expand-format (assert-equal "seventy" (invistra:format nil "~r" 70)))
  (expand-format (assert-equal "eighty" (invistra:format nil "~r" 80)))
  (expand-format (assert-equal "ninety" (invistra:format nil "~r" 90)))
  (expand-format (assert-equal "one hundred" (invistra:format nil "~r" 100)))
  (expand-format (assert-equal "two hundred four" (invistra:format nil "~r" 204)))
  (expand-format (assert-equal "three hundred sixteen" (invistra:format nil "~r" 316)))
  (expand-format (assert-equal "four hundred thirty-six" (invistra:format nil "~r" 436)))
  (expand-format (assert-equal "two thousand" (invistra:format nil "~r" 2000)))
  (expand-format (assert-equal "three thousand five" (invistra:format nil "~r" 3005)))
  (expand-format (assert-equal "four thousand twelve" (invistra:format nil "~r" 4012)))
  (expand-format (assert-equal "five thousand two hundred" (invistra:format nil "~r" 5200)))
  (expand-format (assert-equal "eighty thousand" (invistra:format nil "~r" 80000)))
  (expand-format (assert-equal "five hundred thousand" (invistra:format nil "~r" 500000)))
  (expand-format (assert-equal "two million" (invistra:format nil "~r" 2000000)))
  (expand-format (assert-equal "three million six" (invistra:format nil "~r" 3000006)))
  (expand-format (assert-equal "four million two thousand" (invistra:format nil "~r" 4002000)))
  ;; English ordinal numbers
  (expand-format (assert-equal "zeroth" (invistra:format nil "~:r" 0)))
  (expand-format (assert-equal "first" (invistra:format nil "~:r" 1)))
  (expand-format (assert-equal "second" (invistra:format nil "~:r" 2)))
  (expand-format (assert-equal "third" (invistra:format nil "~:r" 3)))
  (expand-format (assert-equal "fourth" (invistra:format nil "~:r" 4)))
  (expand-format (assert-equal "fifth" (invistra:format nil "~:r" 5)))
  (expand-format (assert-equal "sixth" (invistra:format nil "~:r" 6)))
  (expand-format (assert-equal "seventh" (invistra:format nil "~:r" 7)))
  (expand-format (assert-equal "eighth" (invistra:format nil "~:r" 8)))
  (expand-format (assert-equal "ninth" (invistra:format nil "~:r" 9)))
  (expand-format (assert-equal "tenth" (invistra:format nil "~:r" 10)))
  (expand-format (assert-equal "eleventh" (invistra:format nil "~:r" 11)))
  (expand-format (assert-equal "twelvth" (invistra:format nil "~:r" 12)))
  (expand-format (assert-equal "thirteenth" (invistra:format nil "~:r" 13)))
  (expand-format (assert-equal "fourteenth" (invistra:format nil "~:r" 14)))
  (expand-format (assert-equal "fifteenth" (invistra:format nil "~:r" 15)))
  (expand-format (assert-equal "sixteenth" (invistra:format nil "~:r" 16)))
  (expand-format (assert-equal "seventeenth" (invistra:format nil "~:r" 17)))
  (expand-format (assert-equal "eighteenth" (invistra:format nil "~:r" 18)))
  (expand-format (assert-equal "nineteenth" (invistra:format nil "~:r" 19)))
  (expand-format (assert-equal "twentieth" (invistra:format nil "~:r" 20)))
  (expand-format (assert-equal "twenty-first" (invistra:format nil "~:r" 21)))
  (expand-format (assert-equal "thirtieth" (invistra:format nil "~:r" 30)))
  (expand-format (assert-equal "fourtieth" (invistra:format nil "~:r" 40)))
  (expand-format (assert-equal "fiftieth" (invistra:format nil "~:r" 50)))
  (expand-format (assert-equal "sixtieth" (invistra:format nil "~:r" 60)))
  (expand-format (assert-equal "seventieth" (invistra:format nil "~:r" 70)))
  (expand-format (assert-equal "eightieth" (invistra:format nil "~:r" 80)))
  (expand-format (assert-equal "ninetieth" (invistra:format nil "~:r" 90)))
  (expand-format (assert-equal "one hundredth" (invistra:format nil "~:r" 100)))
  (expand-format (assert-equal "two hundred fourth" (invistra:format nil "~:r" 204)))
  (expand-format (assert-equal "three hundred sixteenth" (invistra:format nil "~:r" 316)))
  (expand-format (assert-equal "four hundred thirty-sixth" (invistra:format nil "~:r" 436)))
  (expand-format (assert-equal "two thousandth" (invistra:format nil "~:r" 2000)))
  (expand-format (assert-equal "three thousand fifth" (invistra:format nil "~:r" 3005)))
  (expand-format (assert-equal "four thousand twelvth" (invistra:format nil "~:r" 4012)))
  (expand-format (assert-equal "five thousand two hundredth" (invistra:format nil "~:r" 5200)))
  (expand-format (assert-equal "eighty thousandth" (invistra:format nil "~:r" 80000)))
  (expand-format (assert-equal "five hundred thousandth" (invistra:format nil "~:r" 500000)))
  (expand-format (assert-equal "two millionth" (invistra:format nil "~:r" 2000000)))
  (expand-format (assert-equal "three million sixth" (invistra:format nil "~:r" 3000006)))
  (expand-format (assert-equal "four million two thousandth" (invistra:format nil "~:r" 4002000)))
  ;; Roman numerals
  (expand-format (assert-equal "I" (invistra:format nil "~@r" 1)))
  (expand-format (assert-equal "II" (invistra:format nil "~@r" 2)))
  (expand-format (assert-equal "III" (invistra:format nil "~@r" 3)))
  (expand-format (assert-equal "IV" (invistra:format nil "~@r" 4)))
  (expand-format (assert-equal "V" (invistra:format nil "~@r" 5)))
  (expand-format (assert-equal "VI" (invistra:format nil "~@r" 6)))
  (expand-format (assert-equal "VII" (invistra:format nil "~@r" 7)))
  (expand-format (assert-equal "VIII" (invistra:format nil "~@r" 8)))
  (expand-format (assert-equal "IX" (invistra:format nil "~@r" 9)))
  (expand-format (assert-equal "X" (invistra:format nil "~@r" 10)))
  (expand-format (assert-equal "XI" (invistra:format nil "~@r" 11)))
  (expand-format (assert-equal "XII" (invistra:format nil "~@r" 12)))
  (expand-format (assert-equal "XIII" (invistra:format nil "~@r" 13)))
  (expand-format (assert-equal "XIV" (invistra:format nil "~@r" 14)))
  (expand-format (assert-equal "XV" (invistra:format nil "~@r" 15)))
  (expand-format (assert-equal "XVI" (invistra:format nil "~@r" 16)))
  (expand-format (assert-equal "XVII" (invistra:format nil "~@r" 17)))
  (expand-format (assert-equal "XVIII" (invistra:format nil "~@r" 18)))
  (expand-format (assert-equal "XIX" (invistra:format nil "~@r" 19)))
  (expand-format (assert-equal "XX" (invistra:format nil "~@r" 20)))
  (expand-format (assert-equal "XXX" (invistra:format nil "~@r" 30)))
  (expand-format (assert-equal "XL" (invistra:format nil "~@r" 40)))
  (expand-format (assert-equal "L" (invistra:format nil "~@r" 50)))
  (expand-format (assert-equal "LXIV" (invistra:format nil "~@r" 64)))
  (expand-format (assert-equal "XCIX" (invistra:format nil "~@r" 99)))
  (expand-format (assert-equal "C" (invistra:format nil "~@r" 100)))
  (expand-format (assert-equal "CXLVII" (invistra:format nil "~@r" 147)))
  (expand-format (assert-equal "CDLXXXIX" (invistra:format nil "~@r" 489)))
  (expand-format (assert-equal "DCCCXXXI" (invistra:format nil "~@r" 831)))
  (expand-format (assert-equal "M" (invistra:format nil "~@r" 1000)))
  (expand-format (assert-equal "MMXL" (invistra:format nil "~@r" 2040)))
  (expand-format (assert-equal "MMMXC" (invistra:format nil "~@r" 3090)))
  ;; Old Roman numerals
  (expand-format (assert-equal "I" (invistra:format nil "~:@r" 1)))
  (expand-format (assert-equal "II" (invistra:format nil "~:@r" 2)))
  (expand-format (assert-equal "III" (invistra:format nil "~:@r" 3)))
  (expand-format (assert-equal "IIII" (invistra:format nil "~:@r" 4)))
  (expand-format (assert-equal "V" (invistra:format nil "~:@r" 5)))
  (expand-format (assert-equal "VI" (invistra:format nil "~:@r" 6)))
  (expand-format (assert-equal "VII" (invistra:format nil "~:@r" 7)))
  (expand-format (assert-equal "VIII" (invistra:format nil "~:@r" 8)))
  (expand-format (assert-equal "VIIII" (invistra:format nil "~:@r" 9)))
  (expand-format (assert-equal "X" (invistra:format nil "~:@r" 10)))
  (expand-format (assert-equal "XI" (invistra:format nil "~:@r" 11)))
  (expand-format (assert-equal "XII" (invistra:format nil "~:@r" 12)))
  (expand-format (assert-equal "XIII" (invistra:format nil "~:@r" 13)))
  (expand-format (assert-equal "XIIII" (invistra:format nil "~:@r" 14)))
  (expand-format (assert-equal "XV" (invistra:format nil "~:@r" 15)))
  (expand-format (assert-equal "XVI" (invistra:format nil "~:@r" 16)))
  (expand-format (assert-equal "XVII" (invistra:format nil "~:@r" 17)))
  (expand-format (assert-equal "XVIII" (invistra:format nil "~:@r" 18)))
  (expand-format (assert-equal "XVIIII" (invistra:format nil "~:@r" 19)))
  (expand-format (assert-equal "XX" (invistra:format nil "~:@r" 20)))
  (expand-format (assert-equal "XXX" (invistra:format nil "~:@r" 30)))
  (expand-format (assert-equal "XXXX" (invistra:format nil "~:@r" 40)))
  (expand-format (assert-equal "L" (invistra:format nil "~:@r" 50)))
  (expand-format (assert-equal "LXIIII" (invistra:format nil "~:@r" 64)))
  (expand-format (assert-equal "LXXXXVIIII" (invistra:format nil "~:@r" 99)))
  (expand-format (assert-equal "C" (invistra:format nil "~:@r" 100)))
  (expand-format (assert-equal "CXXXXVII" (invistra:format nil "~:@r" 147)))
  (expand-format (assert-equal "CCCCLXXXVIIII" (invistra:format nil "~:@r" 489)))
  (expand-format (assert-equal "DCCCXXXI" (invistra:format nil "~:@r" 831)))
  (expand-format (assert-equal "M" (invistra:format nil "~:@r" 1000)))
  (expand-format (assert-equal "MMXXXX" (invistra:format nil "~:@r" 2040)))
  (expand-format (assert-equal "MMMLXXXX" (invistra:format nil "~:@r" 3090)))
  ;; test the use of different values of the radix
  (loop for radix from 2 to 36
        do (loop repeat 1000
                 do (let ((value (random (expt 10 100))))
                      (expand-format
                       (assert-equal
                        (with-output-to-string (stream)
                          (let ((*print-base* radix))
                            (princ value stream)))
                        (invistra:format nil "~vr" radix value))))))
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "123"
                 (invistra:format nil "~10,1r" 123)))
  (expand-format
   (assert-equal "123"
                 (invistra:format nil "~10,2r" 123)))
  (expand-format
   (assert-equal "123"
                 (invistra:format nil "~10,3r" 123)))
  (expand-format
   (assert-equal " 123"
                 (invistra:format nil "~10,4r" 123)))
  (expand-format
   (assert-equal "  123"
                 (invistra:format nil "~10,5r" 123)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx123"
                 (invistra:format nil "~10,5,'xr" 123)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx123"
                 (invistra:format nil "~10,5,'x:r" 123)))
   (expand-format
    (assert-equal "xx1,234"
                 (invistra:format nil "~10,7,'x:r" 1234)))
   (expand-format
    (assert-equal "xx551,234"
                 (invistra:format nil "~10,9,'x:r" 551234)))
   (expand-format
    (assert-equal "xx66,551,234"
                 (invistra:format nil "~10,12,'x:r" 66551234)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx66a551a234"
                 (invistra:format nil "~10,12,'x,'a:r" 66551234))))

(define-test decimal
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "123"
                 (invistra:format nil "~1d" 123)))
  (expand-format
   (assert-equal "123"
                 (invistra:format nil "~2d" 123)))
  (expand-format
   (assert-equal "123"
                 (invistra:format nil "~3d" 123)))
  (expand-format
   (assert-equal " 123"
                 (invistra:format nil "~4d" 123)))
  (expand-format
   (assert-equal "  123"
                 (invistra:format nil "~5d" 123)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx123"
                 (invistra:format nil "~5,'xd" 123)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx123"
                 (invistra:format nil "~5,'x:d" 123)))
   (expand-format
    (assert-equal "xx1,234"
                 (invistra:format nil "~7,'x:d" 1234)))
   (expand-format
    (assert-equal "xx551,234"
                 (invistra:format nil "~9,'x:d" 551234)))
   (expand-format
    (assert-equal "xx66,551,234"
                 (invistra:format nil "~12,'x:d" 66551234)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx66a551a234"
                 (invistra:format nil "~12,'x,'a:d" 66551234))))

(define-test octal
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "123"
                 (invistra:format nil "~1o" #o123)))
  (expand-format
   (assert-equal "123"
                 (invistra:format nil "~2o" #o123)))
  (expand-format
   (assert-equal "123"
                 (invistra:format nil "~3o" #o123)))
  (expand-format
   (assert-equal " 123"
                 (invistra:format nil "~4o" #o123)))
  (expand-format
   (assert-equal "  123"
                 (invistra:format nil "~5o" #o123)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx123"
                 (invistra:format nil "~5,'xo" #o123)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx123"
                 (invistra:format nil "~5,'x:o" #o123)))
   (expand-format
    (assert-equal "xx1,234"
                 (invistra:format nil "~7,'x:o" #o1234)))
   (expand-format
    (assert-equal "xx551,234"
                 (invistra:format nil "~9,'x:o" #o551234)))
   (expand-format
    (assert-equal "xx66,551,234"
                 (invistra:format nil "~12,'x:o" #o66551234)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx66a551a234"
                 (invistra:format nil "~12,'x,'a:o" #o66551234))))

(define-test binary
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "101"
                 (invistra:format nil "~1b" #b101)))
  (expand-format
   (assert-equal "101"
                 (invistra:format nil "~2b" #b101)))
  (expand-format
   (assert-equal "101"
                 (invistra:format nil "~3b" #b101)))
  (expand-format
   (assert-equal " 101"
                 (invistra:format nil "~4b" #b101)))
  (expand-format
   (assert-equal "  101"
                 (invistra:format nil "~5b" #b101)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx101"
                 (invistra:format nil "~5,'xb" #b101)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx101"
                 (invistra:format nil "~5,'x:b" #b101)))
   (expand-format
    (assert-equal "xx1,011"
                 (invistra:format nil "~7,'x:b" #b1011)))
   (expand-format
    (assert-equal "xx111,011"
                 (invistra:format nil "~9,'x:b" #b111011)))
   (expand-format
    (assert-equal "xx10,111,011"
                 (invistra:format nil "~12,'x:b" #b10111011)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx10a111a011"
                 (invistra:format nil "~12,'x,'a:b" #b10111011))))

(define-test aesthetic
  ;; Test that objects are printed as with princ
  (loop for obj in '(234 -10 1.5 'abc "hello" #\x #\Space nil)
        do (expand-format
            (assert-equal (princ-to-string obj)
                          (invistra:format nil "~a" obj))))
  ;; Test that with a `:' modifier, NIL will print as ()
  (expand-format
   (assert-equal "()"
                 (invistra:format nil "~:a" nil)))
  ;; Test that the mincol argument is taken into account
  (expand-format
   (assert-equal "hello  "
                 (invistra:format nil "~7a" "hello")))
  ;; Test that the `@' modifier is taken into account
  (expand-format
   (assert-equal "  hello"
                 (invistra:format nil "~7@a" "hello")))
  ;; Test that the colinc parameter is taken into account
  (expand-format
   (assert-equal "hello                     "
                 (invistra:format nil "~21,7a" "hello")))
  ;; Test that the minpad parameter is taken into account
  (expand-format
   (assert-equal "hello   "
                 (invistra:format nil "~,,3a" "hello")))
  ;; Test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "helloxxx"
                 (invistra:format nil "~0,1,3,'xa" "hello"))))

(define-test standard
  ;; Test that objects are printed as with princ
  (loop for obj in '(234 -10 1.5 'abc "hello" #\x #\Space nil)
        do (expand-format
            (assert-equal (prin1-to-string obj)
                          (invistra:format nil "~s" obj))))
  ;; Test that with a `:' modifier, NIL will print as ()
  (expand-format
   (assert-equal "()"
                 (invistra:format nil "~:s" nil)))
  ;; Test that the mincol argument is taken into account
  (expand-format
   (assert-equal "12345  "
                 (invistra:format nil "~7s" 12345)))
  ;; Test that the `@' modifier is taken into account
  (expand-format
   (assert-equal "  12345"
                 (invistra:format nil "~7@s" 12345)))
  ;; Test that the colinc parameter is taken into account
  (expand-format
   (assert-equal "12345                     "
                 (invistra:format nil "~21,7s" 12345)))
  ;; Test that the minpad parameter is taken into account
  (expand-format
   (assert-equal "12345   "
                 (invistra:format nil "~,,3s" 12345)))
  ;; Test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "12345xxx"
                 (invistra:format nil "~0,1,3,'xs" 12345))))

(define-test write
  ;; Test that objects are renendered as with write.
  (loop for obj in '(234 -10 1.5 'abc "hello" #\x #\Space nil)
        do (expand-format
            (assert-equal (with-output-to-string (stream)
                            (write obj :stream stream))
                          (invistra:format nil "~s" obj))))
  ;; test that it can handle circular lists
  (let ((*print-circle* t))
    (expand-format
     (assert-equal "#1=(1 . #1#)"
                   (invistra:format nil "~w"
                           (let ((l (list 1)))
                             (setf (cdr l) l))))))

  ;; test that this directive reports an error
  ;; if a parameter is given
  (assert-error 'error (invistra:format nil "~1w" 234))
  (assert-error 'error (invistra:format nil "~'aw" 234)))

(define-test go-to
  (expand-format
   (assert-equal "ac"
                 (invistra:format nil "~c~*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "ab"
                 (invistra:format nil "~c~0*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "ac"
                 (invistra:format nil "~c~1*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "ad"
                 (invistra:format nil "~c~2*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "aa"
                 (invistra:format nil "~c~:*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "ab"
                 (invistra:format nil "~c~0:*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "aa"
                 (invistra:format nil "~c~1:*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "aba"
                 (invistra:format nil "~c~c~2:*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "aba"
                 (invistra:format nil "~c~c~@*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "abb"
                 (invistra:format nil "~c~c~1@*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "abd"
                 (invistra:format nil "~c~c~3@*~c" #\a #\b #\c #\d)))
  ;; Test that going beyond the first or last argument
  ;; gives an error.
  (assert-error 'error
                (invistra:format nil "~c~c~*" #\a #\b))
  (assert-error 'error
                (invistra:format nil "~c~c~2*~:2*~c" #\a #\b #\c))
  (assert-error 'error
                (invistra:format nil "~c~:2*~2*~c" #\a #\b #\c))
  (assert-error 'error
                (invistra:format nil "~c~-1@*~0@*~c" #\a #\b #\c))
  (assert-error 'error
                (invistra:format nil "~c~4@*~0@*~c" #\a #\b #\c)))

(define-test conditional
  (expand-format
   (assert-equal "abc"
                 (invistra:format nil "~[xyz~;abc~;def~]" 1)))
  (expand-format
   (assert-equal "xyz"
                 (invistra:format nil "~[xyz~;abc~;def~]" 0)))
  (expand-format
   (assert-equal ""
                 (invistra:format nil "~[xyz~;abc~;def~]" 3)))
  ;; test the default clause
  (expand-format
   (assert-equal "abc"
                 (invistra:format nil "~[xyz~;abc~:;def~]" 1)))
  (expand-format
   (assert-equal "xyz"
                 (invistra:format nil "~[xyz~;abc~:;def~]" 0)))
  (expand-format
   (assert-equal "def"
                 (invistra:format nil "~[xyz~;abc~:;def~]" 3)))
  (expand-format
   (assert-equal "abc"
                 (invistra:format nil "~:[xyz~;abc~]" nil)))
  (expand-format
   (assert-equal "xyz"
                 (invistra:format nil "~:[xyz~;abc~]" 24)))
  (expand-format
   (assert-equal "xyz23"
                 (invistra:format nil "~@[xyz~]~d" 23)))
  (expand-format
   (assert-equal "23"
                 (invistra:format nil "~@[xyz~]~d" nil 23)))
  ;; test the use of the parameter instead of the argument
  (expand-format
   (assert-equal "abc"
                 (invistra:format nil "~#[xyz~;abc~;def~]" 10)))
  (expand-format
   (assert-equal "xyz"
                 (invistra:format nil "~#[xyz~;abc~;def~]")))
  (expand-format
   (assert-equal ""
                 (invistra:format nil "~#[xyz~;abc~;def~]" 10 10 10)))
  (expand-format
   (assert-equal "abc"
                 (invistra:format nil "~v[xyz~;abc~;def~]" 1)))
  (expand-format
   (assert-equal "xyz"
                 (invistra:format nil "~v[xyz~;abc~;def~]" 0)))
  (expand-format
   (assert-equal ""
                 (invistra:format nil "~v[xyz~;abc~;def~]" 3)))
  (expand-format
   (assert-equal "abc"
                 (invistra:format nil "~1[xyz~;abc~;def~]" 10)))
  (expand-format
   (assert-equal "xyz"
                 (invistra:format nil "~0[xyz~;abc~;def~]" 10)))
  (expand-format
   (assert-equal ""
                 (invistra:format nil "~3[xyz~;abc~;def~]" 10)))
  ;; test that giving the : modifier fails if there
  ;; are not exactly two clauses
  (assert-error 'error
                (invistra:format nil "~:[xyz~;abc~;def~]" nil))
  (assert-error 'error
                (invistra:format nil "~:[xyz~]" nil))
  ;; test that giving the @ modifier fails if there
  ;; is not exactly one clause
  (assert-error 'error
                (invistra:format nil "~@[xyz~;abc~]~d" nil 23))
  ;; test that giving no clauses fails
  (assert-error 'error
                (invistra:format nil "~[~]" nil 23))
  ;; test that giving both modifiers gives an error.
  (assert-error 'error
                (invistra:format nil "~:@[xyz~;abc~;def~]" 1 2 3))
  ;; test that giving the : modifier to a clause separator
  ;; other than the last gives an error
  (assert-error 'error
                (invistra:format nil "~[xyz~:;abc~:;def~]" 3))
  ;; test that giving the modifiers to ~] gives an error
  ;; test that giving parameters to ~; or ~] gives an error
  (assert-error 'error
                (invistra:format nil "~[xyz~;abc~;def~2]" 3))
  (assert-error 'error
                (invistra:format nil "~[xyz~;abc~2;def~]" 3))
  (assert-error 'error
                (invistra:format nil "~[xyz~;abc~;def~#]" 3))
  (assert-error 'error
                (invistra:format nil "~[xyz~;abc~#;def~]" 3))
  (assert-error 'error
                (invistra:format nil "~[xyz~;abc~;def~v]" 3))
  (assert-error 'error
                (invistra:format nil "~[xyz~;abc~v;def~]" 3)))

(define-test iteration
  (expand-format
   (assert-equal "ABCDE"
                 (invistra:format nil "~{~a~a~}~a" '(a b c d) 'e)))
  ;; test that, with a parameter, at most that many
  ;; iterations are done.
  (expand-format
   (assert-equal "ABE"
                 (invistra:format nil "~1{~a~a~}~a" '(a b c d) 'e)))
  (expand-format
   (assert-equal "E"
                 (invistra:format nil "~0{~a~a~}~a" '(a b c d) 'e)))
  ;; test that the `:' modifier is taken into account
  (expand-format
   (assert-equal "ABCDE"
                 (invistra:format nil "~:{~a~a~}~a" '((a b 1) (c d 2)) 'e)))
  (expand-format
   (assert-equal "ABE"
                 (invistra:format nil "~1:{~a~a~}~a" '((a b 1) (c d 2)) 'e)))
  (expand-format
   (assert-equal "E"
                 (invistra:format nil "~0:{~a~a~}~a" '((a b 1) (c d 2)) 'e)))
  ;; test that the `@' modifier is taken into account
  (expand-format
   (assert-equal "ABCD"
                 (invistra:format nil "~@{~a~a~}" 'a 'b 'c 'd)))
  (expand-format
   (assert-equal "ABC"
                 (invistra:format nil "~1@{~a~a~}~a" 'a 'b 'c 'd 'e)))
  (expand-format
   (assert-equal "A"
                 (invistra:format nil "~0@{~a~a~}~a" 'a 'b 'c 'd 'e)))
  ;; test that using both modifiers is taken into account
  (expand-format
   (assert-equal "ABCD"
                 (invistra:format nil "~:@{~a~a~}" '(a b) '(c d))))
  (expand-format
   (assert-equal "ABE"
                 (invistra:format nil "~1:@{~a~a~}~a" '(a b) 'e)))
  (expand-format
   (assert-equal "E"
                 (invistra:format nil "~0:@{~a~a~}~a" 'e))))

(defun format-test ()
  (let ((*package* (find-package :invistra/test)))
    (run-tests :all)))
