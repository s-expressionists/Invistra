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
     :compile-at :execute
     (my-with-standard-io-syntax
       (macrolet ((fmt (destination control-string &rest args)
                    `(invistra-extrinsic:format ,destination ,control-string ,@args)))
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

#|(define-test newline
  ;; without any parameters, output a newline
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Newline stream))
                  (fmt nil "~%")))
  ;; also with a pameter of 1
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Newline stream))
                  (fmt nil "~1%")))
  (expand-format (assert-equal
                  ""
                  (fmt nil "~0%")))
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Newline stream)
                    (write-char #\Newline stream))
                  (fmt nil "~2%")))
  ;; The newline directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (fmt nil "~'a%"))
  (assert-error 'error (fmt nil "~1,2%"))
  ;; The newline directive takes no modifiers
  (assert-error 'error (fmt nil "~:%"))
  (assert-error 'error (fmt nil "~@%")))

(define-test fresh-line
  ;; without any parameters, does nothing to a string
  (expand-format (assert-equal
                  ""
                  (fmt nil "~&")))
  ;; same thing for parameter values of 0 and 1
  (expand-format (assert-equal
                  ""
                  (fmt nil "~0&")))
  (expand-format (assert-equal
                  ""
                  (fmt nil "~1&")))
  ;; for a parameter value of 2, outputs a newline
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Newline stream))
                  (fmt nil "~2&")))
  ;; The fresh-line directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (fmt nil "~'a&"))
  (assert-error 'error (fmt nil "~1,2&"))
  ;; The fresh-line directive takes no modifiers
  (assert-error 'error (fmt nil "~:&"))
  (assert-error 'error (fmt nil "~@&")))

(define-test page
  ;; without any parameters, outputs a page separator
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Page stream))
                  (fmt nil "~|")))
  ;; same thing for a parameter value of 1
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Page stream))
                  (fmt nil "~1|")))
  ;; with a parameter value of 0, does nothing
  (expand-format (assert-equal
                  ""
                  (fmt nil "~0|")))
  ;; for a parameter value of 2, outputs two page separators
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\Page stream)
                    (write-char #\Page stream))
                  (fmt nil "~2|")))
  ;; The page directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (fmt nil "~'a|"))
  (assert-error 'error (fmt nil "~1,2|"))
  ;; The page directive takes no modifiers
  (assert-error 'error (fmt nil "~:|"))
  (assert-error 'error (fmt nil "~@|")))

(define-test tilde
  ;; without any parameters, outputs a tilde
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\~ stream))
                  (fmt nil "~~")))
  ;; same thing for a parameter value of 1
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\~ stream))
                  (fmt nil "~1~")))
  ;; with a parameter value of 0, does nothing
  (expand-format (assert-equal
                  ""
                  (fmt nil "~0~")))
  ;; for a parameter value of 2, outputs two tildes
  (expand-format (assert-equal
                  (with-output-to-string (stream)
                    (write-char #\~ stream)
                    (write-char #\~ stream))
                  (fmt nil "~2~")))
  ;; The tilde directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (fmt nil "~'a~"))
  (assert-error 'error (fmt nil "~1,2~"))
  ;; The tilde directive takes no modifiers
  (assert-error 'error (fmt nil "~:~"))
  (assert-error 'error (fmt nil "~@~")))

(define-test radix
  ;; English cardinal numbers
  (expand-format (assert-equal "zero" (fmt nil "~r" 0)))
  (expand-format (assert-equal "one" (fmt nil "~r" 1)))
  (expand-format (assert-equal "two" (fmt nil "~r" 2)))
  (expand-format (assert-equal "three" (fmt nil "~r" 3)))
  (expand-format (assert-equal "four" (fmt nil "~r" 4)))
  (expand-format (assert-equal "five" (fmt nil "~r" 5)))
  (expand-format (assert-equal "six" (fmt nil "~r" 6)))
  (expand-format (assert-equal "seven" (fmt nil "~r" 7)))
  (expand-format (assert-equal "eight" (fmt nil "~r" 8)))
  (expand-format (assert-equal "nine" (fmt nil "~r" 9)))
  (expand-format (assert-equal "ten" (fmt nil "~r" 10)))
  (expand-format (assert-equal "eleven" (fmt nil "~r" 11)))
  (expand-format (assert-equal "twelve" (fmt nil "~r" 12)))
  (expand-format (assert-equal "thirteen" (fmt nil "~r" 13)))
  (expand-format (assert-equal "fourteen" (fmt nil "~r" 14)))
  (expand-format (assert-equal "fifteen" (fmt nil "~r" 15)))
  (expand-format (assert-equal "sixteen" (fmt nil "~r" 16)))
  (expand-format (assert-equal "seventeen" (fmt nil "~r" 17)))
  (expand-format (assert-equal "eighteen" (fmt nil "~r" 18)))
  (expand-format (assert-equal "nineteen" (fmt nil "~r" 19)))
  (expand-format (assert-equal "twenty" (fmt nil "~r" 20)))
  (expand-format (assert-equal "twenty-one" (fmt nil "~r" 21)))
  (expand-format (assert-equal "thirty" (fmt nil "~r" 30)))
  (expand-format (assert-equal "fourty" (fmt nil "~r" 40)))
  (expand-format (assert-equal "fifty" (fmt nil "~r" 50)))
  (expand-format (assert-equal "sixty" (fmt nil "~r" 60)))
  (expand-format (assert-equal "seventy" (fmt nil "~r" 70)))
  (expand-format (assert-equal "eighty" (fmt nil "~r" 80)))
  (expand-format (assert-equal "ninety" (fmt nil "~r" 90)))
  (expand-format (assert-equal "one hundred" (fmt nil "~r" 100)))
  (expand-format (assert-equal "two hundred four" (fmt nil "~r" 204)))
  (expand-format (assert-equal "three hundred sixteen" (fmt nil "~r" 316)))
  (expand-format (assert-equal "four hundred thirty-six" (fmt nil "~r" 436)))
  (expand-format (assert-equal "two thousand" (fmt nil "~r" 2000)))
  (expand-format (assert-equal "three thousand five" (fmt nil "~r" 3005)))
  (expand-format (assert-equal "four thousand twelve" (fmt nil "~r" 4012)))
  (expand-format (assert-equal "five thousand two hundred" (fmt nil "~r" 5200)))
  (expand-format (assert-equal "eighty thousand" (fmt nil "~r" 80000)))
  (expand-format (assert-equal "five hundred thousand" (fmt nil "~r" 500000)))
  (expand-format (assert-equal "two million" (fmt nil "~r" 2000000)))
  (expand-format (assert-equal "three million six" (fmt nil "~r" 3000006)))
  (expand-format (assert-equal "four million two thousand" (fmt nil "~r" 4002000)))
  ;; English ordinal numbers
  (expand-format (assert-equal "zeroth" (fmt nil "~:r" 0)))
  (expand-format (assert-equal "first" (fmt nil "~:r" 1)))
  (expand-format (assert-equal "second" (fmt nil "~:r" 2)))
  (expand-format (assert-equal "third" (fmt nil "~:r" 3)))
  (expand-format (assert-equal "fourth" (fmt nil "~:r" 4)))
  (expand-format (assert-equal "fifth" (fmt nil "~:r" 5)))
  (expand-format (assert-equal "sixth" (fmt nil "~:r" 6)))
  (expand-format (assert-equal "seventh" (fmt nil "~:r" 7)))
  (expand-format (assert-equal "eighth" (fmt nil "~:r" 8)))
  (expand-format (assert-equal "ninth" (fmt nil "~:r" 9)))
  (expand-format (assert-equal "tenth" (fmt nil "~:r" 10)))
  (expand-format (assert-equal "eleventh" (fmt nil "~:r" 11)))
  (expand-format (assert-equal "twelvth" (fmt nil "~:r" 12)))
  (expand-format (assert-equal "thirteenth" (fmt nil "~:r" 13)))
  (expand-format (assert-equal "fourteenth" (fmt nil "~:r" 14)))
  (expand-format (assert-equal "fifteenth" (fmt nil "~:r" 15)))
  (expand-format (assert-equal "sixteenth" (fmt nil "~:r" 16)))
  (expand-format (assert-equal "seventeenth" (fmt nil "~:r" 17)))
  (expand-format (assert-equal "eighteenth" (fmt nil "~:r" 18)))
  (expand-format (assert-equal "nineteenth" (fmt nil "~:r" 19)))
  (expand-format (assert-equal "twentieth" (fmt nil "~:r" 20)))
  (expand-format (assert-equal "twenty-first" (fmt nil "~:r" 21)))
  (expand-format (assert-equal "thirtieth" (fmt nil "~:r" 30)))
  (expand-format (assert-equal "fourtieth" (fmt nil "~:r" 40)))
  (expand-format (assert-equal "fiftieth" (fmt nil "~:r" 50)))
  (expand-format (assert-equal "sixtieth" (fmt nil "~:r" 60)))
  (expand-format (assert-equal "seventieth" (fmt nil "~:r" 70)))
  (expand-format (assert-equal "eightieth" (fmt nil "~:r" 80)))
  (expand-format (assert-equal "ninetieth" (fmt nil "~:r" 90)))
  (expand-format (assert-equal "one hundredth" (fmt nil "~:r" 100)))
  (expand-format (assert-equal "two hundred fourth" (fmt nil "~:r" 204)))
  (expand-format (assert-equal "three hundred sixteenth" (fmt nil "~:r" 316)))
  (expand-format (assert-equal "four hundred thirty-sixth" (fmt nil "~:r" 436)))
  (expand-format (assert-equal "two thousandth" (fmt nil "~:r" 2000)))
  (expand-format (assert-equal "three thousand fifth" (fmt nil "~:r" 3005)))
  (expand-format (assert-equal "four thousand twelvth" (fmt nil "~:r" 4012)))
  (expand-format (assert-equal "five thousand two hundredth" (fmt nil "~:r" 5200)))
  (expand-format (assert-equal "eighty thousandth" (fmt nil "~:r" 80000)))
  (expand-format (assert-equal "five hundred thousandth" (fmt nil "~:r" 500000)))
  (expand-format (assert-equal "two millionth" (fmt nil "~:r" 2000000)))
  (expand-format (assert-equal "three million sixth" (fmt nil "~:r" 3000006)))
  (expand-format (assert-equal "four million two thousandth" (fmt nil "~:r" 4002000)))
  ;; Roman numerals
  (expand-format (assert-equal "I" (fmt nil "~@r" 1)))
  (expand-format (assert-equal "II" (fmt nil "~@r" 2)))
  (expand-format (assert-equal "III" (fmt nil "~@r" 3)))
  (expand-format (assert-equal "IV" (fmt nil "~@r" 4)))
  (expand-format (assert-equal "V" (fmt nil "~@r" 5)))
  (expand-format (assert-equal "VI" (fmt nil "~@r" 6)))
  (expand-format (assert-equal "VII" (fmt nil "~@r" 7)))
  (expand-format (assert-equal "VIII" (fmt nil "~@r" 8)))
  (expand-format (assert-equal "IX" (fmt nil "~@r" 9)))
  (expand-format (assert-equal "X" (fmt nil "~@r" 10)))
  (expand-format (assert-equal "XI" (fmt nil "~@r" 11)))
  (expand-format (assert-equal "XII" (fmt nil "~@r" 12)))
  (expand-format (assert-equal "XIII" (fmt nil "~@r" 13)))
  (expand-format (assert-equal "XIV" (fmt nil "~@r" 14)))
  (expand-format (assert-equal "XV" (fmt nil "~@r" 15)))
  (expand-format (assert-equal "XVI" (fmt nil "~@r" 16)))
  (expand-format (assert-equal "XVII" (fmt nil "~@r" 17)))
  (expand-format (assert-equal "XVIII" (fmt nil "~@r" 18)))
  (expand-format (assert-equal "XIX" (fmt nil "~@r" 19)))
  (expand-format (assert-equal "XX" (fmt nil "~@r" 20)))
  (expand-format (assert-equal "XXX" (fmt nil "~@r" 30)))
  (expand-format (assert-equal "XL" (fmt nil "~@r" 40)))
  (expand-format (assert-equal "L" (fmt nil "~@r" 50)))
  (expand-format (assert-equal "LXIV" (fmt nil "~@r" 64)))
  (expand-format (assert-equal "XCIX" (fmt nil "~@r" 99)))
  (expand-format (assert-equal "C" (fmt nil "~@r" 100)))
  (expand-format (assert-equal "CXLVII" (fmt nil "~@r" 147)))
  (expand-format (assert-equal "CDLXXXIX" (fmt nil "~@r" 489)))
  (expand-format (assert-equal "DCCCXXXI" (fmt nil "~@r" 831)))
  (expand-format (assert-equal "M" (fmt nil "~@r" 1000)))
  (expand-format (assert-equal "MMXL" (fmt nil "~@r" 2040)))
  (expand-format (assert-equal "MMMXC" (fmt nil "~@r" 3090)))
  ;; Old Roman numerals
  (expand-format (assert-equal "I" (fmt nil "~:@r" 1)))
  (expand-format (assert-equal "II" (fmt nil "~:@r" 2)))
  (expand-format (assert-equal "III" (fmt nil "~:@r" 3)))
  (expand-format (assert-equal "IIII" (fmt nil "~:@r" 4)))
  (expand-format (assert-equal "V" (fmt nil "~:@r" 5)))
  (expand-format (assert-equal "VI" (fmt nil "~:@r" 6)))
  (expand-format (assert-equal "VII" (fmt nil "~:@r" 7)))
  (expand-format (assert-equal "VIII" (fmt nil "~:@r" 8)))
  (expand-format (assert-equal "VIIII" (fmt nil "~:@r" 9)))
  (expand-format (assert-equal "X" (fmt nil "~:@r" 10)))
  (expand-format (assert-equal "XI" (fmt nil "~:@r" 11)))
  (expand-format (assert-equal "XII" (fmt nil "~:@r" 12)))
  (expand-format (assert-equal "XIII" (fmt nil "~:@r" 13)))
  (expand-format (assert-equal "XIIII" (fmt nil "~:@r" 14)))
  (expand-format (assert-equal "XV" (fmt nil "~:@r" 15)))
  (expand-format (assert-equal "XVI" (fmt nil "~:@r" 16)))
  (expand-format (assert-equal "XVII" (fmt nil "~:@r" 17)))
  (expand-format (assert-equal "XVIII" (fmt nil "~:@r" 18)))
  (expand-format (assert-equal "XVIIII" (fmt nil "~:@r" 19)))
  (expand-format (assert-equal "XX" (fmt nil "~:@r" 20)))
  (expand-format (assert-equal "XXX" (fmt nil "~:@r" 30)))
  (expand-format (assert-equal "XXXX" (fmt nil "~:@r" 40)))
  (expand-format (assert-equal "L" (fmt nil "~:@r" 50)))
  (expand-format (assert-equal "LXIIII" (fmt nil "~:@r" 64)))
  (expand-format (assert-equal "LXXXXVIIII" (fmt nil "~:@r" 99)))
  (expand-format (assert-equal "C" (fmt nil "~:@r" 100)))
  (expand-format (assert-equal "CXXXXVII" (fmt nil "~:@r" 147)))
  (expand-format (assert-equal "CCCCLXXXVIIII" (fmt nil "~:@r" 489)))
  (expand-format (assert-equal "DCCCXXXI" (fmt nil "~:@r" 831)))
  (expand-format (assert-equal "M" (fmt nil "~:@r" 1000)))
  (expand-format (assert-equal "MMXXXX" (fmt nil "~:@r" 2040)))
  (expand-format (assert-equal "MMMLXXXX" (fmt nil "~:@r" 3090)))
  ;; test the use of different values of the radix
  (loop for radix from 2 to 36
        do (loop repeat 1000
                 do (let ((value (random (expt 10 100))))
                      (expand-format
                       (assert-equal
                        (with-output-to-string (stream)
                          (let ((*print-base* radix))
                            (princ value stream)))
                        (fmt nil "~vr" radix value))))))
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "123"
                 (fmt nil "~10,1r" 123)))
  (expand-format
   (assert-equal "123"
                 (fmt nil "~10,2r" 123)))
  (expand-format
   (assert-equal "123"
                 (fmt nil "~10,3r" 123)))
  (expand-format
   (assert-equal " 123"
                 (fmt nil "~10,4r" 123)))
  (expand-format
   (assert-equal "  123"
                 (fmt nil "~10,5r" 123)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx123"
                 (fmt nil "~10,5,'xr" 123)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx123"
                 (fmt nil "~10,5,'x:r" 123)))
   (expand-format
    (assert-equal "xx1,234"
                 (fmt nil "~10,7,'x:r" 1234)))
   (expand-format
    (assert-equal "xx551,234"
                 (fmt nil "~10,9,'x:r" 551234)))
   (expand-format
    (assert-equal "xx66,551,234"
                 (fmt nil "~10,12,'x:r" 66551234)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx66a551a234"
                 (fmt nil "~10,12,'x,'a:r" 66551234))))

(define-test decimal
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "123"
                 (fmt nil "~1d" 123)))
  (expand-format
   (assert-equal "123"
                 (fmt nil "~2d" 123)))
  (expand-format
   (assert-equal "123"
                 (fmt nil "~3d" 123)))
  (expand-format
   (assert-equal " 123"
                 (fmt nil "~4d" 123)))
  (expand-format
   (assert-equal "  123"
                 (fmt nil "~5d" 123)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx123"
                 (fmt nil "~5,'xd" 123)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx123"
                 (fmt nil "~5,'x:d" 123)))
   (expand-format
    (assert-equal "xx1,234"
                 (fmt nil "~7,'x:d" 1234)))
   (expand-format
    (assert-equal "xx551,234"
                 (fmt nil "~9,'x:d" 551234)))
   (expand-format
    (assert-equal "xx66,551,234"
                 (fmt nil "~12,'x:d" 66551234)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx66a551a234"
                 (fmt nil "~12,'x,'a:d" 66551234))))

(define-test octal
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "123"
                 (fmt nil "~1o" #o123)))
  (expand-format
   (assert-equal "123"
                 (fmt nil "~2o" #o123)))
  (expand-format
   (assert-equal "123"
                 (fmt nil "~3o" #o123)))
  (expand-format
   (assert-equal " 123"
                 (fmt nil "~4o" #o123)))
  (expand-format
   (assert-equal "  123"
                 (fmt nil "~5o" #o123)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx123"
                 (fmt nil "~5,'xo" #o123)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx123"
                 (fmt nil "~5,'x:o" #o123)))
   (expand-format
    (assert-equal "xx1,234"
                 (fmt nil "~7,'x:o" #o1234)))
   (expand-format
    (assert-equal "xx551,234"
                 (fmt nil "~9,'x:o" #o551234)))
   (expand-format
    (assert-equal "xx66,551,234"
                 (fmt nil "~12,'x:o" #o66551234)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx66a551a234"
                 (fmt nil "~12,'x,'a:o" #o66551234))))

(define-test binary
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "101"
                 (fmt nil "~1b" #b101)))
  (expand-format
   (assert-equal "101"
                 (fmt nil "~2b" #b101)))
  (expand-format
   (assert-equal "101"
                 (fmt nil "~3b" #b101)))
  (expand-format
   (assert-equal " 101"
                 (fmt nil "~4b" #b101)))
  (expand-format
   (assert-equal "  101"
                 (fmt nil "~5b" #b101)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx101"
                 (fmt nil "~5,'xb" #b101)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx101"
                 (fmt nil "~5,'x:b" #b101)))
   (expand-format
    (assert-equal "xx1,011"
                 (fmt nil "~7,'x:b" #b1011)))
   (expand-format
    (assert-equal "xx111,011"
                 (fmt nil "~9,'x:b" #b111011)))
   (expand-format
    (assert-equal "xx10,111,011"
                 (fmt nil "~12,'x:b" #b10111011)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx10a111a011"
                 (fmt nil "~12,'x,'a:b" #b10111011))))

(define-test aesthetic
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

(define-test standard
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

|#

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
  "hello"
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
