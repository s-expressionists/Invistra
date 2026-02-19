(defpackage #:invistra
  (:use #:cl)
  (:shadow #:format
           #:formatter)
  (:export #:*format-output*
           #:*roman-digits*
           #:at-sign-p
           #:check-item-syntax
           #:coerce-function-designator
           #:colon-p
           #:compile-item
           #:define-interface
           #:directive
           #:format
           #:format-compiler-macro
           #:formatter
           #:go-to-argument
           #:go-to-argument-forms
           #:inner-exit
           #:inner-exit-forms
           #:inner-exit-if-exhausted
           #:inner-exit-if-exhausted-forms
           #:interpret-item
           #:make-argument-cursor
           #:outer-exit
           #:outer-exit-forms
           #:outer-exit-if-exhausted
           #:parameter-specifications
           #:parse-directive-suffix
           #:pop-argument
           #:pop-argument-form
           #:pop-remaining-arguments
           #:pop-remaining-arguments-form
           #:remaining-argument-count
           #:remaining-argument-count-form
           #:specialize-directive
           #:standard-client
           #:write-cardinal-numeral
           #:write-old-roman-numeral
           #:write-ordinal-numeral
           #:write-radix-numeral
           #:write-roman-numeral))
