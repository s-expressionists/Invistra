(defpackage #:invistra
  (:use #:cl)
  (:shadow #:format
           #:formatter)
  #+sicl
  (:local-nicknames (:trivial-gray-streams :cyclosis))
  (:export #:*format-output*
           #:*roman-digits*
           #:at-sign-p
           #:check-directive-syntax
           #:coerce-function-designator
           #:colon-p
           #:compile-item
           #:pop-argument
           #:define-interface
           #:directive
           #:format
           #:format-compiler-macro
           #:formatter
           #:interpret-item
           #:parameter-specifications
           #:parse-directive-suffix
           #:specialize-directive
           #:standard-client
           #:write-radix-numeral
           #:write-roman-numeral
           #:write-old-roman-numeral
           #:write-cardinal-numeral
           #:write-ordinal-numeral))
