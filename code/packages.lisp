(defpackage #:invistra
  (:use #:cl)
  (:shadow #:format
           #:formatter)
  #+sicl
  (:local-nicknames (:trivial-gray-streams :cyclosis))
  (:export #:*destination*
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
           #:standard-client))
