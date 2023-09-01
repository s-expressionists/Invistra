(defpackage #:invistra
  (:use #:cl)
  (:shadow #:format
           #:formatter)
  #+sicl
  (:local-nicknames (:trivial-gray-streams :cyclosis))
  (:export #:at-sign-p
           #:check-directive-syntax
           #:colon-p
           #:compile-item
           #:directive
           #:format
           #:format-compiler-macro
           #:formatter
           #:interpret-item
           #:parameter-specifications
           #:parse-directive-suffix
           #:specialize-directive))
