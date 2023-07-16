(defpackage #:invistra
  (:use #:cl)
  (:shadow #:format
           #:formatter)
  #+sicl
  (:local-nicknames (:trivial-gray-streams :cyclosis))
  (:export #:format
           #:formatter
           #:format-compiler-macro))
