(defpackage #:invistra-extrinsic
  (:use #:common-lisp)
  (:shadow #:format
           #:formatter)
  (:export #:*client*
           #:format
           #:formatter))
