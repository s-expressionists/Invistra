(defpackage #:invistra-extrinsic
  (:use #:common-lisp)
  (:shadow #:format
           #:formatter)
  (:export #:*client*
           #:client
           #:format
           #:formatter))
