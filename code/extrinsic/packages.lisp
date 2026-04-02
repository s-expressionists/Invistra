(defpackage #:invistra-extrinsic
  (:use #:common-lisp)
  (:shadow #:cerror
           #:error
           #:format
           #:formatter)
  (:export #:*client*
           #:cerror
           #:client
           #:error
           #:format
           #:formatter))
