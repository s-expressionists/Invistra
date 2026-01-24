(defpackage #:invistra-extension-extrinsic
  (:use #:common-lisp)
  (:shadow #:format
           #:formatter)
  (:export #:*client*
           #:format
           #:formatter
           #:initialize-invistra))
