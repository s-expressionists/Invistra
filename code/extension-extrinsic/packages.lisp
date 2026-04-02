(defpackage #:invistra-extension-extrinsic
  (:use #:common-lisp)
  (:shadow #:break
           #:cerror
           #:error
           #:format
           #:formatter
           #:invalid-method-error
           #:method-combination-error)
  (:export #:*client*
           #:break
           #:cerror
           #:client
           #:error
           #:format
           #:formatter
           #:invalid-method-error
           #:method-combination-error))
