(defpackage #:invistra-extension-extrinsic
  (:use #:common-lisp)
  (:shadow #:break
           #:cerror
           #:error
           #:format
           #:formatter
           #:invalid-method-error
           #:method-combination-error
           #:y-or-n-p
           #:yes-or-no-p)
  (:export #:*client*
           #:break
           #:cerror
           #:client
           #:error
           #:format
           #:formatter
           #:invalid-method-error
           #:method-combination-error
           #:y-or-n-p
           #:yes-or-no-p))
