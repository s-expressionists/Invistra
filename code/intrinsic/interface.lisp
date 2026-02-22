(in-package #:invistra-intrinsic)

(defclass intrinsic-client (#-sicl inravina-intrinsic:intrinsic-client
                            #+sicl incless-intrinsic:intrinsic-client
                            invista:standard-client)
  ())

(defclass intrinsic-client-impl
    (intrinsic-client quaviver/schubfach:client)
  ())

(change-class incless-intrinsic:*client* 'intrinsic-client-impl)

(invistra:define-interface :client-form incless-intrinsic:*client*
                           :client-class intrinsic-client
                           :intrinsic t)
