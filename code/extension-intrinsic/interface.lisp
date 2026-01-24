(in-package #:invistra-extension-intrinsic)

(defclass intrinsic-client
    (inravina-extension-intrinsic:intrinsic-client invista:standard-client)
  ())

(defclass intrinsic-client-impl
    (intrinsic-client quaviver/schubfach:client)
  ())

(change-class incless-extension-intrinsic:*client* 'intrinsic-client-impl)

(invistra:define-interface :client-form incless-intrinsic:*client*
                           :client-class intrinsic-client
                           :intrinsic t)
