(in-package #:invistra-intrinsic)

(defclass intrinsic-client (#+sicl incless-intrinsic:intrinsic-client
                            #-sicl inravina-intrinsic:intrinsic-client
                            invistra:standard-client)
  ())

(defclass intrinsic-client-impl
    (intrinsic-client quaviver/schubfach:client)
  ())

(invistra:define-interface (incless-intrinsic:*client* intrinsic-client t))

(setf incless-intrinsic:*client* (make-instance 'intrinsic-client-impl))
