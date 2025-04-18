(in-package #:invistra-extrinsic)

(defclass extrinsic-client (#+sicl incless-extrinsic:extrinsic-client
                            #-sicl inravina-extrinsic:extrinsic-client
                            invistra:standard-client)
  ())

(defclass extrinsic-client-impl
    (extrinsic-client quaviver/schubfach:client)
  ())

(invistra:define-interface (incless-extrinsic:*client* extrinsic-client))

(initialize-invistra)

(setf incless-extrinsic:*client* (make-instance 'extrinsic-client-impl))
