(in-package #:invistra-extrinsic)

(defclass extrinsic-client (inravina-extrinsic:extrinsic-client invistra:standard-client)
  ())

(defclass extrinsic-client-impl
    (extrinsic-client quaviver/schubfach:client)
  ())

(change-class incless-extrinsic:*client* 'extrinsic-client-impl)

(invistra:define-interface :client-form incless-extrinsic:*client*
                           :client-class extrinsic-client)
