(in-package #:invistra-extension-extrinsic)

(defclass extrinsic-client
    (inravina-extension-extrinsic:extrinsic-client invistra-extension:extension-client)
  ())

(defclass extrinsic-client-impl
    (extrinsic-client quaviver/schubfach:client)
  ())

(change-class incless-extension-extrinsic:*client* 'extrinsic-client-impl)

(invistra:define-interface :client-form incless-extension-extrinsic:*client*
                           :client-class extrinsic-client)
