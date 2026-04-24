(in-package #:invistra-extension-extrinsic)

(defclass client (inravina-extension-extrinsic:client invistra-extension:client) ())

(defclass client-impl (client quaviver/schubfach:client quaviver/liebler:client) ())

(change-class incless-extension-extrinsic:*client* 'client-impl)

(invistra:define-interface :client-form incless-extension-extrinsic:*client*
                           :client-class client)
