(in-package #:invistra-extrinsic)

(defclass client (inravina-extrinsic:client invistra:client) ())

(defclass client-impl (client quaviver/schubfach:client) ())

(change-class incless-extrinsic:*client* 'client-impl)

(invistra:define-interface :client-form incless-extrinsic:*client*
                           :client-class client)
