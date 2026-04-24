(in-package #:invistra-extension-intrinsic)

(defclass client (inravina-extension-intrinsic:client invistra:client) ())

(defclass client-impl (client quaviver/schubfach:client quaviver/liebler:client) ())

(change-class incless-extension-intrinsic:*client* 'client-impl)

(invistra:define-interface :client-form incless-intrinsic:*client*
                           :client-class client
                           :intrinsic t)
