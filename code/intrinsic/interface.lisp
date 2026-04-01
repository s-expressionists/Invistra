(in-package #:invistra-intrinsic)

(defclass client
    (#-sicl inravina-intrinsic:client #+sicl incless-intrinsic:client invistra:client)
  ())

(defclass client-impl (client quaviver/schubfach:client) ())

(setf incless-intrinsic:*client* (make-instance 'client-impl))

(invistra:define-interface :client-form incless-intrinsic:*client*
                           :client-class client
                           :intrinsic t)
