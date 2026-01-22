(in-package #:invistra-extrinsic)

(defclass extrinsic-client (inravina-extrinsic:extrinsic-client invistra:standard-client)
  ())

(defclass extrinsic-client-impl
    (extrinsic-client quaviver/schubfach:client)
  ())

(defmethod make-load-form ((object extrinsic-client-impl) &optional environment)
  (declare (ignore environment))
  '(make-instance 'extrinsic-client-impl))

(change-class incless-extrinsic:*client* 'extrinsic-client-impl)

(invistra:define-interface :client-form incless-extrinsic:*client*
                           :client-class extrinsic-client)
