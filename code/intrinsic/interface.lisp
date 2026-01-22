(in-package #:invistra-intrinsic)

(defclass intrinsic-client (inravina-intrinsic:intrinsic-client invista:standard-client)
  ())

(defclass intrinsic-client-impl
    (intrinsic-client quaviver/schubfach:client)
  ())

(defmethod make-load-form ((object intrinsic-client-impl) &optional environment)
  (declare (ignore environment))
  '(make-instance 'intrinsic-client-impl))

(change-class incless-intrinsic:*client* 'intrinsic-client-impl)

(invistra:define-interface :client-form incless-intrinsic:*client*
                           :client-class intrinsic-client
                           :intrinsic t)
