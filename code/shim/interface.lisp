(in-package #:invistra-shim)

(defclass shim-client (incless-native:native-client inravina-native:native-client)
  ())

(defparameter *client* (make-instance 'shim-client))

(defmethod incless:client-form ((client shim-client))
  '*client*)

#+sbcl (setf (sb-c::fun-info-transforms (sb-c::fun-info-or-lose 'format)) nil)

(trivial-package-locks:with-unlocked-system-packages
  (invistra:define-interface (*client* shim-client t)))

(initialize-invistra)
