(in-package #:invistra-shim)

(defclass shim-client (incless-native:native-client inravina-native:native-client)
  ())

(defparameter *client* (make-instance 'shim-client))

(defmethod incless:client-form ((client shim-client))
  '*client*)

#+sbcl (setf (sb-c::fun-info-transforms (sb-c::fun-info-or-lose 'format)) nil)

(trivial-package-locks:with-unlocked-system-packages
  (defun format (destination control-string &rest args)
    (apply #'invistra:format *client* destination control-string args))

  (defmacro formatter (control-string)
    (invistra:formatter *client* control-string))

  (define-compiler-macro format (&whole form destination control-string &rest args)
    (invistra:format-compiler-macro *client* form destination control-string args)))
