(in-package #:invistra-extrinsic)

(defun format (destination control-string &rest args)
  (apply #'invistra:format incless-extrinsic:*client* destination control-string args))

(defmacro formatter (control-string)
  (invistra:formatter incless-extrinsic:*client* control-string))

(define-compiler-macro format (&whole form destination control-string &rest args)
  (invistra:format-compiler-macro incless-extrinsic:*client* form destination control-string args))
