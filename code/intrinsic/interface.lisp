(in-package #:invistra-intrinsic)

(defun format (destination control-string &rest args)
  (apply #'invistra:format incless-intrinsic:*client* destination control-string args))

(defmacro formatter (control-string)
  (invistra:formatter incless-intrinsic:*client* control-string))

(define-compiler-macro format (&whole form destination control-string &rest args)
  (invistra:format-compiler-macro incless-intrinsic:*client* form destination control-string args))
