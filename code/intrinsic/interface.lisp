(in-package #:invistra-intrinsic)

(defparameter *client* inravina-intrinsic:*client*)

(defun format (destination control-string &rest args)
  (apply #'invistra:format *client* destination control-string args))

(define-compiler-macro format (&whole form destination control-string &rest args)
  (invistra:format-compiler-macro '*client* form destination control-string args))