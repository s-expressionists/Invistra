(in-package #:invistra-extrinsic)

(defparameter *client* inravina-extrinsic:*client*)

(defun format (destination control-string &rest args)
  (apply #'invistra:format *client* destination control-string args))

(defun formatter (control-string)
  (invistra:formatter '*client* control-string))

(define-compiler-macro format (&whole form destination control-string &rest args)
  (invistra:format-compiler-macro '*client* form destination control-string args))
