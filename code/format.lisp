(in-package #:invistra)

(defun format (destination control-string &rest args)
  (let (;; initialize part of the runtime environment here
        (*arguments* (coerce args 'vector)))
    (format-with-runtime-arguments destination control-string)))
