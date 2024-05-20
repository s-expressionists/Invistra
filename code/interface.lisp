(in-package #:invistra)

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defmacro define-interface ((client-var client-class &optional intrinsic) &body body)
  (declare (ignore client-class))
  (let* ((intrinsic-pkg (if intrinsic (find-package '#:common-lisp) *package*))
         (format-func (ensure-symbol '#:format intrinsic-pkg))
         (initialize-func (ensure-symbol '#:initialize-invistra)))
    `(progn
       (defun ,format-func (destination control-string &rest args)
         (apply #'format ,client-var destination control-string args))

       (defmacro ,(ensure-symbol '#:formatter intrinsic-pkg) (control-string)
         (formatter ,client-var control-string))

       (define-compiler-macro ,format-func (&whole form destination control-string &rest args)
         (format-compiler-macro ,client-var form destination control-string args))

       (defun ,initialize-func ()
         ,@body))))
