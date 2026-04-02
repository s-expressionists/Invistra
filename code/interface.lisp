(in-package #:invistra)

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defmethod trinsic:features-list nconc ((client client))
  (list :format/invistra))

(trinsic:make-define-interface (:client-form client-form)
    ((format-sym cl:format)
     (formatter-sym cl:formatter)
     (error-sym cl:error)
     (cerror-sym cl:cerror))
  `((defun ,format-sym (destination control-string &rest args)
      (apply (function format-with-client) ,client-form destination control-string args))

    (define-compiler-macro ,format-sym (&whole form destination control-string &rest args)
      (expand-format ,client-form form destination control-string args))

    (defmacro ,formatter-sym (control-string)
      (expand-formatter ,client-form control-string))

    (define-compiler-macro ,error-sym (&whole form datum &rest args)
      (expand-error ,client-form form datum args))

    (define-compiler-macro ,cerror-sym (&whole form continue-format-control datum &rest args)
      (expand-cerror ,client-form form continue-format-control datum args))))
