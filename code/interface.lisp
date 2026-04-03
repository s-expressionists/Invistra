(in-package #:invistra)

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defmethod trinsic:features-list nconc ((client client))
  (list :format/invistra))

(trinsic:make-define-interface (:client-form client-form :intrinsic intrinsicp)
    ((break-sym cl:break)
     (cerror-sym cl:cerror)
     (error-sym cl:error)
     (format-sym cl:format)
     (formatter-sym cl:formatter)
     (invalid-method-error-sym cl:invalid-method-error)
     (method-combination-error-sym cl:method-combination-error)
     (y-or-n-p-sym cl:y-or-n-p)
     (yes-or-no-p-sym cl:yes-or-no-p))
  `((defun ,format-sym (destination control-string &rest args)
      (apply (function format-with-client) ,client-form destination control-string args))

    (define-compiler-macro ,format-sym (&whole form destination control-string &rest args)
      (expand-format ,client-form form destination control-string args))

    (defmacro ,formatter-sym (control-string)
      (expand-formatter ,client-form control-string))

    ,@(unless intrinsicp
        `((defun ,break-sym (&optional (format-control "BREAK invoked") &rest args)
            (apply (function cl:break) format-control args))

          (defun ,cerror-sym (continue-format-control datum &rest args)
            (apply (function cl:cerror) continue-format-control datum args))

          (defun ,error-sym (datum &rest args)
            (apply (function cl:error) datum args))

          (defun ,invalid-method-error-sym (method format-control &rest args)
            (apply (function cl:invalid-method-error) method format-control args))

          (defun ,method-combination-error-sym (format-control &rest args)
            (apply (function cl:method-combination-error) format-control args))

          (defun ,y-or-n-p-sym (&rest args)
            (apply (function cl:y-or-n-p) args))

          (defun ,yes-or-no-p-sym (&rest args)
            (apply (function cl:yes-or-no-p) args))))

    (define-compiler-macro ,break-sym (&whole form &optional (format-control "BREAK invoked") &rest args)
      (declare (ignore args))
      (expand-function ,client-form form 1 format-control))

    (define-compiler-macro ,cerror-sym (&whole form continue-format-control datum &rest args)
      (declare (ignore args))
      (expand-function ,client-form form 1 continue-format-control 2 datum))

    (define-compiler-macro ,error-sym (&whole form datum &rest args)
      (declare (ignore args))
      (expand-function ,client-form form 1 datum))

    (define-compiler-macro ,invalid-method-error-sym (&whole form method format-control &rest args)
      (declare (ignore method args))
      (expand-function ,client-form form 2 format-control))

    (define-compiler-macro ,method-combination-error-sym (&whole form format-control &rest args)
      (declare (ignore args))
      (expand-function ,client-form form 1 format-control))

    (define-compiler-macro ,y-or-n-p-sym (&whole form &optional control &rest args)
      (declare (ignore args))
      (expand-function ,client-form form 1 control))

    (define-compiler-macro ,yes-or-no-p-sym (&whole form &optional control &rest args)
      (declare (ignore args))
      (expand-function ,client-form form 1 control))))
