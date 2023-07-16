(cl:in-package #:invistra)

(defun formatter (client control-string &key output)
  (check-type control-string string)
  (let ((items (structure-items (split-control-string control-string) nil)))
    `(lambda (destination &rest args)
       (let* ((*destination* (cond ((null destination)
                                    (make-string-output-stream))
                                   ((or (streamp destination)
                                        (and (stringp destination)
                                             (array-has-fill-pointer-p destination)))
                                    destination)
                                   ((eq destination t)
                                    *standard-output*)
                                   (t
                                    (error 'invalid-destination
                                           :destination destination))))
              (*previous-argument-index* 0)
              (*remaining-argument-count* (length args))
              (*previous-arguments* (make-array *remaining-argument-count*
                                                :adjustable t :fill-pointer 0))
              (*arguments* args)
              ;; Any unique object will do.
              (*catch-tag* (list nil))
              (*pop-argument-hook* (lambda ()
                                     (pop *arguments*)))
              (*escape-hook* (lambda ()
                               (unless (or *arguments*
                                           (< *previous-argument-index* (length *previous-arguments*)))
                                 (throw *catch-tag* nil)))))
         (catch *catch-tag*
           ,@(compile-items client items))
         ,(if output
              `(when (null destination)
                 (get-output-stream-string *destination*))
              '*arguments*)))))

(defun format-compiler-macro (client form destination control-string args)
  (if (not (stringp control-string))
      form
      `(funcall ,(formatter client control-string :output t)
                ,destination ,@args)))
