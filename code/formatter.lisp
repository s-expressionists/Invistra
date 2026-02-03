(cl:in-package #:invistra)

(defun formatter (client control-string)
  (check-type control-string string)
  (with-unique-names (block rest)
    (let* ((args (make-array 8 :adjustable t :fill-pointer 0))
           (guts (let* ((pos 0)
                         (*outer-exit-if-exhausted* *inner-exit-if-exhausted*)
                         (*outer-exit* *inner-exit*)
                         (*more-arguments-p-hook* (lambda () t))
                         (*argument-index-hook* (lambda () pos))
                         (*pop-argument-hook* (lambda (&optional (type t))
                                                (if (< pos (length args))
                                                    (aref args pos)
                                                    (let ((arg (unique-name '#:arg)))
                                                      (vector-push-extend arg args)
                                                      (incf pos)
                                                      arg))))
                         (*pop-remaining-arguments-hook* (lambda ()
                                                           (if (< pos (length args))
                                                               (nconc (list 'list*)
                                                                      (loop for i from pos
                                                                              below (length args)
                                                                            collect (aref args i))
                                                                      (list rest))
                                                               rest)))
                         (*go-to-argument-hook* (lambda (index &optional absolutep)
                                                  (setf pos (if absolutep index (+ index pos)))
                                                  (loop for i from (length args) to pos
                                                        do (vector-push-extend (unique-name '#:arg) args))))
                         (*inner-exit-if-exhausted* (lambda ()))
                         #+(or)(*inner-exit* (lambda () (return nil))))
                    (nconc (compile-items client (parse-control-string client control-string))
                           (list (pop-remaining-arguments-form))))))
      (if (zerop (length args))
          `(lambda (*destination* &rest ,rest) ,@guts)
          `(lambda (*destination*
                    &optional ,@(loop with l = (length args)
                                    for arg across args
                                    for i from 0
                                    collect `(,arg (error 'go-to-out-of-bounds
                                                          :what-argument ,i
                                                          :max-arguments ,l)))
                    &rest ,rest)
             ,@guts)))))

(defun format-compiler-macro (client form destination control-string args)
  (declare (ignore form))
  `(format ,(trinsic:client-form client) ,destination
           ,(if (stringp control-string)
                (formatter client control-string)
                control-string)
           ,@args))
