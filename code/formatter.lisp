(cl:in-package #:invistra)

(defstruct lambda-argument
  (name (unique-name '#:arg))
  namep
  (type t))

(defun formatter (client control-string)
  (check-type control-string string)
  (with-unique-names (block rest count)
    (let ((items (parse-control-string client control-string)))
      (if (loop for item across items
                thereis (outer-iteration-p item))
          `(lambda (*destination* &rest ,rest)
             (with-arguments (,(trinsic:client-form client) ,rest)
               ,@(compile-items client items)
               (pop-remaining-arguments)))
          (let* ((args (make-array 8 :adjustable t :fill-pointer 0 :element-type 'lambda-argument))
                 (guts (let* ((pos 0)
                              (*outer-exit-if-exhausted* *inner-exit-if-exhausted*)
                              (*outer-exit* *inner-exit*)
                              (*more-arguments-p-hook* (lambda () t))
                              (*argument-index-hook* (lambda () pos))
                              (*remaining-argument-count-hook* (lambda () `(- ,count
                                                                              ,(loop for arg across args
                                                                                     repeat pos
                                                                                     count (not (lambda-argument-namep arg))))))
                              (*pop-argument-hook* (lambda (&optional (type t))
                                                     (if (< pos (length args))
                                                         (let ((arg (aref args pos)))
                                                           (when (subtypep type (lambda-argument-type arg))
                                                             (setf (lambda-argument-type arg) type))
                                                           (incf pos)
                                                           (lambda-argument-name arg))
                                                         (let ((arg (make-lambda-argument :type type)))
                                                           (vector-push-extend arg args)
                                                           (incf pos)
                                                           (lambda-argument-name arg)))))
                              (pop-remaining-arguments-hook (lambda ()
                                                                (if (< pos (length args))
                                                                    (nconc (list 'list*)
                                                                           (loop for i from pos
                                                                                   below (length args)
                                                                                 collect (lambda-argument-name (aref args i)))
                                                                           (list rest))
                                                                    rest)))
                              (*pop-remaining-arguments-hook* pop-remaining-arguments-hook)
                              (*go-to-argument-hook* (lambda (index &optional absolutep)
                                                       (setf pos (if absolutep index (+ index pos)))
                                                       (loop for i from (length args) to pos
                                                             do (vector-push-extend (make-lambda-argument) args))))
                              (*inner-exit-if-exhausted* (lambda ()
                                                           (unless (< pos (length args))
                                                             (let ((arg (make-lambda-argument :namep (unique-name '#:argp))))
                                                               (vector-push-extend arg args)
                                                               `((unless ,(lambda-argument-namep arg)
                                                                   (return-from ,block
                                                                     ,(funcall pop-remaining-arguments-hook))))))))
                              (*inner-exit* (lambda ()
                                              `((return-from ,block
                                                  ,(pop-remaining-arguments-form))))))
                         (nconc (compile-items client items)
                                (list (pop-remaining-arguments-form)))))
                 (lambda-args (unless (zerop (length args))
                                `(&optional
                                  ,@(loop with l = (length args)
                                          for arg across args
                                          for i from 0
                                          when (lambda-argument-namep arg)
                                            collect `(,(lambda-argument-name arg) nil ,(lambda-argument-namep arg))
                                          else
                                            collect `(,(lambda-argument-name arg)
                                                      (error 'go-to-out-of-bounds
                                                             :what-argument ,i
                                                             :max-arguments ,l))))))
                 (declarations (loop for arg across args
                                     for type = (lambda-argument-type arg)
                                     unless (eq type t)
                                       collect `(type ,type ,(lambda-argument-name arg)))))
            `(lambda (*destination*
                      ,@lambda-args
                      &rest ,rest
                      &aux (,count (+ ,(loop for arg across args
                                             count (not (lambda-argument-namep arg)))
                                      (list-length ,rest))))
               (declare (ignorable ,@(map 'list #'lambda-argument-name args) ,count ,rest)
                        ,@declarations)
               (block ,block
                 ,@guts)))))))

(defun format-compiler-macro (client form destination control-string args)
  (declare (ignore form))
  `(format ,(trinsic:client-form client) ,destination
           ,(if (stringp control-string)
                (formatter client control-string)
                control-string)
           ,@args))
