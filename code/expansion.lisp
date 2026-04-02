(cl:in-package #:invistra)

(defstruct lambda-argument
  (name (unique-name '#:arg))
  namep
  (type t))

(defun expand-formatter (client control-string)
  (check-type control-string string)
  (with-unique-names (block rest count)
    (let ((items (parse-control-string client control-string)))
      (if (reduce #'calculate-argument-position items :initial-value 0)
          (let* ((args (make-array 8 :adjustable t :fill-pointer 0))
                 (restp t)
                 (guts (let* ((pos 0)
                              (*outer-exit-if-exhausted* *inner-exit-if-exhausted*)
                              (*outer-exit* *inner-exit*)
                              (*more-arguments-p*
                                (lambda ()
                                  (if (< pos (length args))
                                      (or (lambda-argument-namep (aref args pos)) t)
                                      (let ((arg (make-lambda-argument :namep
                                                                       (unique-name '#:argp))))
                                        (vector-push-extend arg args)
                                        (or (lambda-argument-namep arg) t)))))
                              (*argument-index*
                                (lambda () pos))
                              (*remaining-argument-count*
                                (lambda ()
                                  `(- ,count
                                      ,(loop for arg across args
                                             repeat pos
                                             count (not (lambda-argument-namep arg))))))
                              (*pop-argument*
                                (lambda (&optional (type t))
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
                              (pop-remaining-arguments-hook
                                (lambda ()
                                  (when restp
                                    (setf restp nil)
                                    (if (< pos (length args))
                                        (nconc (list 'list*)
                                               (loop for i from pos
                                                       below (length args)
                                                     collect (lambda-argument-name (aref args i)))
                                               (list rest))
                                      rest))))
                              (*pop-remaining-arguments* pop-remaining-arguments-hook)
                              (*go-to-argument*
                                (lambda (index &optional absolutep)
                                  (setf pos (if absolutep index (+ index pos)))
                                  (when (minusp pos)
                                    (error 'go-to-out-of-bounds
                                           :argument-position pos
                                           :argument-count (length args)))
                                  (loop for i from (length args) below pos
                                        do (vector-push-extend (make-lambda-argument) args))))
                              (*inner-exit-if-exhausted*
                                (lambda ()
                                  (unless (< pos (length args))
                                    (let ((arg (make-lambda-argument :namep
                                                                     (unique-name '#:argp))))
                                      (vector-push-extend arg args)
                                      `((unless ,(lambda-argument-namep arg)
                                          (return-from ,block
                                            ,(funcall pop-remaining-arguments-hook))))))))
                              (*inner-exit* (lambda ()
                                              `((return-from ,block
                                                  ,(pop-remaining-arguments-form))))))
                         (nconc (compile-items client items)
                                (list (pop-remaining-arguments-form)))))
                 (lambda-args (loop with required = t
                                    for arg across args
                                    when (and (lambda-argument-namep arg)
                                              required)
                                      collect '&optional
                                      and do (setf required nil)
                                    when (lambda-argument-namep arg)
                                      collect `(,(lambda-argument-name arg) nil
                                                ,(lambda-argument-namep arg))
                                    else
                                      collect (lambda-argument-name arg)))
                 (declarations (loop for arg across args
                                     for type = (lambda-argument-type arg)
                                     unless (eq type t)
                                       collect `(type ,type ,(lambda-argument-name arg)))))
            `(lambda (*format-output* ,@lambda-args &rest ,rest)
               (declare (ignorable ,@(map 'list #'lambda-argument-name args) ,rest)
                        ,@declarations)
               (let ((,count (+ ,(loop for arg across args
                                       count (not (lambda-argument-namep arg)))
                                (list-length ,rest))))
                 (declare (ignorable ,count))
                 (block ,block
                   ,@guts))))
          `(lambda (*format-output* &rest ,rest)
             (with-arguments (,(trinsic:client-form client) ,rest)
               ,@(compile-items client items)
               (pop-remaining-arguments)))))))

(defun expand-format (client form destination control-string args)
  (declare (ignore form))
  (flet ((funcall-expand (formatter)
           (cond ((null destination)
                  `(with-output-to-string (*format-output*)
                     (funcall ,formatter *format-output* ,@args)))
                 ((eq destination t)
                  `(progn
                     (funcall ,formatter *standard-output* ,@args)
                     nil))
                 (t
                  `(format-with-client ,(trinsic:client-form client) ,destination
                                       ,formatter ,@args)))))
    (cond ((stringp control-string)
           (funcall-expand (expand-formatter client control-string)))
          ((and (consp control-string)
                (eq (car control-string) 'function)
                (cdr control-string)
                (null (cddr control-string)))
           (funcall-expand control-string))
          (t
           `(format-with-client ,(trinsic:client-form client) ,destination
                                ,control-string ,@args)))))

(defun expand-function (client form start &optional end)
  (loop with expanded-form = nil
        for (index format-control) on args by #'cddr
        when (stringp format-control)
          do (unless expanded-form
               (setf expanded-form (copy-list form)))
             (unless (< index (length expanded-form))
               (setf expanded-form (nconc expanded-form (make-list (- index (length expanded-form) -1)))))
             (setf (nth index expanded-form) (expand-formatter client format-control))
        finally (return (or expanded-form form))))


(defun expand-function (client form &rest args)
  (loop with expanded-form = nil
        for (index format-control) on args by #'cddr
        when (stringp format-control)
          do (unless expanded-form
               (setf expanded-form (copy-list form)))
             (unless (< index (length expanded-form))
               (setf expanded-form (nconc expanded-form (make-list (- index (length expanded-form) -1)))))
             (setf (nth index expanded-form) (expand-formatter client format-control))
        finally (return (or expanded-form form))))

#|(defun expand-error (client form datum args)
  (if (stringp datum)
      `(,(car form) ,(expand-formatter client datum) ,@args)
      form))

(defun expand-cerror (client form continue-format-control datum args)
  (let ((expand nil))
    (when (stringp continue-format-control)
      (setf continue-format-control (expand-formatter client continue-format-control)
            expand t))
    (when (stringp datum)
      (setf datum (expand-formatter client datum)
            expand t))
    (if expand
        `(,(car form) ,continue-format-control ,datum ,@args)
        form)))|#
