(cl:in-package #:invistra)

(defun interpret-items (client items)
  (loop for item across items
        do (interpret-item client item)))

(defun compile-items (client items)
  (loop for item across items
        append (compile-item client item)))

;;; Runtime environment

(defparameter *extra-space* nil)

(defparameter *line-length* nil)

(defparameter *newline-kind* nil)

(defvar *more-arguments-p* nil)

(defvar *argument-index* nil)

(defvar *remaining-argument-count* nil)

(defvar *pop-argument* nil)

(defvar *go-to-argument* nil)

(defvar *pop-remaining-arguments* nil)

(defvar *inner-exit-if-exhausted* nil)

(defvar *outer-exit-if-exhausted* nil)

(defvar *inner-exit* nil)

(defvar *outer-exit* nil)

(defmethod make-argument-cursor ((client client) object)
  (error 'type-error :datum object :expected-type 'list))

(defmethod make-argument-cursor ((client client) (object null))
  (values (lambda ()
            nil)
          (lambda ()
            0)
          (lambda ()
            0)
          (lambda (&optional type)
            (declare (ignore type))
            (error 'no-more-arguments))
          (lambda ()
            nil)
          (lambda (index &optional absolutep)
            (declare (ignore absolutep))
            (unless (zerop index)
              (error 'go-to-out-of-bounds
                     :argument-position index
                     :argument-count 0)))))

(defmethod make-argument-cursor ((client client) (object cons))
  (let ((head object)
        (position 0)
        (len (dotted-list-length object)))
    (values (lambda ()
              head)
            (lambda ()
              position)
            (lambda ()
              (- len position))
            (lambda (&optional (type t))
              (cond (head
                     (unless (typep (car head) type)
                       (error 'type-error :datum (car head) :expected-type type))
                     (incf position)
                     (pop head))
                    (t
                     (error 'no-more-arguments))))
            (lambda ()
              (prog1
                  head
                (setf head nil
                      position len)))
            (lambda (index &optional absolutep)
              (tagbody
                 (unless absolutep
                   (incf index position))
                 (when (minusp index)
                   (error 'go-to-out-of-bounds
                          :argument-position index
                          :argument-count len))
                 (when (< index position)
                   (setf head object
                         position 0))
               next
                 (when (< position index)
                   (unless head
                     (error 'go-to-out-of-bounds
                            :argument-position index
                            :argument-count len))
                   (incf position)
                   (pop head)
                   (go next)))))))

(defmacro with-arguments ((client arguments &key outer) &body body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (multiple-value-bind (more-arguments-p-hook *argument-index* *remaining-argument-count*
                             *pop-argument* *pop-remaining-arguments*
                             *go-to-argument*)
           (make-argument-cursor ,client ,arguments)
         (let* ((*more-arguments-p* more-arguments-p-hook)
                (*outer-exit-if-exhausted* ,(when outer '*inner-exit-if-exhausted*))
                (*outer-exit* ,(when outer '*inner-exit*))
                (*inner-exit-if-exhausted* (lambda ()
                                             (unless (funcall more-arguments-p-hook)
                                               (return-from ,block-name nil))))
                (*inner-exit* (lambda ()
                                (return-from ,block-name nil))))
           ,@body)))))

(defmacro with-remaining-arguments ((&key outer) &body body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (let* ((more-arguments-p-hook *more-arguments-p*)
              (*outer-exit-if-exhausted* ,(when outer '*inner-exit-if-exhausted*))
              (*outer-exit* ,(when outer '*inner-exit*))
              (*inner-exit-if-exhausted* (lambda ()
                                           (unless (funcall more-arguments-p-hook)
                                             (return-from ,block-name nil))))
              (*inner-exit* (lambda ()
                              (return-from ,block-name nil))))
         ,@body))))

(defmacro with-remaining-arguments-form ((&key outer) &body body)
  (with-unique-names (body-func)
    (let ((block-name (gensym)))
      `(flet ((,body-func () ,@body))
         (if *more-arguments-p*
             (let* ((more-arguments-p-hook *more-arguments-p*)
                    (*outer-exit-if-exhausted* ,(when outer '*inner-exit-if-exhausted*))
                    (*outer-exit* ,(when outer '*inner-exit*))
                    (*inner-exit-if-exhausted* (lambda ()
                                                 (list (list 'unless (funcall more-arguments-p-hook)
                                                       (list 'return-from ',block-name nil)))))
                    (*inner-exit* (lambda ()
                                    (list (list 'return-from ',block-name nil)))))
               (list (list* 'block ',block-name (,body-func))))
             (list (list* 'with-remaining-arguments
                          (list :outer ,outer)
                          (,body-func))))))))

(defmacro with-dynamic-arguments ((&key outer) &body body)
  `(let ((*argument-index* nil)
         (*pop-argument* nil)
         (*pop-remaining-arguments* nil)
         (*go-to-argument* nil)
         (*remaining-argument-count* nil)
         (*outer-exit-if-exhausted* ,(when outer '*inner-exit-if-exhausted*))
         (*outer-exit* ,(when outer '*inner-exit*))
         (*inner-exit-if-exhausted* nil)
         (*inner-exit* nil))
     ,@body))

;;; The directive interpreter.

(declaim (inline pop-argument pop-remaining-arguments go-to-argument remaining-argument-count
                 inner-exit outer-exit inner-exit-if-exhausted outer-exit-if-exhausted))

(defun pop-argument (&optional (type t))
  (funcall *pop-argument* type))

(defun pop-argument-form (&optional (type t))
  (if *pop-argument*
      (funcall *pop-argument* type)
      `(funcall *pop-argument* ',type)))

(defun pop-remaining-arguments ()
  (funcall *pop-remaining-arguments*))

(defun pop-remaining-arguments-form ()
  (if *pop-remaining-arguments*
      (funcall *pop-remaining-arguments*)
      `(funcall *pop-remaining-arguments*)))

(defun go-to-argument (index &optional absolute)
  (funcall *go-to-argument* index absolute))

(defun go-to-argument-forms (index &optional absolute)
  (cond (*go-to-argument*
         (funcall *go-to-argument* index absolute)
         nil)
        (t
         `((funcall *go-to-argument* ,index ,absolute)))))

(defun remaining-argument-count ()
  (funcall *remaining-argument-count*))

(defun remaining-argument-count-form ()
  (if *remaining-argument-count*
      (funcall *remaining-argument-count*)
      `(funcall *remaining-argument-count*)))

(defun inner-exit ()
  (funcall *inner-exit*))

(defun inner-exit-forms ()
  (if *inner-exit*
      (funcall *inner-exit*)
      `((funcall *inner-exit*))))

(defun outer-exit ()
  (funcall *outer-exit*))

(defun outer-exit-forms ()
  (if *outer-exit*
      (funcall *outer-exit*)
      `((funcall *outer-exit*))))

(defun inner-exit-if-exhausted ()
  (funcall *inner-exit-if-exhausted*))

(defun inner-exit-if-exhausted-forms ()
  (if *inner-exit-if-exhausted*
      (funcall *inner-exit-if-exhausted*)
      `((funcall *inner-exit-if-exhausted*))))

(defun outer-exit-if-exhausted ()
  (funcall *outer-exit-if-exhausted*))

(defun outer-exit-if-exhausted-forms ()
  (if *outer-exit-if-exhausted*
      (funcall *outer-exit-if-exhausted*)
      `((funcall *outer-exit-if-exhausted*))))

(defmethod interpret-parameter ((parameter argument-reference-parameter))
  (or (pop-argument `(or null ,(parameter-type parameter)))
      (parameter-default parameter)))

(defmethod compile-parameter ((parameter argument-reference-parameter))
  (if (parameter-default parameter)
      `(or ,(pop-argument-form `(or null ,(parameter-type parameter)))
           ,(parameter-default parameter))
      (pop-argument-form `(or null ,(parameter-type parameter)))))

(defmethod interpret-parameter ((parameter remaining-argument-count-parameter))
  (let ((value (remaining-argument-count)))
    (if (typep value (parameter-type parameter))
        value
        (error 'argument-type-error
               :expected-type (parameter-type parameter)
               :datum value))))

(defmethod compile-parameter ((parameter remaining-argument-count-parameter))
  `(let ((value ,(remaining-argument-count-form)))
     (if (typep value ',(parameter-type parameter))
         value
         (error 'argument-type-error
                :expected-type ',(parameter-type parameter)
                :datum value))))

(defmethod interpret-parameter ((parameter literal-parameter))
  (parameter-value parameter))

(defmethod compile-parameter ((parameter literal-parameter))
  (parameter-value parameter))

(deftype format-control ()
  `(or string function))

(declaim (ftype (function (t format-control t) t) format-single-recursive)
         (ftype (function (t format-control) t) format-remaining-recursive))

(defun format-single-recursive (client control args)
  (if (stringp control)
      (with-arguments (client args)
        (interpret-items client (parse-control-string client control)))
      (apply control *format-output* args)))

(defun format-remaining-recursive (client control)
  (if (stringp control)
      (with-remaining-arguments ()
        (interpret-items client (parse-control-string client control)))
      (go-to-argument (- (length (apply control
                                        *format-output* (pop-remaining-arguments)))))))

(defun format (client destination control &rest args)
  (let ((*format-output* (cond ((or (streamp destination)
                                  #-sicl (inravina:pretty-stream-p client destination)
                                  (and (stringp destination)
                                       (array-has-fill-pointer-p destination)))
                              destination)
                             ((null destination)
                              (make-string-output-stream))
                             ((eq destination t)
                              *standard-output*)
                             (t
                              (error 'invalid-destination
                                     :destination destination)))))
    (format-single-recursive client control args)
    (if (null destination)
        (get-output-stream-string *format-output*)
        nil)))

(defmethod interpret-item ((client client) (item string) &optional parameters)
  (declare (ignore parameters))
  (if *newline-kind*
      (loop with start = 0
            with in-blank-p = nil
            for char across item
            for index from 0
            for blankp = (and (find char #(#\Space #\Tab #\Page #\Return)) t)
            finally (write-string (subseq item start) *format-output*)
                    #-sicl (when in-blank-p
                             (inravina:pprint-newline client *format-output* *newline-kind*))
            when (and in-blank-p (not blankp))
              do (write-string (subseq item start index) *format-output*)
                 #-sicl (inravina:pprint-newline client *format-output* *newline-kind*)
                 (setf start index)
            do (setf in-blank-p blankp))
      (write-string item *format-output*)))

(defmethod compile-item ((client client) (item string) &optional parameters)
  (declare (ignore parameters))
  (if *newline-kind*
      #+sicl nil #-sicl
      (loop with start = 0
            with in-blank-p = nil
            with pprint-newline = `(inravina:pprint-newline ,(trinsic:client-form client)
                                                            *format-output* ,*newline-kind*)
            for char across item
            for index from 0
            for blankp = (and (find char #(#\Space #\Tab #\Page #\Return)) t)
            finally (return (nconc forms
                                   `((write-string ,(subseq item start) *format-output*))
                                   (when in-blank-p
                                     (list pprint-newline))))
            when (and in-blank-p (not blankp))
              collect `(write-string ,(subseq item start index) *format-output*) into forms and
              collect pprint-newline into forms and
              do (setf start index)
            do (setf in-blank-p blankp))
      `((write-string ,item *format-output*))))

(defmethod interpret-item :around
    ((client client) (item directive) &optional parameters)
  (declare (ignore parameters))
  (call-next-method client item
                    (mapcar #'interpret-parameter (parameters item))))

(defmethod compile-item :around
    ((client client) (item directive) &optional parameters)
  (declare (ignore parameters))
  (loop for parameter in (parameters item)
        for compiled-parameter = (compile-parameter parameter)
        for name = (gensym (symbol-name (parameter-name parameter)))
        finally (return (if bindings
                            `((let* ,bindings
                                (declare (ignorable ,@(mapcar #'first bindings)))
                                ,@(call-next-method client item forms)))
                            (call-next-method client item forms)))
        when (or (not (parameter-bind-p parameter))
                 (constantp compiled-parameter)
                 (symbolp compiled-parameter))
          collect compiled-parameter into forms
        else
          collect name into forms
          and collect `(,name ,compiled-parameter) into bindings
        end))
