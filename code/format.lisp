;;; A portable implementation of the Common Lisp FORMAT function.
;;;
;;; Status:
;;;
;;;   Not all directives are implemented.
;;;
;;; TODO:
;;;
;;;   * Implement the last couple of directives.
;;;
;;;   * Implement the directive compiler.
;;;
;;;   * Improve some of the condition reports.
;;;
;;;   * We might want to use reader conditionals to determine how
;;;     to handle things like counting colums (for the TAB directive),
;;;     because it could be costly (and/or imprecise) to count each
;;;     character output.
;;;
;;;
;;; To think about:
;;;
;;;   * Should we use ASSERT as opposed to ERROR to get correctable
;;;     errors?
;;;
;;;   * Should we put in restarts?
;;;
;;;   * What do we do about the possibility that the syntax categories
;;;     of some characters might be altered (for ignored newline
;;;     directive)?

(cl:in-package #:invistra)

;;; During runtime, this variable is bound to a stream to which
;;; all the output goes.
(defvar *destination*)

(defun interpret-items (client items)
  (loop for item across items
        do (interpret-item client item)))

;;; Runtime environment

(defparameter *extra-space* nil)

(defparameter *line-length* nil)

(defparameter *newline-kind* nil)

(defvar *more-arguments-p-hook* nil)

(defvar *argument-index-hook* nil)

(defvar *remaining-argument-count-hook* nil)

(defvar *pop-argument-hook* nil)

(defvar *go-to-argument-hook* nil)

(defvar *pop-remaining-arguments-hook* nil)

(defvar *inner-exit-if-exhausted* nil)

(defvar *outer-exit-if-exhausted* nil)

(defvar *inner-exit* nil)

(defvar *outer-exit* nil)

(defmethod make-argument-cursor ((client standard-client) object)
  (error 'type-error :datum object :expected-type 'list))

(defmethod make-argument-cursor ((client standard-client) (object null))
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
                     :what-argument index
                     :max-arguments 0)))))

(defmethod make-argument-cursor ((client standard-client) (object cons))
  (let ((head object)
        (position 0)
        (len (list-length object)))
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
                      position (length object))))
            (lambda (index &optional absolutep)
              (cond (absolutep
                     (setf position index)
                     (when (minusp position)
                       (error 'go-to-out-of-bounds
                              :what-argument position
                              :max-arguments (length object)))
                     (setf head (nthcdr position object)))
                    ((minusp index)
                     (setf position (+ position index))
                     (when (minusp position)
                       (error 'go-to-out-of-bounds
                              :what-argument position
                              :max-arguments (length object)))
                     (setf head (nthcdr position object)))
                    (t
                     (setf position (+ position index)
                           head (nthcdr index head))))))))

(defmacro with-arguments ((client arguments) &body body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (multiple-value-bind (more-arguments-p-hook *argument-index-hook* *remaining-argument-count-hook*
                             *pop-argument-hook* *pop-remaining-arguments-hook*
                             *go-to-argument-hook*)
           (make-argument-cursor ,client ,arguments)
         (let* ((*more-arguments-p-hook* more-arguments-p-hook)
                (*outer-exit-if-exhausted* *inner-exit-if-exhausted*)
                (*outer-exit* *inner-exit*)
                (*inner-exit-if-exhausted* (lambda ()
                                             (unless (funcall more-arguments-p-hook)
                                               (return-from ,block-name nil))))
                (*inner-exit* (lambda ()
                                (return-from ,block-name nil))))
           ,@body)))))

(defmacro with-remaining-arguments (&body body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (let* ((more-arguments-p-hook *more-arguments-p-hook*)
              (*outer-exit-if-exhausted* *inner-exit-if-exhausted*)
              (*outer-exit* *inner-exit*)
              (*inner-exit-if-exhausted* (lambda ()
                                           (unless (funcall more-arguments-p-hook)
                                             (return-from ,block-name nil))))
              (*inner-exit* (lambda ()
                              (return-from ,block-name nil))))
         ,@body))))

(defmacro with-dynamic-arguments ((&key outer) &body body)
  `(let ((*argument-index-hook* nil)
         (*pop-argument-hook* nil)
         (*pop-remaining-arguments-hook* nil)
         (*go-to-argument-hook* nil)
         (*remaining-argument-count-hook* nil)
         (*outer-exit-if-exhausted* ,(when outer '*inner-exit-if-exhausted*))
         (*outer-exit* ,(when outer '*inner-exit*))
         (*inner-exit-if-exhausted* nil)
         (*inner-exit* nil))
     ,@body))

;;; The directive interpreter.

(defun pop-argument (&optional (type t))
  (funcall *pop-argument-hook* type))

(defun pop-argument-form (&optional (type t))
  (if *pop-argument-hook*
      (funcall *pop-argument-hook* type)
      `(funcall *pop-argument-hook* ',type)))

(defun pop-remaining-arguments ()
  (funcall *pop-remaining-arguments-hook*))

(defun pop-remaining-arguments-form ()
  (if *pop-remaining-arguments-hook*
      (funcall *pop-remaining-arguments-hook*)
      `(funcall *pop-remaining-arguments-hook*)))

(defun go-to-argument (index &optional absolute)
  (funcall *go-to-argument-hook* index absolute))

(defun go-to-argument-forms (index &optional absolute)
  (cond (*go-to-argument-hook*
         (funcall *go-to-argument-hook* index absolute)
         nil)
        (t
         `((funcall *go-to-argument-hook* ,index ,absolute)))))

(defun remaining-argument-count ()
  (funcall *remaining-argument-count-hook*))

(defun remaining-argument-count-form ()
  (if *remaining-argument-count-hook*
      (funcall *remaining-argument-count-hook*)
      `(funcall *remaining-argument-count-hook*)))

(defun inner-exit-forms ()
  (if *inner-exit*
      (funcall *inner-exit*)
      `((funcall *inner-exit*))))

(defun outer-exit-forms ()
  (if *outer-exit*
      (funcall *outer-exit*)
      `((funcall *outer-exit*))))

(defun inner-exit-if-exhausted-forms ()
  (if *inner-exit-if-exhausted*
      (funcall *inner-exit-if-exhausted*)
      `((funcall *inner-exit-if-exhausted*))))

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

;;; The reason we define this function is that the ~? directive
;;; (recursive processing), when a @ modifier is used, reuses
;;; the arguments of the parent control string, so we need
;;; to call a version of format that doesn't initialize the
;;; *arguments* runtime environment variable.
(defun format-with-runtime-arguments (client control-string)
  (interpret-items client (parse-control-string client control-string)))

(defun format (client destination control &rest args)
  (let ((*destination* (cond ((or (streamp destination)
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
    (if (functionp control)
        (apply control *destination* args)
        (with-arguments (client args)
          (format-with-runtime-arguments client control)))
    (if (null destination)
        (get-output-stream-string *destination*)
        nil)))

(defmethod interpret-item (client (item string) &optional parameters)
  (declare (ignore parameters))
  (if *newline-kind*
      (loop with start = 0
            with in-blank-p = nil
            for char across item
            for index from 0
            for blankp = (and (find char #(#\Space #\Tab #\Page #\Return)) t)
            finally (write-string (subseq item start) *destination*)
                    #-sicl (when in-blank-p
                             (inravina:pprint-newline client *destination* *newline-kind*))
            when (and in-blank-p (not blankp))
              do (write-string (subseq item start index) *destination*)
                 #-sicl (inravina:pprint-newline client *destination* *newline-kind*)
                 (setf start index)
            do (setf in-blank-p blankp))
      (write-string item *destination*)))

(defmethod compile-item (client (item string) &optional parameters)
  (declare (ignore parameters))
  (if *newline-kind*
      #+sicl nil #-sicl
      (loop with start = 0
            with in-blank-p = nil
            with pprint-newline = `(inravina:pprint-newline ,(trinsic:client-form client) *destination* ,*newline-kind*)
            for char across item
            for index from 0
            for blankp = (and (find char #(#\Space #\Tab #\Page #\Return)) t)
            finally (return (nconc forms
                                   `((write-string ,(subseq item start) *destination*))
                                   (when in-blank-p
                                     (list pprint-newline))))
            when (and in-blank-p (not blankp))
              collect `(write-string ,(subseq item start index) *destination*) into forms and
              collect pprint-newline into forms and
              do (setf start index)
            do (setf in-blank-p blankp))
      `((write-string ,item *destination*))))

(defmethod interpret-item :around (client (item directive) &optional parameters)
  (declare (ignore parameters))
  (call-next-method client item
                    (mapcar #'interpret-parameter (parameters item))))

(defmethod compile-item :around (client (item directive) &optional parameters)
  (declare (ignore parameters))
  (loop for parameter in (parameters item)
        for compiled-parameter = (compile-parameter parameter)
        for name = (parameter-name parameter)
        finally (return (if bindings
                            `((let* ,bindings
                                (declare (ignorable ,@(mapcar #'first bindings)))
                                ,@(call-next-method client item forms)))
                            (call-next-method client item forms)))
        when (or (not (parameter-bind-p parameter))
                 (constantp compiled-parameter))
          collect compiled-parameter into forms
        else
          collect name into forms
          and collect `(,name ,compiled-parameter) into bindings
        end))
