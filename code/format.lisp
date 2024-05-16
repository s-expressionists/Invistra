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

(defvar *previous-arguments*)

(defvar *previous-argument-index*)

(defvar *remaining-argument-count*)

(defvar *pop-argument-hook*)

(defvar *pop-remaining-arguments*)

(defvar *inner-exit-if-exhausted* nil)

(defvar *outer-exit-if-exhausted* nil)

(defvar *inner-exit* nil)

(defvar *outer-exit* nil)

(defvar *inner-tag* nil)

(defvar *outer-tag* nil)

(defmacro with-arguments (arguments &body body)
  (let ((block-name (gensym))
        (arguments-var (gensym)))
    `(catch ',block-name
       (let* ((,arguments-var ,arguments)
              (*previous-argument-index* 0)
              (*remaining-argument-count* (length ,arguments-var))
              (*previous-arguments* (make-array *remaining-argument-count*
                                                :adjustable t :fill-pointer 0))
              (*pop-argument-hook* (lambda ()
                                     (pop ,arguments-var)))
              (*pop-remaining-arguments* (lambda ()
                                           (prog1 ,arguments-var
                                             (setf ,arguments-var nil))))
              (*outer-exit-if-exhausted* *inner-exit-if-exhausted*)
              (*outer-exit* *inner-exit*)
              (*outer-tag* *inner-tag*)
              (*inner-exit-if-exhausted* (lambda (&optional ret)
                                           (unless (or ,arguments-var
                                                       (< *previous-argument-index*
                                                          (length *previous-arguments*)))
                                             (throw ',block-name ret))))
              (*inner-exit* (lambda (&optional ret)
                              (throw ',block-name ret)))
              (*inner-tag* ',block-name))
         ,@body))))

;;; The directive interpreter.

(defun pop-argument (&optional (type t))
  (unless (< *previous-argument-index* (length *previous-arguments*))
    (let (exited)
      (unwind-protect
           (progn
             (funcall *inner-exit-if-exhausted*)
             (setf exited t))
        (unless exited
          (error 'no-more-arguments)))
      (vector-push-extend (funcall *pop-argument-hook*) *previous-arguments*)))
  (when (= *previous-argument-index* (length *previous-arguments*))
    (error 'no-more-arguments))
  (let ((arg (aref *previous-arguments* *previous-argument-index*)))
    (incf *previous-argument-index*)
    (decf *remaining-argument-count*)
    (unless (typep arg type)
      (error 'argument-type-error
             :expected-type type
             :datum arg))
    arg))

(defun pop-remaining-arguments ()
  (let* ((tail (funcall *pop-remaining-arguments*))
         (tail-len (length tail)))
    (adjust-array *previous-arguments* (+ (length *previous-arguments*) tail-len))
    (replace *previous-arguments* tail :start1 (length *previous-arguments*))
    (setf tail
          (concatenate 'list
                       (subseq *previous-arguments* *previous-argument-index*)
                       tail))
    (setf *previous-argument-index* (length *previous-arguments*))
    tail))

(defun go-to-argument (index &optional absolute)
  (when absolute
    (incf *remaining-argument-count* *previous-argument-index*)
    (setf *previous-argument-index* 0))
  (cond ((zerop index)
         (aref *previous-arguments* *previous-argument-index*))
        ((plusp index)
         (prog ()
          next
            (decf index)
            (when (zerop index)
              (return (pop-argument)))
            (pop-argument)
            (go next)))
        (t
         (let ((new-arg-index (+ *previous-argument-index* index)))
           (when (minusp new-arg-index)
             (error 'go-to-out-of-bounds
                    :what-argument new-arg-index
                    :max-arguments *remaining-argument-count*))
           (decf *remaining-argument-count* index)
           (setf *previous-argument-index* new-arg-index)
           (aref *previous-arguments* *previous-argument-index*)))))

(defmethod interpret-parameter ((parameter argument-reference-parameter))
  (or (pop-argument `(or null ,(parameter-type parameter)))
      (parameter-default parameter)))

(defmethod compile-parameter ((parameter argument-reference-parameter))
  `(or (pop-argument '(or null ,(parameter-type parameter)))
       ,(parameter-default parameter)))

(defmethod interpret-parameter ((parameter remaining-argument-count-parameter))
  (if (typep *remaining-argument-count*
             (parameter-type parameter))
      *remaining-argument-count*
      (error 'argument-type-error
             :expected-type (parameter-type parameter)
             :datum *remaining-argument-count*)))

(defmethod compile-parameter ((parameter remaining-argument-count-parameter))
  `(if (typep *remaining-argument-count*
              ',(parameter-type parameter))
       *remaining-argument-count*
       (error 'argument-type-error
              :expected-type ',(parameter-type parameter)
              :datum *remaining-argument-count*)))

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
  (catch *inner-tag*
    (interpret-items client
                     (structure-items client (split-control-string control-string)))))

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
        (with-arguments args
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
            with pprint-newline = `(inravina:pprint-newline ,(incless:client-form client) *destination* ,*newline-kind*)
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
        when (constantp compiled-parameter)
          collect compiled-parameter into forms
        else
          collect name into forms
          and collect `(,name ,compiled-parameter) into bindings))
