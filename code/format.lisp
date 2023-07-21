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
        do (interpret-format-directive client item)))

;;; Runtime environment

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

(defun compute-parameter-value (directive parameter-spec)
  (let* ((parameter-name (car parameter-spec))
         (compile-time-value (funcall parameter-name directive)))
    (cond ((null compile-time-value)
           ;; The parameter was not given at all in the format control
           ;; string, neither as a constant value, nor as a value to
           ;; be acquired at runtime (# or V).  We must use a default
           ;; value if it has any.
           (getf (cdr parameter-spec) :default-value))
          ((eq compile-time-value :argument-reference)
           ;; The parameter was given the explicit value V in the
           ;; format control string, meaning we use the next argument
           ;; to acquire the value of the parameter.  We must test
           ;; that there are more arguments, consume the next one, and
           ;; check that the type of the argument acquired is correct.
           (or (consume-next-argument `(or null
                                           ,(getf (cdr parameter-spec) :type)))
               (getf (cdr parameter-spec) :default-value)))
          ((eq compile-time-value :remaining-argument-count)
           ;; The parameter was given the explicit value # in the
           ;; format control string, meaning we use the number of
           ;; remaining arguments as the value of the parameter.
           (unless (typep *remaining-argument-count*
                          (getf (cdr parameter-spec) :type))
             (error 'argument-type-error
                    :expected-type (getf (cdr parameter-spec) :type)
                    :datum *remaining-argument-count*))
           *remaining-argument-count*)
          (t
           ;; The parameter was given an explicit value (number or
           ;; character) in the format control string, and this is the
           ;; value we want.
           compile-time-value))))

;;; The directive interpreter.

(defmethod interpret-format-directive (client directive)
  (declare (ignore client))
  (error 'unknown-format-directive
         :control-string (control-string directive)
         :tilde-position (start directive)
         :index (1- (end directive))))

(defmacro define-format-directive-interpreter (class-name &body body)
  `(defmethod interpret-format-directive (client (directive ,class-name))
     (declare (ignorable client))
     (with-accessors ((control-string control-string)
                      (start start)
                      (end end)
                      (colonp colonp)
                      (at-signp at-signp))
       directive
       (let ,(loop for parameter-spec in (parameter-specs class-name)
                   collect `(,(car parameter-spec)
                              (compute-parameter-value directive ',parameter-spec)))
         ,@body))))

(defun consume-next-argument (type)
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

(defun consume-remaining-arguments ()
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
              (return (consume-next-argument t)))
            (consume-next-argument t)
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

(defmacro define-format-directive-compiler (class-name &body body)
  `(defmethod compile-format-directive (client (directive ,class-name))
     (declare (ignorable client))
     (with-accessors ((control-string control-string)
                      (start start)
                      (end end)
                      (colonp colonp)
                      (at-signp at-signp)
                      (given-parameters given-parameters)
                      ,@(loop for parameter-spec in (parameter-specs class-name)
                              collect `(,(car parameter-spec) ,(car parameter-spec))))
       directive
       ,@body)))

(defun compile-time-value (directive slot-name)
  (or (slot-value directive slot-name)
      (getf (cdr (find slot-name
                       (parameter-specs (class-name (class-of directive)))
                       :key #'car))
            :default-value)))

;;; The reason we define this function is that the ~? directive
;;; (recursive processing), when a @ modifier is used, reuses
;;; the arguments of the parent control string, so we need
;;; to call a version of format that doesn't initialize the
;;; *arguments* runtime environment variable.
(defun format-with-runtime-arguments (client control-string)
  (catch *inner-tag*
    (interpret-items client
                     (structure-items (split-control-string control-string) nil))))

(defun format (client destination control &rest args)
  (let ((*destination* (cond ((or (streamp destination)
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

(defmethod interpret-format-directive (client (item string))
  (if *newline-kind*
      (loop with start = 0
            with in-blank-p = nil
            for char across item
            for index from 0
            for blankp = (and (find char #(#\Space #\Tab #\Page #\Return)) t)
            finally (write-string (subseq item start) *destination*)
                    (when in-blank-p
                      (inravina:pprint-newline client *destination* *newline-kind*))
            when (and in-blank-p (not blankp))
              do (write-string (subseq item start index) *destination*)
                 (inravina:pprint-newline client *destination* *newline-kind*)
                 (setf start index)
            do (setf in-blank-p blankp))
      (write-string item *destination*)))

(defmethod compile-format-directive (client (item string))
  (if *newline-kind*
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
