;;;; 22.3.1 Basic output

(in-package #:invistra)

;;; 22.3.1.1 ~c Character

(defclass character-directive (directive)
  ())

(defmethod specialize-directive
    ((client client) (char (eql #\C)) directive end-directive)
  (declare (ignore end-directive))
  (change-class directive 'character-directive))

(defmethod calculate-argument-position (position (item character-directive))
  (setf position (call-next-method))
  (when position
    (1+ position)))

(defun format-char (client colon-p at-sign-p char)
  (cond (colon-p
         ;; We have a colon modifier.  The HyperSpec says to do what WRITE-CHAR does for
         ;; printing characters, and what char-name does otherwise.  The definition of "printing
         ;; char" is a graphic character other than space. We use a generic function to do this
         ;; test leaving open the possibility that the client interprets any space like
         ;; character as non-printing.
         (if (printing-char-p client char)
             (write-char char *format-output*)
             (write-string (char-name char) *format-output*))
         (when at-sign-p
           ;; Allow client specific key sequence for at sign modifier.
           (print-key-sequence client char *format-output*)))
        (at-sign-p
         ;; We have only an at-sign modifier.  The HyperSpec says to print it the way the Lisp
         ;; reader can understand, which I take to mean "use PRIN1".  It also says to bind
         ;; *PRINT-ESCAPE* to t.
         (let ((*print-escape* t))
           (incless:write-object client char *format-output*)))
        (t
         ;; Neither colon nor at-sign.  The HyperSpec says to do what WRITE-CHAR does.
         (write-char char *format-output*))))

(defmethod interpret-item
    ((client client) (directive character-directive) &optional parameters)
  (declare (ignore parameters))
  (format-char client (colon-p directive) (at-sign-p directive) (pop-argument 'character)))

(defmethod compile-item
    ((client client) (directive character-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((at-sign-p at-sign-p)
                   (colon-p colon-p))
      directive
    (let ((arg-form (pop-argument-form 'character)))
      (cond (colon-p
             (with-unique-names (char)
               `((let ((,char ,arg-form))
                   (if (printing-char-p ,(trinsic:client-form client) ,char)
                       (write-char ,char *format-output*)
                       (write-string (char-name ,char) *format-output*))
                   ,@(when at-sign-p
                       `((print-key-sequence ,(trinsic:client-form client) ,char
                                             *format-output*)))))))
            (at-sign-p
             `((let ((*print-escape* t))
                 (incless:write-object ,(trinsic:client-form client)
                                       ,arg-form *format-output*))))
            (t
             `((write-char ,arg-form *format-output*)))))))

;;; 22.3.1.2 ~% Newline.

(defclass newline-directive (directive no-modifiers-mixin)
  ())

(defmethod specialize-directive
    ((client client) (char (eql #\%)) directive end-directive)
  (declare (ignore end-directive))
  (change-class directive 'newline-directive))

(defmethod parameter-specifications ((client client) (directive newline-directive))
  '((:name n
     :type (integer 0)
     :bind nil
     :default 1)))

(defmethod interpret-item
    ((client client) (directive newline-directive) &optional parameters)
  (loop repeat (car parameters)
        do (terpri *format-output*)))

(defmethod compile-item
    ((client client) (directive newline-directive) &optional parameters)
  (let ((n (car parameters)))
    (case n
      (0 '())
      (1 '((terpri *format-output*)))
      (2 '((terpri *format-output*)
           (terpri *format-output*)))
      (otherwise
       `((loop repeat ,n
               do (terpri *format-output*)))))))

;;; 22.3.1.3 ~& Fresh line and newlines.

(defclass fresh-line-directive (directive no-modifiers-mixin)
  ())

(defmethod specialize-directive
    ((client client) (char (eql #\&)) directive end-directive)
  (declare (ignore end-directive))
  (change-class directive 'fresh-line-directive))

(defmethod parameter-specifications
    ((client client) (directive fresh-line-directive))
  '((:name n
     :bind nil
     :type (integer 0)
     :default 1)))

(defmethod interpret-item
    ((client client) (item fresh-line-directive) &optional parameters)
  (let ((how-many (car parameters)))
    (unless (zerop how-many)
      (fresh-line *format-output*)
      (loop repeat (1- how-many)
            do (terpri *format-output*)))))

(defmethod compile-item
    ((client client) (item fresh-line-directive) &optional parameters)
  (let ((n (car parameters)))
    (case n
      (0 nil)
      (1 `((fresh-line *format-output*)))
      (2 `((fresh-line *format-output*)
           (terpri *format-output*)))
      (otherwise
       (if (numberp n)
           `((fresh-line *format-output*)
             (loop repeat ,(1- n)
                   do (terpri *format-output*)))
           (with-unique-names (i)
             `((loop for ,i below ,n
                     when (zerop ,i)
                       do (fresh-line *format-output*)
                     else
                       do (terpri *format-output*)))))))))

;;; 22.3.1.4 ~| Page separators.

(defclass page-directive (directive no-modifiers-mixin)
  ())

(defmethod specialize-directive
    ((client client) (char (eql #\|)) directive end-directive)
  (declare (ignore end-directive))
  (change-class directive 'page-directive))

(defmethod parameter-specifications
    ((client client) (directive page-directive))
  '((:name n
     :type (integer 0)
     :bind nil
     :default 1)))

(defmethod interpret-item
    ((client client) (directive page-directive) &optional parameters)
  (loop repeat (car parameters)
        do (write-char #\Page *format-output*)))

(defmethod compile-item
    ((client client) (directive page-directive) &optional parameters)
  (let ((n (car parameters)))
    (case n
      (0 nil)
      (1 `((write-char #\Page *format-output*)))
      (2 `((write-char #\Page *format-output*)
           (write-char #\Page *format-output*)))
      (otherwise
       `((loop repeat ,n
               do (write-char #\Page *format-output*)))))))

;;; 22.3.1.5 ~~ Tildes.

(defclass tilde-directive (directive no-modifiers-mixin)
  ())

(defmethod specialize-directive
    ((client client) (char (eql #\~)) directive end-directive)
  (declare (ignore end-directive))
  (change-class directive 'tilde-directive))

(defmethod parameter-specifications ((client client) (directive tilde-directive))
  '((:name n
     :type (integer 0)
     :bind nil
     :default 1)))

(defmethod interpret-item
    ((client client) (directive tilde-directive) &optional parameters)
  (loop repeat (car parameters)
        do (write-char #\~ *format-output*)))

(defmethod compile-item
    ((client client) (directive tilde-directive) &optional parameters)
  (let ((n (car parameters)))
    (case n
      (0 nil)
      (1 `((write-char #\~ *format-output*)))
      (2 `((write-char #\~ *format-output*)
           (write-char #\~ *format-output*)))
      (otherwise
       `((loop repeat ,n
               do (write-char #\~ *format-output*)))))))
