;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1 Basic output

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.1 ~c Character

(defclass c-directive (directive)
  ())

(defmethod specialize-directive
    (client (char (eql #\C)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'c-directive))

(defmethod interpret-item (client (directive c-directive) &optional parameters)
  (declare (ignore parameters))
  (let ((char (consume-next-argument 'character))
        (colonp (colonp directive))
        (at-signp (at-signp directive)))
    (cond ((and (not colonp) (not at-signp))
           ;; Neither colon nor at-sign.
           ;; The HyperSpec says to do what WRITE-CHAR does.
           (write-char char *destination*))
          ((not at-signp)
           ;; We have only a colon modifier.
           ;; The HyperSpec says to do what WRITE-CHAR does for
           ;; printing characters, and what char-name does otherwise.
           ;; The definition of "printing char" is a graphic character
           ;; other than space.
           (if (and (graphic-char-p char) (not (eql char #\Space)))
               (write-char char *destination*)
               (write-string (char-name char) *destination*)))
          ((not colonp)
           ;; We have only an at-sign modifier.
           ;; The HyperSpec says to print it the way the Lisp
           ;; reader can understand, which I take to mean "use PRIN1".
           ;; It also says to bind *PRINT-ESCAPE* to t.
           (let ((*print-escape* t))
             (incless:write-object client char *destination*)))
          (t
           ;; We have both a colon and and at-sign.
           ;; The HyperSpec says to do what ~:C does, but
           ;; also to mention unusual shift keys on the
           ;; keyboard required to type the character.
           ;; I don't see how to do that, so we do the same
           ;; as for ~:C.
           (if (and (graphic-char-p char) (not (eql char #\Space)))
               (write-char char *destination*)
               (write-string (char-name char) *destination*))))))

(defmethod compile-item (client (directive c-directive) &optional parameters)
  (declare (ignore parameters))
  (let ((colonp (colonp directive))
        (at-signp (at-signp directive)))
    `((let ((char (consume-next-argument 'character)))
        ,(cond ((and (not colonp) (not at-signp))
                `(write-char char *destination*))
               ((not at-signp)
                `(if (and (graphic-char-p char) (not (eql char #\Space)))
                     (write-char char *destination*)
                     (write-string (char-name char) *destination*)))
               ((not colonp)
                `(let ((*print-escape* t))
                   (incless:write-object ,(incless:client-form client) char *destination*)))
               (t
                `(if (and (graphic-char-p char) (not (eql char #\Space)))
                     (write-char char *destination*)
                     (write-string (char-name char) *destination*))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.2 ~% Newline.

(defclass percent-directive (directive no-modifiers-mixin)
  ())

(defmethod specialize-directive
    (client (char (eql #\%)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'percent-directive))

(defmethod parameter-specifications (client (directive percent-directive))
  (declare (ignore client))
  '((:type (integer 0) :default 1)))

(defmethod interpret-item (client (directive percent-directive) &optional parameters)
  (loop repeat (car parameters)
        do (terpri *destination*)))

(defmethod compile-item (client (directive percent-directive) &optional parameters)
  (case (car parameters)
    (0 '())
    (1 '((terpri *destination*)))
    (2 '((terpri *destination*)
         (terpri *destination*)))
    (otherwise
     `((loop repeat ,(car parameters)
             do (terpri *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.3 ~& Fresh line and newlines.

(defclass ampersand-directive (directive no-modifiers-mixin)
  ())

(defmethod specialize-directive
    (client (char (eql #\&)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'ampersand-directive))

(defmethod parameter-specifications
    (client (directive ampersand-directive))
  (declare (ignore client))
  '((:type (integer 0) :default 1)))

(defmethod interpret-item (client (item ampersand-directive) &optional parameters)
  (let ((how-many (car parameters)))
    (unless (zerop how-many)
      (fresh-line *destination*)
      (loop repeat (1- how-many)
            do (terpri *destination*)))))

(defmethod compile-item (client (item ampersand-directive) &optional parameters)
  (let ((how-many (car parameters)))
    (case how-many
      (0 nil)
      (1 `((fresh-line *destination*)))
      (2 `((fresh-line *destination*)
           (terpri *destination*)))
      (otherwise
       (if (numberp how-many)
           `((fresh-line *destination*)
             (loop repeat ,(1- how-many)
                   do (terpri *destination*)))
           `((let ((how-many ,how-many))
               (unless (zerop how-many)
                 (fresh-line *destination*)
                 (loop repeat (1- how-many)
                       do (terpri *destination*))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.4 ~| Page separators.

(defclass vertical-bar-directive (directive no-modifiers-mixin)
  ())

(defmethod specialize-directive
    (client (char (eql #\|)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'vertical-bar-directive))

(defmethod parameter-specifications
    (client (directive vertical-bar-directive))
  (declare (ignore client))
  '((:type (integer 0) :default 1)))

(defmethod interpret-item (client (directive vertical-bar-directive) &optional parameters)
  (loop repeat (car parameters)
        do (write-char #\Page *destination*)))

(defmethod compile-item (client (directive vertical-bar-directive) &optional parameters)
  (let ((how-many (car parameters)))
    (case how-many
      (0 nil)
      (1 `((write-char #\Page *destination*)))
      (2 `((write-char #\Page *destination*)
           (write-char #\Page *destination*)))
      (otherwise
       `((loop repeat ,how-many
               do (write-char #\Page *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.5 ~~ Tildes.

(defclass tilde-directive (directive no-modifiers-mixin)
  ())

(defmethod specialize-directive
    (client (char (eql #\~)) directive end-directive)
  (declare (ignore client end-directive))
  (change-class directive 'tilde-directive))

(defmethod parameter-specifications (client (directive tilde-directive))
  (declare (ignore client))
  '((:type (integer 0) :default 1)))

(defmethod interpret-item (client (directive tilde-directive) &optional parameters)
  (loop repeat (car parameters)
        do (write-char #\~ *destination*)))

(defmethod compile-item (client (directive tilde-directive) &optional parameters)
  (let ((how-many (car parameters)))
    (case how-many
      (0 nil)
      (1 `((write-char #\~ *destination*)))
      (2 `((write-char #\~ *destination*)
           (write-char #\~ *destination*)))
      (otherwise
       `((loop repeat ,how-many
               do (write-char #\~ *destination*)))))))
