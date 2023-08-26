;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1 Basic output

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.1 ~c Character

(define-directive t #\c c-directive t (named-parameters-directive) ())

(define-format-directive-interpreter c-directive
  (let ((char (consume-next-argument 'character)))
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

(define-format-directive-compiler c-directive
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
                   (write-string (char-name char) *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.2 ~% Newline.

(define-directive t #\% percent-directive t (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default 1)))

(define-format-directive-interpreter percent-directive
  (loop repeat how-many
        do (terpri *destination*)))

(define-format-directive-compiler percent-directive
  (case how-many
    (0 '())
    (1 '((terpri *destination*)))
    (2 '((terpri *destination*)
         (terpri *destination*)))
    (otherwise
     `((loop repeat how-many
             do (terpri *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.3 ~& Fresh line and newlines.

(define-directive t #\& ampersand-directive t (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default 1)))

(define-format-directive-interpreter ampersand-directive
  (unless (zerop how-many)
    (fresh-line *destination*)
    (loop repeat (1- how-many)
          do (terpri *destination*))))

(define-format-directive-compiler ampersand-directive
  (case how-many
    (:run-time-value
     `((unless (zerop how-many)
         (fresh-line *destination*)
         (loop repeat (1- how-many)
               do (terpri *destination*)))))
    (0 nil)
    (1 `((fresh-line *destination*)))
    (2 `((fresh-line *destination*)
         (terpri *destination*)))
    (otherwise
     `((fresh-line *destination*)
       (loop repeat (1- how-many)
             do (terpri *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.4 ~| Page separators.

(define-directive t #\| vertical-bar-directive t (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default 1)))

(define-format-directive-interpreter vertical-bar-directive
  (loop repeat how-many
        do (write-char #\Page *destination*)))

(define-format-directive-compiler vertical-bar-directive
  (case how-many
    (0 nil)
    (1 `((write-char #\Page *destination*)))
    (2 `((write-char #\Page *destination*)
         (write-char #\Page *destination*)))
    (otherwise
     `((loop repeat how-many
             do (write-char #\Page *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.5 ~~ Tildes.

(define-directive t #\~ tilde-directive t (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default 1)))

(define-format-directive-interpreter tilde-directive
  (loop repeat how-many
        do (write-char #\~ *destination*)))

(define-format-directive-compiler tilde-directive
  (case how-many
    (0 nil)
    (1 `((write-char #\~ *destination*)))
    (2 `((write-char #\~ *destination*)
         (write-char #\~ *destination*)))
    (otherwise
     `((loop repeat how-many
             do (write-char #\~ *destination*))))))
