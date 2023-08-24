;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4 Printer operations

(in-package #:invistra)

(defun print-a-or-s (raw-output at-signp mincol colinc minpad padchar)
  (let ((pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
    (if at-signp
        (progn (loop repeat pad-length do (write-char padchar *destination*))
               (write-string raw-output *destination*))
        (progn (write-string raw-output *destination*)
               (loop repeat pad-length do (write-char padchar *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.1 ~a Aesthetic.

(define-directive t #\a a-directive t (named-parameters-directive)
    ((mincol :type integer :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type integer :default-value 0)
     (padchar :type character :default-value #\Space)))

(define-format-directive-interpreter a-directive
  (let ((*print-escape* nil)
        (*print-readably* nil)
        (arg (consume-next-argument t)))
    (print-a-or-s (if (and colonp (null arg))
                      "()"
                      (with-output-to-string (stream)
                        (incless:write-object client arg stream)))
                  at-signp mincol colinc minpad padchar)))

(define-format-directive-compiler a-directive
  `((let* ((*print-escape* nil)
           (*print-readably* nil)
           (arg (consume-next-argument t))
           (raw-output
             ,(if colonp
                  `(if (null arg)
                       "()"
                       (with-output-to-string (stream)
                         (incless:write-object ,(incless:client-form client) arg stream)))
                  `(with-output-to-string (stream)
                     (incless:write-object ,(incless:client-form client) arg stream))))
           (pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
      ,@(if at-signp
            `((loop repeat pad-length
                    do (write-char padchar *destination*))
              (write-string raw-output *destination*))
            `((write-string raw-output *destination*)
              (loop repeat pad-length
                    do (write-char padchar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.2 ~s Standard.

(define-directive t #\s s-directive t (named-parameters-directive)
    ((mincol :type integer :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type integer :default-value 0)
     (padchar :type character :default-value #\Space)))

(define-format-directive-interpreter s-directive
  (let ((*print-escape* t)
        (arg (consume-next-argument t)))
    (print-a-or-s (if (and colonp (null arg))
                      "()"
                      (with-output-to-string (stream)
                        (incless:write-object client arg stream)))
                  at-signp mincol colinc minpad padchar)))

(define-format-directive-compiler s-directive
  `((let* ((*print-escape* t)
           (arg (consume-next-argument t))
           (raw-output
             ,(if colonp
                  `(if (null arg)
                       "()"
                       (with-output-to-string (stream)
                         (incless:write-object ,(incless:client-form client) arg stream)))
                  `(with-output-to-string (stream)
                     (incless:write-object ,(incless:client-form client) arg stream))))
           (pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
      ,@(if at-signp
            `((loop repeat pad-length
                    do (write-char padchar *destination*))
              (write-string raw-output *destination*))
            `((write-string raw-output *destination*)
              (loop repeat pad-length
                    do (write-char padchar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.3 ~w Write.

(define-directive t #\w w-directive t (named-parameters-directive) ())

(defmethod layout-requirements ((item w-directive))
  (list :logical-block))

(define-format-directive-interpreter w-directive
  (cond ((and colonp at-signp)
         (let ((*print-pretty* t)
               (*print-level* nil)
               (*print-length* nil))
           (incless:write-object client (consume-next-argument t) *destination*)))
        (colonp
         (let ((*print-pretty* t))
           (incless:write-object client (consume-next-argument t) *destination*)))
        (at-signp
         (let ((*print-level* nil)
               (*print-length* nil))
           (incless:write-object client (consume-next-argument t) *destination*)))
        (t
         (incless:write-object client (consume-next-argument t) *destination*))))

(define-format-directive-compiler w-directive
  (cond ((and colonp at-signp )
         `((let ((*print-pretty* t)
                 (*print-level* nil)
                 (*print-length* nil))
             (incless:write-object ,(incless:client-form client) (consume-next-argument t) *destination*))))
        (colonp
         `((let ((*print-pretty* t))
             (incless:write-object ,(incless:client-form client) (consume-next-argument t) *destination*))))
        (at-signp
         `((let ((*print-level* nil)
                 (*print-length* nil))
             (incless:write-object ,(incless:client-form client) (consume-next-argument t) *destination*))))
        (t
         `((incless:write-object ,(incless:client-form client) (consume-next-argument t) *destination*)))))
