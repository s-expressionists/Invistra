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

(defclass a-directive (named-parameters-directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\A)) directive (end-directive t))
  (change-class directive 'a-directive))

(defmethod parameter-specifications ((client t) (directive a-directive))
  '((:type integer :default 0)
    (:type (integer 0) :default 1)
    (:type integer :default 0)
    (:type character :default #\Space)))

(defmethod interpret-item (client (directive a-directive) &optional parameters)
  (let ((*print-escape* nil)
        (*print-readably* nil)
        (arg (consume-next-argument t)))
    (apply #'print-a-or-s
           (if (and (colonp directive) (null arg))
               "()"
               (with-output-to-string (stream)
                 (incless:write-object client arg stream)))
           (at-signp directive) parameters)))

(defmethod compile-item (client (directive a-directive) &optional parameters)
  `((let* ((*print-escape* nil)
           (*print-readably* nil)
           (parameters (list ,@parameters))
           (arg (consume-next-argument t)))
      (apply #'print-a-or-s
             ,(if (colonp directive)
                  `(if (null arg)
                       "()"
                       (with-output-to-string (stream)
                         (incless:write-object ,(incless:client-form client) arg stream)))
                  `(with-output-to-string (stream)
                     (incless:write-object ,(incless:client-form client) arg stream)))
             ,(at-signp directive) parameters))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.2 ~s Standard.

(defclass s-directive (named-parameters-directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\S)) directive (end-directive t))
  (change-class directive 's-directive))

(defmethod parameter-specifications ((client t) (directive s-directive))
  '((:type integer :default 0)
    (:type (integer 0) :default 1)
    (:type integer :default 0)
    (:type character :default #\Space)))

(defmethod interpret-item (client (directive s-directive) &optional parameters)
  (let ((*print-escape* t)
        (arg (consume-next-argument t)))
    (apply #'print-a-or-s
           (if (and (colonp directive) (null arg))
               "()"
               (with-output-to-string (stream)
                 (incless:write-object client arg stream)))
           (at-signp directive) parameters)))

(defmethod compile-item (client (directive s-directive) &optional parameters)
  `((let* ((*print-escape* t)
           (parameters (list ,@parameters))
           (arg (consume-next-argument t)))
      (apply #'print-a-or-s
             ,(if (colonp directive)
                  `(if (null arg)
                       "()"
                       (with-output-to-string (stream)
                         (incless:write-object ,(incless:client-form client) arg stream)))
                  `(with-output-to-string (stream)
                     (incless:write-object ,(incless:client-form client) arg stream)))
             ,(at-signp directive) parameters))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.3 ~w Write.

(defclass w-directive (named-parameters-directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\W)) directive (end-directive t))
  (change-class directive 'w-directive))

(defmethod layout-requirements ((item w-directive))
  (list :logical-block))

(defmethod interpret-item (client (directive w-directive) &optional parameters)
  (declare (ignore parameters))
  (let ((arg (consume-next-argument t))
        (colonp (colonp directive))
        (at-signp (at-signp directive)))
    (cond ((and colonp at-signp)
           (let ((*print-pretty* t)
                 (*print-level* nil)
                 (*print-length* nil))
             (incless:write-object client arg *destination*)))
          (colonp
           (let ((*print-pretty* t))
             (incless:write-object client arg *destination*)))
          (at-signp
           (let ((*print-level* nil)
                 (*print-length* nil))
             (incless:write-object client arg *destination*)))
          (t
           (incless:write-object client arg *destination*)))))

(defmethod compile-item (client (directive w-directive) &optional parameters)
  (declare (ignore parameters))
  (let ((colonp (colonp directive))
        (at-signp (at-signp directive)))
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
           `((incless:write-object ,(incless:client-form client) (consume-next-argument t) *destination*))))))
