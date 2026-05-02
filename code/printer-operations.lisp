;;;; 22.3.4 Printer operations

(in-package #:invistra)

;;; 22.3.4.1 ~a Aesthetic.

(defclass aesthetic-directive (directive) nil)

(defmethod specialize-directive ((client client) (char (eql #\A)) directive)
  (change-class directive 'aesthetic-directive))

(defmethod parameter-specifications ((client client) (directive aesthetic-directive))
  '((:name mincol
     :type integer
     :bind nil
     :default 0)
    (:name colinc
     :type (integer 0)
     :bind nil
     :default 1)
    (:name minpad
     :type integer
     :bind nil
     :default 0)
    (:name padchar
     :type character
     :bind nil
     :default #\Space)))

(defmethod traverse-item ((client client) (directive aesthetic-directive))
  (go-to-argument 1))

(defun format-aesthetic (client colon-p at-sign-p mincol colinc minpad padchar value)
  (flet ((write-value ()
           (let ((*print-escape* nil)
                 (*print-readably* nil))
             (if (and colon-p (null value))
                 (write-string "()" *format-output*)
                 (incless:write-object client value *format-output*)))))
    (if (and (zerop mincol)
             (zerop minpad))
        (write-value)
        (write-with-padding (at-sign-p mincol colinc minpad padchar)
          (write-value)))))

(defmethod interpret-item
    ((client client) (directive aesthetic-directive) &optional parameters)
  (multiple-value-call #'format-aesthetic
    client (colon-p directive) (at-sign-p directive) (values-list parameters) (pop-argument)))

(defmethod compile-item
    ((client client) (directive aesthetic-directive) &optional parameters)
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (destructuring-bind (mincol colinc minpad padchar)
        parameters
      (cond ((or (not (constantp colinc))
                 (not (constantp padchar))
                 (not (eql 0 mincol))
                 (not (eql 0 minpad)))
             `((format-aesthetic ,(trinsic:client-form client) ,colon-p ,at-sign-p ,@parameters
                                 ,(pop-argument-form))))
            (colon-p
             (let ((arg-form (pop-argument-form)))
               (if (symbolp arg-form)
                   `((let ((*print-escape* nil)
                           (*print-readably* nil))
                       (if (null ,arg-form)
                           (write-string "()" *format-output*)
                           (incless:write-object ,(trinsic:client-form client)
                                                 ,arg-form
                                                 *format-output*))))
                   (with-unique-names (arg)
                     `((let ((*print-escape* nil)
                             (*print-readably* nil)
                             (,arg ,arg-form))
                         (if (null ,arg)
                             (write-string "()" *format-output*)
                             (incless:write-object ,(trinsic:client-form client)
                                                   ,arg
                                                   *format-output*))))))))
            (t
             `((let ((*print-escape* nil)
                     (*print-readably* nil))
                 (incless:write-object ,(trinsic:client-form client)
                                       ,(pop-argument-form)
                                       *format-output*))))))))

;;; 22.3.4.2 ~s Standard.

(defclass standard-directive (directive) nil)

(defmethod specialize-directive ((client client) (char (eql #\S)) directive)
  (change-class directive 'standard-directive))

(defmethod parameter-specifications ((client client) (directive standard-directive))
  '((:name mincol
     :type integer
     :bind nil
     :default 0)
    (:name colinc
     :type (integer 0)
     :bind nil
     :default 1)
    (:name minpad
     :type integer
     :bind nil
     :default 0)
    (:name padchar
     :type character
     :bind nil
     :default #\Space)))

(defmethod traverse-item ((client client) (directive standard-directive))
  (go-to-argument 1))

(defun format-standard (client colon-p at-sign-p mincol colinc minpad padchar value)
  (flet ((write-value ()
           (let ((*print-escape* t))
             (if (and colon-p (null value))
                 (write-string "()" *format-output*)
                 (incless:write-object client value *format-output*)))))
    (if (and (zerop mincol)
             (zerop minpad))
        (write-value)
        (write-with-padding (at-sign-p mincol colinc minpad padchar)
          (write-value)))))

(defmethod interpret-item
    ((client client) (directive standard-directive) &optional parameters)
  (multiple-value-call #'format-standard
    client (colon-p directive) (at-sign-p directive) (values-list parameters) (pop-argument)))

(defmethod compile-item
    ((client client) (directive standard-directive) &optional parameters)
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (destructuring-bind (mincol colinc minpad padchar)
        parameters
      (cond ((or (not (constantp colinc))
                 (not (constantp padchar))
                 (not (eql 0 mincol))
                 (not (eql 0 minpad)))
             `((format-standard ,(trinsic:client-form client) ,colon-p ,at-sign-p ,@parameters
                                ,(pop-argument-form))))
            (colon-p
             (let ((arg-form (pop-argument-form)))
               (if (symbolp arg-form)
                   `((let ((*print-escape* t))
                       (if (null ,arg-form)
                           (write-string "()" *format-output*)
                           (incless:write-object ,(trinsic:client-form client)
                                                 ,arg-form
                                                 *format-output*))))
                   (with-unique-names (arg)
                     `((let ((*print-escape* t)
                             (,arg ,arg-form))
                         (if (null ,arg)
                             (write-string "()" *format-output*)
                             (incless:write-object ,(trinsic:client-form client)
                                                   ,arg
                                                   *format-output*))))))))
            (t
             `((let ((*print-escape* t))
                 (incless:write-object ,(trinsic:client-form client)
                                       ,(pop-argument-form)
                                       *format-output*))))))))

;;; 22.3.4.3 ~w Write.

(defclass write-directive (directive) nil)

(defmethod specialize-directive ((client client) (char (eql #\W)) directive)
  (change-class directive 'write-directive))

(defmethod check-item-syntax :around
    ((client client) (directive write-directive) global-layout local-layout parent
     &optional group position)
  (call-next-method client directive global-layout
                    (merge-layout client directive global-layout local-layout :logical-block t)
                    parent group position))

(defmethod traverse-item ((client client) (directive write-directive))
  (go-to-argument 1))

(defun format-write (client colon-p at-sign-p value)
  (cond ((and colon-p at-sign-p)
         (let ((*print-pretty* t)
               (*print-level* nil)
               (*print-length* nil))
           (incless:write-object client value *format-output*)))
        (colon-p
         (let ((*print-pretty* t))
           (incless:write-object client value *format-output*)))
        (at-sign-p
         (let ((*print-level* nil)
               (*print-length* nil))
           (incless:write-object client value *format-output*)))
        (t
         (incless:write-object client value *format-output*))))

(defmethod interpret-item
    ((client client) (directive write-directive) &optional parameters)
  (declare (ignore parameters))
  (format-write client (colon-p directive) (at-sign-p directive) (pop-argument)))

(defmethod compile-item
    ((client client) (directive write-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (let ((write-form `(incless:write-object ,(trinsic:client-form client) ,(pop-argument-form)
                                             *format-output*)))
      (cond ((and colon-p at-sign-p)
             `((let ((*print-pretty* t)
                     (*print-level* nil)
                     (*print-length* nil))
                 ,write-form)))
            (colon-p
             `((let ((*print-pretty* t))
                 ,write-form)))
            (at-sign-p
             `((let ((*print-level* nil)
                     (*print-length* nil))
                 ,write-form)))
            (t
             `(,write-form))))))
