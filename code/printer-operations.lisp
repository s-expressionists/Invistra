;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4 Printer operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.1 ~a Aesthetic.

(defclass a-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\A)) directive (end-directive t))
  (change-class directive 'a-directive))

(defmethod parameter-specifications ((client t) (directive a-directive))
  '((:name mincol :type integer :default 0)
    (:name colinc :type (integer 0) :default 1)
    (:name minpad :type integer :default 0)
    (:name padchar :type character :default #\Space)))

(defmethod interpret-item (client (directive a-directive) &optional parameters)
  (let ((*print-escape* nil)
        (*print-readably* nil)
        (arg (pop-argument)))
    (apply #'write-string-with-padding
           (if (and (colon-p directive) (null arg))
               "()"
               (with-output-to-string (stream)
                 (incless:write-object client arg stream)))
           (at-sign-p directive) parameters)))

(defmethod compile-item (client (directive a-directive) &optional parameters)
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (destructuring-bind (mincol colinc minpad padchar)
        parameters
      (cond ((and colon-p
                  (eql 0 mincol)
                  (eql 0 minpad))
             `((let ((*print-escape* nil)
                     (*print-readably* nil)
                     (arg (pop-argument)))
                 (if (null arg)
                     (write-string "()" *destination*)
                     (incless:write-object ,(incless:client-form client)
                                           arg
                                           *destination*)))))
            ((and (eql 0 mincol)
                  (eql 0 minpad))
             `((let ((*print-escape* nil)
                     (*print-readably* nil))
                 (incless:write-object ,(incless:client-form client)
                                       (pop-argument)
                                       *destination*))))
            (colon-p
             `((let ((*print-escape* nil)
                     (*print-readably* nil)
                     (arg (pop-argument)))
                 (write-string-with-padding
                  (if (null arg)
                      "()"
                      (with-output-to-string (stream)
                        (incless:write-object ,(incless:client-form client) arg stream)))
                  ,at-sign-p ,@parameters))))
            (t
             `((let ((*print-escape* nil)
                     (*print-readably* nil))
                 (write-string-with-padding
                  (with-output-to-string (stream)
                    (incless:write-object ,(incless:client-form client)
                                          (pop-argument)
                                          stream))
                  ,at-sign-p ,@parameters))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.2 ~s Standard.

(defclass s-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\S)) directive (end-directive t))
  (change-class directive 's-directive))

(defmethod parameter-specifications ((client t) (directive s-directive))
  '((:name mincol :type integer :default 0)
    (:name colinc :type (integer 0) :default 1)
    (:name minpad :type integer :default 0)
    (:name padchar :type character :default #\Space)))

(defmethod interpret-item (client (directive s-directive) &optional parameters)
  (let ((*print-escape* t)
        (arg (pop-argument)))
    (apply #'write-string-with-padding
           (if (and (colon-p directive) (null arg))
               "()"
               (with-output-to-string (stream)
                 (incless:write-object client arg stream)))
           (at-sign-p directive) parameters)))

(defmethod compile-item (client (directive s-directive) &optional parameters)
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (destructuring-bind (mincol colinc minpad padchar)
        parameters
      (cond ((and colon-p
                  (eql 0 mincol)
                  (eql 0 minpad))
             `((let ((*print-escape* t)
                     (arg (pop-argument)))
                 (if (null arg)
                     (write-string "()" *destination*)
                     (incless:write-object ,(incless:client-form client)
                                           arg
                                           *destination*)))))
            ((and (eql 0 mincol)
                  (eql 0 minpad))
             `((let ((*print-escape* t))
                 (incless:write-object ,(incless:client-form client)
                                       (pop-argument)
                                       *destination*))))
            (colon-p
             `((let ((*print-escape* t)
                     (arg (pop-argument)))
                 (write-string-with-padding
                  (if (null arg)
                      "()"
                      (with-output-to-string (stream)
                        (incless:write-object ,(incless:client-form client) arg stream)))
                  ,at-sign-p ,@parameters))))
            (t
             `((let ((*print-escape* t))
                 (write-string-with-padding
                  (with-output-to-string (stream)
                    (incless:write-object ,(incless:client-form client)
                                          (pop-argument)
                                          stream))
                  ,at-sign-p ,@parameters))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.3 ~w Write.

(defclass w-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\W)) directive (end-directive t))
  (change-class directive 'w-directive))

(defmethod layout-requirements ((item w-directive))
  (list :logical-block))

(defmethod interpret-item (client (directive w-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (let ((arg (pop-argument)))
      (cond ((and colon-p at-sign-p)
             (let ((*print-pretty* t)
                   (*print-level* nil)
                   (*print-length* nil))
               (incless:write-object client arg *destination*)))
            (colon-p
             (let ((*print-pretty* t))
               (incless:write-object client arg *destination*)))
            (at-sign-p
             (let ((*print-level* nil)
                   (*print-length* nil))
               (incless:write-object client arg *destination*)))
            (t
             (incless:write-object client arg *destination*))))))

(defmethod compile-item (client (directive w-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (cond ((and colon-p at-sign-p)
           `((let ((*print-pretty* t)
                   (*print-level* nil)
                   (*print-length* nil))
               (incless:write-object ,(incless:client-form client) (pop-argument) *destination*))))
          (colon-p
           `((let ((*print-pretty* t))
               (incless:write-object ,(incless:client-form client) (pop-argument) *destination*))))
          (at-sign-p
           `((let ((*print-level* nil)
                   (*print-length* nil))
               (incless:write-object ,(incless:client-form client) (pop-argument) *destination*))))
          (t
           `((incless:write-object ,(incless:client-form client) (pop-argument) *destination*))))))
