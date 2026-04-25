;;;; 22.3.8 Miscellaneous operations

(in-package #:invistra)

;;; 22.3.8.2 ~) End of case conversion

(defclass end-case-conversion-directive
    (directive no-modifiers-mixin end-structured-directive-mixin)
  nil)

(defmethod specialize-directive ((client client) (char (eql #\))) directive)
  (change-class directive 'end-case-conversion-directive))

;;; 22.3.8.1 ~( Case conversion

(defclass case-conversion-directive (directive structured-directive-mixin) ())

(defmethod calculate-argument-position (position (directive case-conversion-directive))
  (calculate-argument-position position (first (clauses directive))))

(defmethod specialize-directive ((client client) (char (eql #\()) directive)
  (change-class directive 'case-conversion-directive))

(defmethod append-clause
    ((client client) (directive case-conversion-directive) items
     (terminator end-case-conversion-directive))
  (declare (ignore items)))

(defmethod append-clause
    ((client client) (directive case-conversion-directive) items (terminator null))
  (declare (ignore items))
  (signal-missing-directive client directive #\)))

(defmethod interpret-item
    ((client client) (directive case-conversion-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (let ((*format-output* (cond ((and colon-p at-sign-p)
                                  (make-upcase-stream))
                                 (colon-p
                                  (make-capitalize-stream))
                                 (at-sign-p
                                  (make-first-capitalize-stream))
                                 (t
                                  (make-downcase-stream)))))
      (interpret-item client (first (clauses directive))))))

(defmethod compile-item
    ((client client) (directive case-conversion-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    `((let ((*format-output* ,(cond ((and colon-p at-sign-p)
                                     '(make-upcase-stream))
                                    (colon-p
                                     '(make-capitalize-stream))
                                    (at-sign-p
                                     '(make-first-capitalize-stream))
                                    (t
                                     '(make-downcase-stream)))))
        ,@(compile-item client (first (clauses directive)))))))

;;; 22.3.8.3 ~p Plural

(defclass plural-directive (directive) nil)

(defmethod specialize-directive ((client client) (char (eql #\P)) directive)
  (change-class directive 'plural-directive))

(defmethod calculate-argument-position (position (directive plural-directive))
  (if (or (colon-p directive) (null position))
      position
      (1+ position)))

(defmethod interpret-item
    ((client client) (directive plural-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (when colon-p
      (go-to-argument -1))
    (if at-sign-p
        (write-string (if (eql (pop-argument) 1)
                          "y"
                          "ies")
                      *format-output*)
        (unless (eql (pop-argument) 1)
          (write-char #\s *format-output*)))))

(defmethod compile-item
    ((client client) (directive plural-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    `(,@(when colon-p
          (go-to-argument-forms -1))
      ,(if at-sign-p
           `(write-string (if (eql ,(pop-argument-form) 1)
                              "y"
                              "ies")
                          *format-output*)
           `(unless (eql ,(pop-argument-form) 1)
              (write-char #\s *format-output*))))))
