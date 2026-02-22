;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8 Miscellaneous operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.2 ~) End of case conversion

(defclass end-case-conversion-directive
    (directive no-modifiers-mixin
     end-structured-directive-mixin)
  nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\))) directive (end-directive t))
  (change-class directive 'end-case-conversion-directive))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.1 ~( Case conversion

(defclass case-conversion-directive
    (directive structured-directive-mixin) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\()) directive
     (end-directive end-case-conversion-directive))
  (change-class directive 'case-conversion-directive))

(defmethod specialize-directive
    ((client standard-client) (char (eql #\()) directive (end-directive t))
  (signal-missing-end-case-conversion client directive))

(defmethod calculate-argument-position (position (directive case-conversion-directive))
  (reduce #'calculate-argument-position (aref (clauses directive) 0)
          :initial-value (call-next-method)))

(defmethod interpret-item
    ((client standard-client) (directive case-conversion-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (let ((*format-output* (cond ((and colon-p at-sign-p)
                                (make-instance 'upcase-stream :target *format-output*))
                               (colon-p
                                (make-instance 'capitalize-stream :target *format-output*))
                               (at-sign-p
                                (make-instance 'first-capitalize-stream
                                               :target *format-output*))
                               (t
                                (make-instance 'downcase-stream :target *format-output*)))))
      (interpret-items client (aref (clauses directive) 0)))))

(defmethod compile-item
    ((client standard-client) (directive case-conversion-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    `((let ((*format-output* ,(cond ((and colon-p at-sign-p)
                                   '(make-instance 'upcase-stream :target *format-output*))
                                  (colon-p
                                   '(make-instance 'capitalize-stream :target *format-output*))
                                  (at-sign-p
                                   '(make-instance 'first-capitalize-stream
                                     :target *format-output*))
                                  (t
                                   '(make-instance 'downcase-stream :target *format-output*)))))
        ,@(compile-items client (aref (clauses directive) 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.3 ~p Plural

(defclass plural-directive (directive) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\P)) directive (end-directive t))
  (change-class directive 'plural-directive))

(defmethod calculate-argument-position (position (directive plural-directive))
  (if (or (colon-p directive) (null position))
      position
      (1+ position)))

(defmethod interpret-item
    ((client standard-client) (directive plural-directive) &optional parameters)
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
    ((client standard-client) (directive plural-directive) &optional parameters)
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
