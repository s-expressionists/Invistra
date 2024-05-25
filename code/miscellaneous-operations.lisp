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
    ((client t) (char (eql #\))) directive (end-directive t))
  (change-class directive 'end-case-conversion-directive))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.1 ~( Case conversion

(defclass case-conversion-directive
    (directive structured-directive-mixin) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\()) directive
     (end-directive end-case-conversion-directive))
  (change-class directive 'case-conversion-directive))

(defmethod specialize-directive
    ((client t) (char (eql #\()) directive (end-directive t))
  (error 'unmatched-directive
         :directive directive
         :control-string (control-string directive)
         :tilde-position (start directive)))

(defmethod interpret-item (client (directive case-conversion-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (let ((*destination* (cond ((and colon-p at-sign-p)
                                (make-instance 'upcase-stream :target *destination*))
                               (colon-p
                                (make-instance 'capitalize-stream :target *destination*))
                               (at-sign-p
                                (make-instance 'first-capitalize-stream :target *destination*))
                               (t
                                (make-instance 'downcase-stream :target *destination*)))))
      (interpret-items client (aref (clauses directive) 0)))))

(defmethod compile-item (client (directive case-conversion-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    `((let ((*destination* ,(cond ((and colon-p at-sign-p)
                                   '(make-instance 'upcase-stream :target *destination*))
                                  (colon-p
                                   '(make-instance 'capitalize-stream :target *destination*))
                                  (at-sign-p
                                   '(make-instance 'first-capitalize-stream :target *destination*))
                                  (t
                                   '(make-instance 'downcase-stream :target *destination*)))))
        ,@(compile-items client (aref (clauses directive) 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.3 ~p Plural

(defclass plural-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\P)) directive (end-directive t))
  (change-class directive 'plural-directive))

(defmethod interpret-item (client (directive plural-directive) &optional parameters)
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
                      *destination*)
        (unless (eql (pop-argument) 1)
          (write-char #\s *destination*)))))

(defmethod compile-item (client (directive plural-directive) &optional parameters)
  (declare (ignore parameters))
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    `(,@(when colon-p
          `((go-to-argument -1)))
      ,(if at-sign-p
           `(write-string (if (eql (pop-argument) 1)
                              "y"
                              "ies")
                          *destination*)
           `(unless (eql (pop-argument) 1)
              (write-char #\s *destination*))))))
