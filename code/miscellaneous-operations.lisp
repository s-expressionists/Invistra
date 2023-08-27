;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8 Miscellaneous operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.2 ~) End of case conversion

(define-directive t #\)
    end-case-conversion-directive
    t
    (named-parameters-directive
     no-modifiers-mixin end-structured-directive-mixin)
    ())

(defmethod interpret-item (client (item end-case-conversion-directive) &optional parameters)
  (declare (ignore client parameters)))

(defmethod compile-item (client (item end-case-conversion-directive) &optional parameters)
  (declare (ignore client parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.1 ~( Case conversion

(define-directive t #\(
    case-conversion-directive
    end-case-conversion-directive
    (named-parameters-directive structured-directive-mixin)
    ())

(defmethod interpret-item (client (item case-conversion-directive) &optional parameters)
  (declare (ignore parameters))
  (let* ((colonp (colonp item))
         (at-signp (at-signp item))
         (*destination* (cond ((and colonp at-signp)
                               (make-instance 'upcase-stream :target *destination*))
                              (colonp
                               (make-instance 'capitalize-stream :target *destination*))
                              (at-signp
                               (make-instance 'first-capitalize-stream :target *destination*))
                              (t
                               (make-instance 'downcase-stream :target *destination*)))))
    (interpret-items client (aref (clauses item) 0))))

(defmethod compile-item (client (item case-conversion-directive) &optional parameters)
  (declare (ignore parameters))
  (let ((colonp (colonp item))
        (at-signp (at-signp item)))
    `((let ((*destination* ,(cond ((and colonp at-signp)
                                   '(make-instance 'upcase-stream :target *destination*))
                                  (colonp
                                   '(make-instance 'capitalize-stream :target *destination*))
                                  (at-signp
                                   '(make-instance 'first-capitalize-stream :target *destination*))
                                  (t
                                   '(make-instance 'downcase-stream :target *destination*)))))
        ,@(compile-items client (aref (clauses item) 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.3 ~p Plural

(define-directive t #\p plural-directive t (named-parameters-directive) ())

(defmethod interpret-item (client (item plural-directive) &optional parameters)
  (declare (ignore parameters))
  (when (colonp item)
    (go-to-argument -1))
  (if (at-signp item)
      (write-string (if (eql (consume-next-argument t) 1)
                        "y"
                        "ies")
                    *destination*)
      (unless (eql (consume-next-argument t) 1)
        (write-char #\s *destination*))))

(defmethod compile-item (client (item plural-directive) &optional parameters)
  (declare (ignore parameters))
  `(,@(when (colonp item)
        `((go-to-argument -1)))
    ,(if (at-signp item)
         `(write-string (if (eql (consume-next-argument t) 1)
                            "y"
                            "ies")
                        *destination*)
         `(unless (eql (consume-next-argument t) 1)
            (write-char #\s *destination*)))))
