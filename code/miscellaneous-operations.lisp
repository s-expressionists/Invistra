;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8 Miscellaneous operations

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.2 ~) End of case conversion

(define-directive #\)
    end-case-conversion-directive
    nil
    (named-parameters-directive
     no-modifiers-mixin end-structured-directive-mixin)
    ())

(define-format-directive-interpreter end-case-conversion-directive
    ;; do nothing
    nil)

(define-format-directive-compiler end-case-conversion-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.1 ~( Case conversion

(define-directive #\(
    case-conversion-directive
    end-case-conversion-directive
    (named-parameters-directive structured-directive-mixin)
    ())

(define-format-directive-interpreter case-conversion-directive
  (let ((*destination* (cond ((and colonp at-signp)
                              (make-instance 'upcase-stream :target *destination*))
                             (colonp
                              (make-instance 'capitalize-stream :target *destination*))
                             (at-signp
                              (make-instance 'first-capitalize-stream :target *destination*))
                             (t
                              (make-instance 'downcase-stream :target *destination*)))))
    (interpret-items client (aref (clauses directive) 0))))

(define-format-directive-compiler case-conversion-directive
  `((let ((*destination* ,(cond ((and colonp at-signp)
                                 '(make-instance 'upcase-stream :target *destination*))
                                (colonp
                                 '(make-instance 'capitalize-stream :target *destination*))
                                (at-signp
                                 '(make-instance 'first-capitalize-stream :target *destination*))
                                (t
                                 '(make-instance 'downcase-stream :target *destination*)))))
      ,@(compile-items client (aref (clauses directive) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.3 ~p Plural

(define-directive #\p plural-directive nil (named-parameters-directive) ())

(define-format-directive-interpreter plural-directive
  (when colonp
    (go-to-argument -1))
  (if at-signp
      (write-string (if (eql (consume-next-argument t) 1)
                        "y"
                        "ies")
                    *destination*)
      (unless (eql (consume-next-argument t) 1)
        (write-char #\s *destination*))))

(define-format-directive-compiler plural-directive
  `(,@(when colonp
        `((go-to-argument -1)))
    ,(if at-signp
         `(write-string (if (eql (consume-next-argument t) 1)
                            "y"
                            "ies")
                        *destination*)
         `(unless (eql (consume-next-argument t) 1)
            (write-char #\s *destination*)))))
