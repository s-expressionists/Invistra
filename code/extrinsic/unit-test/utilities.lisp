(cl:in-package #:invistra-extrinsic/unit-test)

(defun format-eval (&rest args)
  (apply #'invistra-extrinsic:format args))

(defun rti (x) x)

(defmacro my-with-standard-io-syntax (&body body)
  `(let ((*print-array* t)
         (*print-base* 10)
         (*print-case* :upcase)
         (*print-circle* nil)
         (*print-escape* t)
         (*print-gensym* t)
         (*print-length* nil)
         (*print-level* nil)
         (*print-lines* nil)
         (*print-miser-width* nil)
         (*print-pretty* nil)
         (*print-radix* nil)
         (*print-readably* t)
         (*print-right-margin* nil))
     ,@body))

(defmacro is-equal (expected form)
  `(progn (macrolet ((fmt (destination control-string &rest args)
                       `(invistra-extrinsic:format ,destination ,control-string ,@args)))
      (is equal
          ,expected
          ,form))
    (macrolet ((fmt (destination control-string &rest args)
                 `(invistra-extrinsic:format ,destination (rti ,control-string) ,@args)))
      (is equal
          ,expected
          ,form))))

(defmacro define-equal-test (name expected form)
  `(define-test ,name
     (my-with-standard-io-syntax
       (macrolet ((fmt (destination control-string &rest args)
                    `(invistra-extrinsic:format ,destination ,control-string ,@args)))
         (is equal
             ,expected
             ,form))
       (macrolet ((fmt (destination control-string &rest args)
                    `(invistra-extrinsic:format ,destination (rti ,control-string) ,@args)))
         (is equal
             ,expected
             ,form)))))

(defmacro define-control-fail-test (name form)
  `(define-test ,name
     :compile-at :execute
     (fail-compile (macrolet ((fmt (destination control-string &rest args)
                                `(invistra-extrinsic:format ,destination ,control-string ,@args)))
                     ,form)
                   condition)
     (fail (macrolet ((fmt (destination control-string &rest args)
                        `(invistra-extrinsic:format ,destination (rti ,control-string) ,@args)))
             ,form))))

(defmacro define-argument-fail-test (name form)
  `(define-test ,name
     :compile-at :execute
     (fail (macrolet ((fmt (destination control-string &rest args)
                                `(invistra-extrinsic:format ,destination ,control-string ,@args)))
                     ,form))
     (fail (macrolet ((fmt (destination control-string &rest args)
                        `(invistra-extrinsic:format ,destination (rti ,control-string) ,@args)))
             ,form))))
