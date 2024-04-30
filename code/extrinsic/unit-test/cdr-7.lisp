(in-package #:invistra-extrinsic/unit-test)

(defun cdr-7-fmt (stream argument colonp atsignp &rest params)
  (declare (ignore argument colonp atsignp))
  (prin1 params stream))

(define-equal-test cdr-7.comma.1
  "(1 2)"
  (fmt nil "~1,2/ieut:cdr-7-fmt/" t))

(define-equal-test cdr-7.comma.2
  "(1 2)"
  (fmt nil "~1,2,/ieut:cdr-7-fmt/" t))

(define-equal-test cdr-7.comma.3
  "(1 2)"
  (fmt nil "~1,2:/ieut:cdr-7-fmt/" t))

(define-equal-test cdr-7.comma.4
  "(1 2)"
  (fmt nil "~1,2,:/ieut:cdr-7-fmt/" t))

(define-equal-test cdr-7.parameter.1
  "(1 2)"
  (invistra-extrinsic:format nil "~1,v/ieut:cdr-7-fmt/" 2 3))

(define-equal-test cdr-7.parameter.2
  "(1 NIL)"
  (invistra-extrinsic:format nil "~1,v/ieut:cdr-7-fmt/" nil 3))

(define-equal-test cdr-7.parameter.3
  "(1 2)"
  (invistra-extrinsic:format nil "~1,v,/ieut:cdr-7-fmt/" 2 3))

(define-equal-test cdr-7.parameter.4
  "(1 NIL)"
  (invistra-extrinsic:format nil "~1,v,/ieut:cdr-7-fmt/" nil 3))

(define-equal-test cdr-7.parameter.5
  "(1 2)"
  (invistra-extrinsic:format nil "~1,v:/ieut:cdr-7-fmt/" 2 3))

(define-equal-test cdr-7.parameter.6
  "(1 NIL)"
  (invistra-extrinsic:format nil "~1,v:/ieut:cdr-7-fmt/" nil 3))

(define-equal-test cdr-7.parameter.7
  "(1 2)"
  (invistra-extrinsic:format nil "~1,v,:/ieut:cdr-7-fmt/" 2 3))

(define-equal-test cdr-7.parameter.8
  "(1 NIL)"
  (invistra-extrinsic:format nil "~1,v,:/ieut:cdr-7-fmt/" nil 3))
