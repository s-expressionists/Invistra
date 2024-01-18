(in-package #:invistra-numeral/test)

(define-test "kaktovik.01"
  (let ((incless-extrinsic:*client* (make-instance 'numeral-client)))
    (is equal
        "𝋒𝋆𝋀"
        (with-output-to-string (stream)
          (funcall (my-formatter "~k") stream (+ (* 18 20 20) (* 6 20)))))))
