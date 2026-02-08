(in-package #:invistra-extension)

(defclass extension-client (invistra:standard-client) ())

(defmethod make-argument-cursor ((client extension-client) object)
  (error 'type-error :datum object :expected-type 'sequence))

(defmethod invistra::make-argument-cursor ((client extension-client) (object sequence))
  (let ((position 0))
    (values (lambda ()
              (< position (length object)))
            (lambda ()
              position)
            (lambda ()
              (- (length object) position))
            (lambda (&optional (type t))
              (if (< position (length object))
                  (let ((value (elt object position)))
                    (unless (typep value type)
                      (error 'type-error :datum value :expected-type type))
                    (incf position)
                    value)
                  (error 'invistra::no-more-arguments)))
            (lambda ()
              (loop for p from position below (length object)
                    collect (elt object p)))
            (lambda (index &optional absolutep)
              (unless absolutep
                (incf position index))
              (unless (< -1 position (length object))
                (error 'invistra::go-to-out-of-bounds
                       :what-argument position
                       :max-arguments (length object)))))))
