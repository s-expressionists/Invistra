(in-package #:invistra-extension)

(defclass extension-client (invistra:standard-client) ())

(defmethod make-argument-cursor ((client extension-client) object)
  (error 'type-error :datum object :expected-type 'sequence))

(defmethod invistra::make-argument-cursor ((client extension-client) (object sequence))
  (let ((position 0))
    (values (length object)
            (lambda ()
              (< position (length object)))
            (lambda ()
              position)
            (lambda ()
              (if (< position (length object))
                  (prog1 (elt object position)
                    (incf position))
                  (error 'no-more-arguments)))
            (lambda ()
              (loop for p from position below (length object)
                    collect (elt object p)))
            (lambda (index &optional absolutep)
              (unless absolutep
                (incf position index))
              (unless (< -1 position (length object))
                (error 'go-to-out-of-bounds
                       :what-argument position
                       :max-arguments (length object)))))))
