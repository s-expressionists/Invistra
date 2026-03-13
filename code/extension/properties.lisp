(in-package #:invistra-extrinsic)

(defmethod invistra:printing-char-p ((client client) char)
  (and (graphic-char-p char) (not (invistra:whitespace-char-p client char))))
