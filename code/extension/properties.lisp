(in-package #:invistra-extension)

(defmethod incless:printing-char-p ((client client) char)
  (and (graphic-char-p char) (not (incless:whitespace-char-p client char))))

(defmethod trinsic:features-list nconc ((client client))
  (list :format/invistra-extension))
