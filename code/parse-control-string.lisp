(cl:in-package #:invistra)

(defmethod parse-parameter ((client standard-client) directive (character (eql #\,)))
  (with-accessors ((parameters parameters))
      directive
    (setf parameters
          (nconc parameters
                 (list (make-instance 'literal-parameter))))
    t))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\V)))
  (with-accessors ((end end)
                   (parameters parameters))
      directive
    (setf parameters
          (nconc parameters
                 (list (make-instance 'argument-reference-parameter))))
    (incf end)
    t))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\#)))
  (with-accessors ((end end)
                   (parameters parameters))
      directive
    (setf parameters
          (nconc parameters
                 (list (make-instance 'remaining-argument-count-parameter))))
    (incf end)
    t))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\')))
  (with-accessors ((control-string control-string)
                   (end end)
                   (parameters parameters))
      directive
    (incf end)
    (cond ((< end (length control-string))
           (setf parameters
                 (nconc parameters
                        (list (make-instance 'literal-parameter
                                             :value (char control-string end)))))
           (incf end)
           t)
          (t
           (error 'end-of-control-string
                  :directive directive
                  :positions (list end))))))

(defun parse-integer-parameter (directive)
  (with-accessors ((control-string control-string)
                   (end end)
                   (parameters parameters))
      directive
    (multiple-value-bind (value end-position)
        (parse-integer control-string :start end :junk-allowed t)
      (setf end end-position)
      (when (null value)
        (error 'expected-integer-error
               :directive directive
               :positions (list end-position)))
      (setf parameters
            (nconc parameters
                   (list (make-instance 'literal-parameter :value value))))
      t)))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\-)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\+)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\0)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\1)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\2)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\3)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\4)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\5)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\6)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\7)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\8)))
  (parse-integer-parameter directive))

(defmethod parse-parameter ((client standard-client) directive (character (eql #\9)))
  (parse-integer-parameter directive))

(defun parse-parameters (client directive)
  (with-accessors ((control-string control-string)
                   (end end)
                   (parameters parameters))
      directive
    (prog ((required nil))
     next
       (when (>= end (length control-string))
         (error 'end-of-control-string
                :directive directive
                :positions (list end)))
       (unless (parse-parameter client directive
                                (char-upcase (char control-string end)))
         (if required
             (setf parameters
                   (nconc parameters
                          (list (make-instance 'literal-parameter))))
             (return nil)))
       (when (and (< end (length control-string))
                  (char= #\, (char control-string end)))
         (incf end)
         (setf required t)
         (go next)))))

(defmethod parse-modifier ((client standard-client) directive (character (eql #\@)))
  (with-accessors ((end end)
                   (at-sign-p at-sign-p))
      directive
    (when at-sign-p
      (error 'duplicate-modifiers
             :directive directive
             :positions (list end)))
    (setf at-sign-p t)
    (incf end)
    t))

(defmethod parse-modifier ((client standard-client) directive (character (eql #\:)))
  (with-accessors ((end end)
                   (colon-p colon-p))
      directive
    (when colon-p
      (error 'duplicate-modifiers
             :directive directive
             :positions (list end)))
    (setf colon-p t)
    (incf end)
    t))

(defun parse-modifiers (client directive)
  (with-accessors ((control-string control-string)
                   (end end))
      directive
    (tagbody
     next
       (unless (< end (length control-string))
         (error 'end-of-control-string
                :directive directive
                :positions (list end)))
       (when (parse-modifier client directive (char-upcase (char control-string end)))
         (go next)))))

(defmethod parse-directive
    ((client standard-client) (character (eql #\~)) control-string position)
  (let ((directive (make-instance 'directive
                                  :start position
                                  :end (1+ position)
                                  :control-string control-string)))
    (with-accessors ((end end)
                     (suffix-start suffix-start)
                     (directive-character directive-character))
        directive
      (parse-parameters client directive)
      (parse-modifiers client directive)
      (unless (< end (length control-string))
        (error 'end-of-control-string
               :directive directive
               :positions (list end)))
      (setf directive-character (char-upcase (char control-string end)))
      (incf end)
      (setf suffix-start end)
      (parse-suffix client directive directive-character)
      directive)))
