(cl:in-package #:invistra)

(defmethod parse-parameter ((client client) directive (character (eql #\,)))
  (with-accessors ((end end)
                   (parameters parameters))
      directive
    (setf parameters
          (nconc parameters
                 (list (make-instance 'literal-parameter
                                      :start end
                                      :end end))))
    t))

(defmethod parse-parameter ((client client) directive (character (eql #\V)))
  (with-accessors ((end end)
                   (parameters parameters))
      directive
    (setf parameters
          (nconc parameters
                 (list (make-instance 'argument-reference-parameter
                                      :start end
                                      :end (incf end)))))
    t))

(defmethod parse-parameter ((client client) directive (character (eql #\#)))
  (with-accessors ((end end)
                   (parameters parameters))
      directive
    (setf parameters
          (nconc parameters
                 (list (make-instance 'remaining-argument-count-parameter
                                      :start end
                                      :end (incf end)))))
    t))

(defmethod parse-parameter ((client client) directive (character (eql #\')))
  (with-accessors ((control-string control-string)
                   (end end)
                   (parameters parameters))
      directive
    (check-end-of-control-string client directive (incf end))
    (setf parameters
          (nconc parameters
                 (list (make-instance 'literal-parameter
                                      :value (char control-string end)
                                      :start (1- end)
                                      :end (incf end)))))
    t))

(defun parse-integer-parameter (client directive)
  (with-accessors ((control-string control-string)
                   (end end)
                   (parameters parameters))
      directive
    (multiple-value-bind (value end-position)
        (parse-integer control-string :start end :junk-allowed t)
      (when (null value)
        (signal-expected-integer-error client directive end-position))
      (setf parameters
            (nconc parameters
                   (list (make-instance 'literal-parameter :value value
                                                           :start end
                                                           :end end-position)))
            end end-position)
      t)))

(defmethod parse-parameter ((client client) directive (character (eql #\-)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\+)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\0)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\1)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\2)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\3)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\4)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\5)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\6)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\7)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\8)))
  (parse-integer-parameter client directive))

(defmethod parse-parameter ((client client) directive (character (eql #\9)))
  (parse-integer-parameter client directive))

(defun parse-parameters (client directive)
  (with-accessors ((control-string control-string)
                   (end end)
                   (parameters parameters))
      directive
    (tagbody
     next
       (check-end-of-control-string client directive end)
       (when (and (parse-parameter client directive
                                   (char-upcase (char control-string end)))
                  (< end (length control-string))
                  (char= #\, (char control-string end)))
         (incf end)
         (go next)))))

(defmethod parse-modifier ((client client) directive (character (eql #\@)))
  (with-accessors ((end end)
                   (at-sign-p at-sign-p))
      directive
    (incf end)
    (if at-sign-p
        (signal-duplicate-modifiers client directive #\@)
        (setf at-sign-p t))
    t))

(defmethod parse-modifier ((client client) directive (character (eql #\:)))
  (with-accessors ((end end)
                   (colon-p colon-p))
      directive
    (incf end)
    (if colon-p
      (signal-duplicate-modifiers client directive #\:)
      (setf colon-p t))
    t))

(defun parse-modifiers (client directive)
  (with-accessors ((control-string control-string)
                   (end end)
                   (modifiers-start modifiers-start))
      directive
    (tagbody
       (setf modifiers-start end)
     next
       (check-end-of-control-string client directive end)
       (when (parse-modifier client directive (char-upcase (char control-string end)))
         (go next)))))

(defun parse-next-directive (client control-string start)
  (loop for position from start below (length control-string)
        for (next-position directive) = (multiple-value-list (parse-directive client (char control-string position)
                                         control-string position))
        when directive
          do (return (values next-position
                             (when (< start position)
                               (subseq control-string start position))
                             directive))
        finally (return (values (length control-string)
                                (when (< start (length control-string))
                                  (subseq control-string start))
                                nil))))

(defun parse-clauses (client control-string parent)
  (prog ((position (end parent))
         (text nil)
         (directive nil)
         items)
   next
     (setf (values position text directive) (parse-next-directive client control-string position))
     (when text
       (push text items))
     (cond ((null directive))
           ((structured-end-p directive)
            (append-clause client parent (nreverse items) directive)
            (return position))
           ((structured-separator-p directive)
            (append-clause client parent (nreverse items) directive)
            (setf items nil))
           (directive
            (push directive items)))
     (go next)))

(defmethod parse-directive
    ((client client) (character (eql #\~)) control-string position)
  (let ((directive (make-instance 'directive
                                  :start position
                                  :end (1+ position)
                                  :control-string control-string)))
    (with-accessors ((end end)
                     (suffix-start suffix-start)
                     (directive-character directive-character)
                     (character-start character-start))
        directive
      (parse-parameters client directive)
      (parse-modifiers client directive)
      (check-end-of-control-string client directive end)
      (setf directive-character (char-upcase (char control-string end))
            character-start end)
      (incf end)
      (setf suffix-start end)
      (specialize-directive client directive-character directive)
      (cond ((structured-start-p directive)
             (values (parse-clauses client control-string directive)
                     directive))
            (t
             (values (end directive)
                     directive))))))

(defun parse-items (client control-string)
  (prog ((position 0)
         (text nil)
         (directive nil)
         items)
   next
     (when (< position (length control-string))
       (setf (values position text directive) (parse-next-directive client control-string position))
       (when text
         (push text items))
       (when directive
         (push directive items))
       (go next))
     (return (nreverse items))))

(defun parse-control-string (client control-string)
  (loop with items = (parse-items client control-string)
        with global = (make-instance 'layout)
        with local = (make-instance 'layout)
        for item in items
        finally (return items)
        do (check-item-syntax client item global local nil)))
