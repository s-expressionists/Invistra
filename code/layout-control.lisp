;;;; 22.3.6 Layout control

(in-package #:invistra)

;;; 22.3.6.1 ~TAB Tabulate

(defclass tabulate-directive (directive) ())

(defmethod specialize-directive
    ((client client) (char (eql #\T)) directive (end-directive t))
  (change-class directive 'tabulate-directive))

(defmethod parameter-specifications
    ((client client) (directive tabulate-directive))
  '((:name colnum
     :type (integer 0)
     :bind nil
     :default 1)
    (:name colinc
     :type (integer 0)
     :bind nil
     :default 1)))

(defmethod check-item-syntax :around
    ((client client) (directive tabulate-directive) global-layout local-layout parent
     &optional group position)
  (if (colon-p directive)
      (call-next-method client directive global-layout
                        (merge-layout client directive global-layout local-layout
                                      :logical-block t)
                        parent group position)
      (call-next-method)))

(defun format-tab (client colon-p at-sign-p colnum colinc)
  (if (and (not colon-p) #-sicl (not (inravina:pretty-stream-p client *format-output*)))
      (let ((cur (ngray:stream-line-column *format-output*)))
        (cond (at-sign-p
               (ngray:stream-advance-to-column *format-output*
                                               (if (and cur (plusp colinc))
                                                   (* (ceiling (+ cur colnum) colinc) colinc)
                                                   colnum)))
              ((null cur)
               (write-string "  " *format-output*))
              ((< cur colnum)
               (ngray:stream-advance-to-column *format-output* colnum))
              ((plusp colinc)
               (ngray:stream-advance-to-column *format-output*
                                               (+ cur
                                                  (- colinc (rem (- cur colnum) colinc)))))))
      #-sicl
      (inravina:pprint-tab client *format-output*
                           (cond ((and colon-p at-sign-p) :section-relative)
                                 (colon-p :section)
                                 (at-sign-p :line-relative)
                                 (t :line))
                           colnum colinc)))

(define-compiler-macro format-tab (&whole form client colon-p at-sign-p colnum colinc)
  (if (and (constantp colon-p) colon-p)
      `(inravina:pprint-tab ,client *format-output*
                            ,(if (constantp at-sign-p)
                                 (if at-sign-p :section-relative :section)
                                 `(if ,at-sign-p :section-relative :section))
                            ,colnum ,colinc)
      form))

(defmethod interpret-item
    ((client client) (directive tabulate-directive) &optional parameters)
  (apply #'format-tab client (colon-p directive) (at-sign-p directive) parameters))

(defmethod compile-item
    ((client client) (directive tabulate-directive) &optional parameters)
  `((format-tab ,(trinsic:client-form client) ,(colon-p directive) ,(at-sign-p directive)
                ,@parameters)))

;;; 22.3.6.3 ~> End of justification or of logical block

(defclass end-justification-directive
    (directive end-structured-directive-mixin no-modifiers-mixin) nil)

(defmethod specialize-directive
    ((client client) (char (eql #\>)) directive (end-directive t))
  (if (colon-p directive)
      (change-class directive 'end-logical-block-directive)
      (change-class directive 'end-justification-directive)))

;;; 22.3.6.2 ~< Justification

(defclass justification-directive
    (directive structured-directive-mixin) nil)

(defmethod specialize-directive
    ((client client) (char (eql #\<)) directive
     (end-directive end-justification-directive))
  (change-class directive 'justification-directive))

(defmethod parameter-specifications
    ((client client) (directive justification-directive))
  '((:name mincol
     :type integer
     :bind nil
     :default 0)
    (:name colinc
     :type (integer 0)
     :bind nil
     :default 1)
    (:name minpad
     :type integer
     :bind nil
     :default 0)
    (:name padchar
     :type character
     :bind nil
     :default #\Space)))

(defmethod check-item-syntax :around
    ((client client) (directive justification-directive) global-layout local-layout
     parent &optional group position)
  (call-next-method client directive global-layout
                    (merge-layout client directive global-layout local-layout
                                  :justification t
                                  :dynamic (colon-p (aref (aref (clauses directive) 0)
                                                          (1- (length (aref (clauses directive)
                                                                            0))))))
                    parent group position))

(defmethod calculate-argument-position (position (directive justification-directive))
  (reduce (lambda (position clauses)
            (reduce #'calculate-argument-position clauses :initial-value position))
          (clauses directive)
          :initial-value (call-next-method)))

(defun str-line-length (stream)
  (or *print-right-margin*
      #+gray-streams-line-length (ngray:stream-line-length stream)
      100))

(defun format-justification
    (client pad-left pad-right newline-segment-p mincol colinc minpad padchar segments)
  (declare (ignore client))
  (let ((newline-segment (when newline-segment-p
                           (pop segments))))
    (when (and (not pad-left) (not pad-right) (null (cdr segments)))
      (setf pad-left t))
    (let* ((pad-count (1- (length segments)))
           (chars (+ (* pad-count minpad)
                     (loop for segment in segments
                           sum (length segment))))
           (total-length (if (> chars mincol)
                             (+ mincol (* (ceiling (- chars mincol) colinc) colinc))
                             mincol))
           (padding (- total-length chars)))
      (when (and newline-segment
                 (> (+ (or (ngray:stream-line-column *format-output*) 0)
                       total-length (or *extra-space* 0))
                    (or *line-length*
                        (str-line-length *format-output*))))
        (write-string newline-segment *format-output*))
      (when pad-left
        (incf pad-count))
      (when pad-right
        (incf pad-count))
      (when (zerop pad-count)
        (incf pad-count)
        (setf pad-left t))
      (flet ((write-padding (interiorp)
               (let ((pad-len (truncate padding pad-count)))
                 (decf padding pad-len)
                 (decf pad-count)
                 (when interiorp
                   (incf pad-len minpad))
                 (dotimes (i pad-len) (write-char padchar *format-output*)))))
        (when pad-left
          (write-padding nil))
        (when segments
          (write-string (car segments) *format-output*)
          (dolist (segment (cdr segments))
            (write-padding t)
            (write-string segment *format-output*)))
        (when pad-right
          (write-padding nil))))))

(defmethod interpret-item
    ((client client) (directive justification-directive) &optional parameters)
  (with-accessors ((clauses clauses)
                   (colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (let (*extra-space* *line-length*)
      (multiple-value-call #'format-justification
        client colon-p at-sign-p
        (colon-p (aref (aref clauses 0) (1- (length (aref clauses 0)))))
        (values-list parameters)
        (let* ((head (cons nil nil))
               (tail head))
          (declare (dynamic-extent head))
          (with-remaining-arguments ()
            (loop for clause across (clauses directive)
                  do (setf (cdr tail) (cons (with-output-to-string (*format-output*)
                                              (interpret-items client clause))
                                            nil)
                           tail (cdr tail))))
          (cdr head))))))

(defmethod compile-item
    ((client client) (directive justification-directive) &optional parameters)
  (with-unique-names (head tail)
    `((let (*extra-space* *line-length*)
        (format-justification ,(trinsic:client-form client) ,(colon-p directive)
                              ,(at-sign-p directive)
                              ,(colon-p (aref (aref (clauses directive) 0)
                                              (1- (length (aref (clauses directive) 0)))))
                              ,@parameters
                              (let* ((,head (cons nil nil))
                                     (,tail ,head))
                                (declare (dynamic-extent ,head))
                                ,@(with-remaining-arguments-form ()
                                   (loop for clause across (clauses directive)
                                         collect `(setf (cdr ,tail) (cons (with-output-to-string (*format-output*)
                                                                            ,@(compile-items client clause))
                                                                          nil)
                                                        ,tail (cdr ,tail))))
                                (cdr ,head)))))))
