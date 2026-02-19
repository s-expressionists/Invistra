;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6 Layout control

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.1 ~TAB Tabulate

(defclass tabulate-directive (directive) ())

(defmethod specialize-directive
    ((client standard-client) (char (eql #\T)) directive (end-directive t))
  (change-class directive 'tabulate-directive))

(defmethod parameter-specifications
    ((client t) (directive tabulate-directive))
  '((:name colnum
     :type (integer 0)
     :bind nil
     :default 1)
    (:name colinc
     :type (integer 0)
     :bind nil
     :default 1)))

(defmethod layout-requirements ((item tabulate-directive))
  (when (colon-p item)
    (list :logical-block)))

(defun format-relative-tab (client colnum colinc)
  (if #+sicl nil #-sicl (inravina:pretty-stream-p client *format-output*)
      #+sicl nil #-sicl (inravina:pprint-tab client *format-output* :line-relative colnum
                                             colinc)
      (let* ((cur (ngray:stream-line-column *format-output*)))
        (ngray:stream-advance-to-column *format-output*
                                        (if (and cur (plusp colinc))
                                            (* (ceiling (+ cur colnum) colinc) colinc)
                                            colnum)))))

(defun format-absolute-tab (client colnum colinc)
  (if #+sicl nil #-sicl (inravina:pretty-stream-p client *format-output*)
      #+sicl nil #-sicl (inravina:pprint-tab client *format-output* :line colnum colinc)
      (let ((cur (ngray:stream-line-column *format-output*)))
        (cond ((null cur)
               (write-string "  " *format-output*))
              ((< cur colnum)
               (ngray:stream-advance-to-column *format-output* colnum))
              ((plusp colinc)
               (ngray:stream-advance-to-column *format-output*
                                               (+ cur
                                                  (- colinc (rem (- cur colnum) colinc)))))))))

(defmethod interpret-item
    ((client standard-client) (directive tabulate-directive) &optional parameters)
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (cond (colon-p
           #-sicl
           (apply #'inravina:pprint-tab
                  client *format-output*
                  (if at-sign-p :section-relative :section)
                  parameters))
          (at-sign-p
           (apply #'format-relative-tab client parameters))
          (t
           (apply #'format-absolute-tab client parameters)))))

(defmethod compile-item
    ((client standard-client) (directive tabulate-directive) &optional parameters)
  (with-accessors ((colon-p colon-p)
                   (at-sign-p at-sign-p))
      directive
    (cond (colon-p
           #-sicl
           `((inravina:pprint-tab ,(trinsic:client-form client) *format-output*
                                  ,(if at-sign-p :section-relative :section)
                                  ,@parameters)))
          (at-sign-p
           `((format-relative-tab ,(trinsic:client-form client) ,@parameters)))
          (t
           `((format-absolute-tab ,(trinsic:client-form client) ,@parameters))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.3 ~> End of justification or of logical block

(defclass end-justification-directive
    (directive end-structured-directive-mixin no-modifiers-mixin) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\>)) directive (end-directive t))
  (if (colon-p directive)
      (change-class directive 'end-logical-block-directive)
      (change-class directive 'end-justification-directive)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.2 ~< Justification

(defclass justification-directive
    (directive structured-directive-mixin) nil)

(defmethod specialize-directive
    ((client standard-client) (char (eql #\<)) directive
     (end-directive end-justification-directive))
  (change-class directive 'justification-directive))

(defmethod parameter-specifications
    ((client t) (directive justification-directive))
  '((:name mincol
     :type integer
     :default 0)
    (:name colinc
     :type (integer 0)
     :default 1)
    (:name minpad
     :type integer
     :default 0)
    (:name padchar
     :type character
     :default #\Space)))

(defmethod layout-requirements :around ((item justification-directive))
  (merge-layout-requirements (list (if (colon-p (aref (aref (clauses item) 0)
                                                      (1- (length (aref (clauses item) 0)))))
                                       :justify-dynamic
                                       :justify))
                             (call-next-method)
                             t))

(defmethod calculate-argument-position (position (directive justification-directive))
  nil
  #+(or)(reduce (lambda (position clauses)
            (reduce #'calculate-argument-position clauses :initial-value position))
          (clauses directive)
          :initial-value (call-next-method)))

(defun str-line-length (stream)
  (or *print-right-margin*
      #+gray-streams-line-length (ngray:stream-line-length stream)
      100))

(defun print-justification (client pad-left pad-right extra-space line-len
                            newline-segment segments
                            mincol colinc minpad padchar)
  (declare (ignore client))
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
                     total-length (or extra-space 0))
                  (or line-len
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
        (write-padding nil)))))

(defmethod interpret-item
    ((client standard-client) (directive justification-directive) &optional parameters)
  (loop with newline-segment = nil
        with *extra-space* = nil
        with *line-length* = nil
        for clause across (clauses directive)
        for segment = (with-remaining-arguments ()
                        (with-output-to-string (*format-output*)
                          (interpret-items client clause)))
        for index from 0
        finally (apply #'print-justification client
                       (colon-p directive) (at-sign-p directive)
                       *extra-space* *line-length*
                       newline-segment segments
                       parameters)
        while segment
        if (and (zerop index)
                (colon-p (aref clause (1- (length clause)))))
          do (setf newline-segment segment)
        else
          collect segment into segments))

(defmethod compile-item
    ((client standard-client) (directive justification-directive) &optional parameters)
  `((let (newline-segment segments
          *extra-space* *line-length*)
      (with-remaining-arguments ()
        ,@(loop for clause across (clauses directive)
                for segment = `(with-output-to-string (*format-output*)
                                 ,@(compile-items client clause))
                for index from 0
                while segment
                if (and (zerop index)
                        (colon-p (aref clause (1- (length clause)))))
                  collect `(setf newline-segment ,segment)
                else
                  collect `(push ,segment segments)))
      (print-justification ,(trinsic:client-form client)
                           ,(colon-p directive) ,(at-sign-p directive)
                           *extra-space* *line-length*
                           newline-segment (nreverse segments)
                           ,@parameters))))
