;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6 Layout control

(in-package #:invistra)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.1 ~TAB Tabulate

(define-directive #\t tabulate-directive nil (named-parameters-directive)
    ((colnum :type (integer 0) :default-value 1)
     (colinc :type (integer 0) :default-value 1)))

(defmethod layout-requirements ((item tabulate-directive))
  (when (colonp item)
    (list :logical-block)))

(defun format-relative-tab (client colnum colinc)
  (if #+sicl nil #-sicl (inravina:pretty-stream-p client *destination*)
      #+sicl nil #-sicl (inravina:pprint-tab client *destination* :line-relative colnum colinc)
      (let* ((cur (trivial-stream-column:line-column *destination*)))
        (trivial-stream-column:advance-to-column (if (and cur (plusp colinc))
                                                     (* (ceiling (+ cur colnum) colinc) colinc)
                                                     colnum)
                                                 *destination*))))

(defun format-absolute-tab (client colnum colinc)
  (if #+sicl nil #-sicl (inravina:pretty-stream-p client *destination*)
      #+sicl nil #-sicl (inravina:pprint-tab client *destination* :line colnum colinc)
      (let ((cur (trivial-stream-column:line-column *destination*)))
        (cond ((null cur)
               (write-string "  " *destination*))
              ((< cur colnum)
               (trivial-stream-column:advance-to-column colnum *destination*))
              ((plusp colinc)
               (trivial-stream-column:advance-to-column (+ cur (- colinc (rem (- cur colnum) colinc)))
                                                        *destination*))))))

(define-format-directive-interpreter tabulate-directive
  (cond (colonp
         #-sicl
         (inravina:pprint-tab client *destination*
                              (if at-signp :section-relative :section)
                              colnum colinc))
        (at-signp
         (format-relative-tab client colnum colinc))
        (t
         (format-absolute-tab client colnum colinc))))

(define-format-directive-compiler tabulate-directive
  (cond (colonp
         #-sicl
         `((inravina:pprint-tab ,(incless:client-form client) *destination*
                                ,(if at-signp :section-relative :section)
                                colnum colinc)))
        (at-signp
         `((format-relative-tab ,(incless:client-form client) colnum colinc)))
        (t
         `((format-absolute-tab ,(incless:client-form client) colnum colinc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.3 ~> End of justification or of logical block

(define-directive #\>
    end-justification-directive
    nil
    (named-parameters-directive end-structured-directive-mixin)
    ())

(defmethod check-directive-syntax progn ((directive end-justification-directive))
  (cond ((colonp directive)
         (change-class directive 'end-logical-block-directive))
        ((at-signp directive)
         (error "wibble"))))

(define-format-directive-interpreter end-justification-directive
    ;; do nothing
    nil)

(define-format-directive-compiler end-justification-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.2 ~< Justification

(define-directive #\<
    justification-directive
    end-justification-directive
    (named-parameters-directive structured-directive-mixin)
    ((mincol :type integer :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type integer :default-value 0)
     (padchar :type character :default-value #\Space)))

(defmethod layout-requirements :around ((item justification-directive))
  (merge-layout-requirements (list (if (colonp (aref (aref (clauses item) 0) (1- (length (aref (clauses item) 0)))))
                                       :justify-dynamic
                                       :justify))
                             (call-next-method)
                             t))

(defun str-line-length (stream)
  (or *print-right-margin*
      (trivial-stream-column:line-length stream)
      100))

(defun print-justification (client pad-left pad-right extra-space line-len
                            mincol colinc minpad padchar
                            newline-segment segments)
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
               (> (+ (or (trivial-stream-column:line-column *destination*) 0)
                     total-length (or extra-space 0))
                  (or line-len
                      (str-line-length *destination*))))
      (write-string newline-segment *destination*))
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
               (dotimes (i pad-len) (write-char padchar *destination*)))))
      (when pad-left
        (write-padding nil))
      (when segments
        (write-string (car segments) *destination*)
        (dolist (segment (cdr segments))
          (write-padding t)
          (write-string segment *destination*)))
      (when pad-right
        (write-padding nil)))))

(define-format-directive-interpreter justification-directive
  (loop with newline-segment = nil
        with *extra-space* = nil
        with *line-length* = nil
        for clause across (clauses directive)
        for segment = (catch *inner-tag*
                        (with-output-to-string (*destination*)
                          (interpret-items client clause)))
        for index from 0
        finally (print-justification client
                                     colonp at-signp *extra-space* *line-length*
                                     mincol colinc minpad padchar
                                     newline-segment segments)
        while segment
        if (and (zerop index)
                (colonp (aref clause (1- (length clause)))))
          do (setf newline-segment segment)
        else
          collect segment into segments))

(define-format-directive-compiler justification-directive
  `((prog (newline-segment segments
           *extra-space* *line-length*)
       ,@(loop for clause across (clauses directive)
               for segment = `(catch *inner-tag*
                                (with-output-to-string (*destination*)
                                  ,@(compile-items client clause)))
               for index from 0
               while segment
               if (and (zerop index)
                       (colonp (aref clause (1- (length clause)))))
                 collect `(let ((segment ,segment))
                            (if segment
                                (setf newline-segment segment)
                                (go end)))
               else
                 collect `(let ((segment ,segment))
                            (if segment
                                (push segment segments)
                                (go end))))
     end
       (print-justification ,(incless:client-form client)
                            ,colonp ,at-signp *extra-space* *line-length*
                            mincol colinc minpad padchar
                            newline-segment (nreverse segments)))))
