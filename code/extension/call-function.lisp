(in-package #:invistra-extension)

(defclass call-function-directive (invistra:directive)
  ((%function-name :accessor function-name)))

(defmethod invistra:specialize-directive
    ((client extension-client) (char (eql #\`)) directive (end-directive t))
  (change-class directive 'call-function-directive))

(defmethod invistra:parameter-specifications (client (directive call-function-directive))
  (declare (ignore client))
  '((:type (or null character integer) :default nil :rest t)))

(defmethod invistra:parse-suffix
    ((client extension-client) directive (directive-character (eql #\`)))
  (with-accessors ((end invistra:end)
                   (control-string invistra:control-string))
      directive
    (prog ((escape nil))
     next
       (invistra:check-end-of-control-string client directive end)
       (case (char control-string end)
         (#\`
          (incf end)
          (unless escape
            (return nil)))
         (#\\
          (incf end)
          (invistra:check-end-of-control-string client directive end)
          (incf end))
         (#\|
          (setf escape (not escape))
          (incf end))
         (otherwise
          (incf end)))
       (go next))))

(defun char-invert-case (ch)
  (cond ((upper-case-p ch)
         (char-downcase ch))
        ((lower-case-p ch)
         (char-upcase ch))
        (t
         ch)))

(defmethod invistra:check-item-syntax progn ((client extension-client) (directive call-function-directive) global-layout local-layout parent &optional group position)
  (declare (ignore global-layout local-layout parent group position))
  (with-accessors ((control-string invistra:control-string)
                   (start invistra:start)
                   (suffix-start invistra:suffix-start)
                   (end invistra:end)
                   (function-name function-name)
                   (colon-p invistra:colon-p))
      directive
    (loop with package = nil
          with internalp = nil
          with start = suffix-start
          with token = (make-array (- end suffix-start)
                                   :element-type 'character :fill-pointer 0)
          with mode = nil
          with char-case = (ecase (readtable-case *readtable*)
                             (:preserve #'identity)
                             (:upcase #'char-upcase)
                             (:downcase #'char-downcase)
                             (:invert #'char-invert-case))
          for position from suffix-start below (1- end)
          for char = (char control-string position)
          finally (if (or (null package)
                          (eq package *package*))
                      (setf function-name (intern token))
                      (multiple-value-bind (symbol status)
                          (find-symbol token package)
                        (cond ((null symbol)
                               (invistra:signal-no-such-symbol client directive token start (1+ position)))
                              ((and (not internalp)
                                    (not (eq status :external)))
                               (invistra:signal-symbol-not-external client directive symbol start (1+ position)))
                              (t
                               (setf function-name symbol)))))
          do (case mode
               (:escape-single
                (vector-push char token)
                (setf mode nil))
               (:escape-range-single
                (vector-push char token)
                (setf mode :escape-range))
               (:escape-range
                (case char
                  (#\\
                   (setf mode :escape-range-single))
                  (#\|
                   (setf mode nil))
                  (otherwise
                   (vector-push char token))))
               (:package-delimiter
                (case char
                  (#\\
                   (setf mode :escape-single
                         start position))
                  (#\|
                   (setf mode :escape-range
                         start position))
                  (#\:
                   (setf internalp t
                         mode nil))
                  (otherwise
                   (setf mode nil
                         start position)
                   (vector-push (funcall char-case char) token))))
               (otherwise
                (case char
                  (#\\
                   (setf mode :escape-single))
                  (#\|
                   (setf mode :escape-range))
                  (#\:
                   (setf mode :package-delimiter
                         package (find-package token)
                         (fill-pointer token) 0)
                   (when (null package)
                     (invistra:signal-no-such-package client directive token start (1+ position))))
                  (otherwise
                   (vector-push (funcall char-case char) token))))))))

(defmethod invistra:interpret-item (client (directive call-function-directive) &optional parameters)
  (apply (invistra:coerce-function-designator client (function-name directive))
         invistra:*format-output*
         (invistra:pop-argument)
         (invistra:colon-p directive)
         (invistra:at-sign-p directive)
         parameters))

(defmethod invistra:compile-item (client (directive call-function-directive) &optional parameters)
  `((funcall (invistra:coerce-function-designator ,(trinsic:client-form client) ',(function-name directive))
            invistra:*format-output*
             (invistra:pop-argument)
             ,(invistra:colon-p directive)
             ,(invistra:at-sign-p directive)
             ,@parameters)))
