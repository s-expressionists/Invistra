(in-package #:invistra-extension)

(defclass call-function-directive (invistra:directive)
  ((%function-name :accessor function-name)))

(defmethod invistra:specialize-directive
    ((client extension-client) (char (eql #\`)) directive (end-directive t))
  (change-class directive 'call-function-directive))

(defmethod invistra:parameter-specifications (client (directive call-function-directive))
  (declare (ignore client))
  '((:type (or null character integer) :default nil :rest t)))

(defmethod invistra:parse-directive-suffix
    ((client extension-client) (directive-character (eql #\`)) control-string start end)
  (prog ((position start)
         (escape nil))
   next
     (when (= position end)
       (error 'invistra::end-of-control-string-error
              :control-string control-string
              :tilde-position start
              :why "expected a trailing backquote"))
     (case (char control-string position)
       (#\`
        (if escape
            (incf position)
            (return (1+ position))))
       (#\\
        (when (= (1+ start) end)
          (error "EOF"))
        (incf position 2))
       (#\|
        (setf escape (not escape))
        (incf position))
       (otherwise
        (incf position)))
     (go next)))

(defun char-invert-case (ch)
  (cond ((upper-case-p ch)
         (char-downcase ch))
        ((lower-case-p ch)
         (char-upcase ch))
        (t
         ch)))

(defmethod invistra:check-directive-syntax progn (client (directive call-function-directive))
  (declare (ignore client))
  (with-accessors ((control-string invistra::control-string)
                   (start invistra::start)
                   (suffix-start invistra::suffix-start)
                   (end invistra::end)
                   (function-name function-name)
                   (colon-p invistra::colon-p))
      directive
    (loop with package = nil
          with internalp = nil
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
          finally (print token)
                  (if (or (null package)
                          (eq package *package*))
                      (setf function-name (intern token))
                      (multiple-value-bind (symbol status)
                          (find-symbol token package)
                        (cond ((null symbol)
                               (error "Symbol not found ~a" token))
                              ((and (not internalp)
                                    (not (eq status :external)))
                               (error "Not exported"))
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
                   (setf mode :escape-single))
                  (#\|
                   (setf mode :escape-range))
                  (#\:
                   (setf internalp t
                         mode nil))
                  (otherwise
                   (setf mode nil)
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
                     (error 'invistra::no-such-package
                            :directive directive)))
                  (otherwise
                   (vector-push (funcall char-case char) token))))))))

(defmethod invistra::interpret-item (client (directive call-function-directive) &optional parameters)
  (apply (invistra::coerce-function-designator client (function-name directive))
         invistra::*destination*
         (invistra::pop-argument)
         (invistra::colon-p directive)
         (invistra::at-sign-p directive)
         parameters))

(defmethod invistra::compile-item (client (directive call-function-directive) &optional parameters)
  `((funcall (invistra::coerce-function-designator ,(trinsic:client-form client) ',(function-name directive))
             *destination*
             (invistra::pop-argument)
             ,(invistra::colon-p directive)
             ,(invistra::at-sign-p directive)
             ,@parameters)))
