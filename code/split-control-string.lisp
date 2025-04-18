(cl:in-package #:invistra)

(defmethod parse-control-string-fragment (client control-string start)
  (let ((tilde-position (position #\~ control-string :start start)))
    (cond ((null tilde-position)
           ;; No tilde was found.  The rest of the control string
           ;; is just a string to be printed.
           (values (subseq control-string start)
                   (length control-string)))
          ((> tilde-position start)
           ;; A tilde was found, but it is not in the
           ;; start position.  A prefix of the control
           ;; string is therefore a string to be
           ;; printed.
           (values (subseq control-string start tilde-position)
                   tilde-position))
          (t
           ;; We found a tilde in the start position, so we have
           ;; a directive.
           (multiple-value-bind (directive-character
                                 parameters
                                 colon-p
                                 at-sign-p
                                 suffix-start
                                 end-of-directive-position)
               (parse-format-directive control-string tilde-position)
             (values (make-instance 'directive
                                    :control-string control-string
                                    :start tilde-position
                                    :suffix-start suffix-start
                                    :end end-of-directive-position
                                    :directive-character (char-upcase directive-character)
                                    :parameters parameters
                                    :colon-p colon-p
                                    :at-sign-p at-sign-p)
                     end-of-directive-position))))))

;;; Split a control string into its components.  Each component is
;;; either a string to be printed as it is, or a directive.  The list
;;; of components will never contain two consecutive strings.
(defun split-control-string (client control-string)
  (loop with end = (length control-string)
        for (fragment start) = (multiple-value-list (parse-control-string-fragment client control-string 0))
          then (multiple-value-list (parse-control-string-fragment client control-string start))
        when fragment
          collect fragment into fragments
        unless (< start end)
          return fragments))
