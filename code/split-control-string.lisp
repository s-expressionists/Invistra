(cl:in-package #:invistra)

;;; Split a control string into its components.  Each component is
;;; either a string to be printed as it is, or a directive.  The list
;;; of components will never contain two consecutive strings.
(defun split-control-string (client control-string)
  (prog ((start 0)
         (end 0)
         (position 0)
         (items nil)
         (directive nil))
   next
     (when (< position (length control-string))
       (setf end position
             directive (parse-directive client (char control-string position)
                                        control-string position))
       (cond (directive
              (setf position (end directive))
              (when (< start end)
                (push (subseq control-string start end) items))
              (push directive items)
              (setf start position
                    end position))
             (t
              (incf position)
              (setf end position)))
       (go next))
     (when (< start end)
       (push (subseq control-string start end) items))
     (return (nreverse items))))
