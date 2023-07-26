;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3 Floating-point printers

(in-package #:invistra)

(defun print-float-arg (client func)
  (let ((value (consume-next-argument t)))
    (if (or (complexp value)
            (not (numberp value)))
        (let ((*print-base* 10)
              (*print-escape* nil)
              (*print-readably* nil))
          (incless:write-object client value *destination*))
        (let ((coerced-value (if (floatp value)
                                 value
                                 (coerce value 'single-float))))
          (multiple-value-call func client coerced-value (burger-dybvig-2 coerced-value))))))

(defclass decimal ()
  ((%digits :accessor decimal-digits
            :initarg :digits)
   (%position :accessor decimal-position
              :initarg :position
              :initform 0)))

(defun round-decimal (decimal count)
  (with-accessors ((decimal-digits decimal-digits)
                   (decimal-position decimal-position))
      decimal
    (when (and (< (+ decimal-position count) (length decimal-digits))
               (< -1 (+ decimal-position count) (length decimal-digits))
               (> (aref decimal-digits (+ decimal-position count)) 4))
      (loop for pos = (+ decimal-position count -1)
            for (carry new-digit) = (multiple-value-list (floor (1+ (aref decimal-digits pos)) 10))
            do (setf (aref decimal-digits pos) new-digit)
            when (zerop carry)
              do (loop-finish)
            when (zerop pos)
              do (setf decimal-digits (concatenate 'vector #(1) decimal-digits))
                 (incf decimal-position)
                 (loop-finish)))
    (setf decimal-digits
          (subseq decimal-digits 0 (+ decimal-position (max 0 count))))))

(defun print-decimal (decimal)
  (loop with d-pos = (decimal-position decimal)
        with len = (length (decimal-digits decimal))
        for digit across (decimal-digits decimal)
        for pos from 0
        finally (when (= d-pos len)
                  (write-char #\. *destination*))
        when (= pos d-pos)
          do (write-char #\. *destination*)
        do (write-char (aref *digits* digit) *destination*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.1 ~f Fixed-format floating point.
(define-directive #\f
    f-directive
    nil
    (named-parameters-directive)
    ((w :type (or null integer)
        :default-value nil)
     (d :type (or null integer)
        :defaule-value nil)
     (k :type (or null integer)
        :default-value 0)
     (overflowchar :type (or null character)
                   :default-value nil)
     (padchar :type character
              :default-value #\Space)))

(defparameter *digits* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun print-fixed-arg (client value digits exponent
                        colonp at-signp w d k overflowchar padchar)
  (declare (ignore client colonp))
  (let ((decimal (make-instance 'decimal :digits digits))
        sign
        len)
    (with-accessors ((decimal-digits decimal-digits)
                     (decimal-position decimal-position))
        decimal
      (setf sign
            (cond ((minusp (float-sign value)) #\-)
                  ((and at-signp (plusp value)) #\+)))
      (incf exponent k)
      (cond ((zerop (aref decimal-digits 0))
             (setf decimal-position 1))
            ((not (plusp exponent))
             (setf decimal-position 0
                   decimal-digits (concatenate 'vector
                                               (make-array (- exponent) :initial-element 0)
                                               decimal-digits)))
            ((<= exponent (length decimal-digits))
             (setf decimal-position exponent))
            (t
             (setf decimal-digits (concatenate 'vector
                                               decimal-digits
                                               (make-array (- exponent (length decimal-digits))
                                                           :initial-element 0))
                   decimal-position exponent)))
      (setf len (+ (if sign 2 1)
                   (length decimal-digits)))
      (when (and w
                 (null d)
                 (> len w))
        (round-decimal decimal
                      (min (- (length decimal-digits) decimal-position)
                           (max 0
                                (- w
                                   decimal-position
                                   (if sign 2 1)))))
        (let ((q (or (find-if #'plusp decimal-digits :start decimal-position :from-end t)
                     decimal-position)))
          (when (< q (1- (length decimal-digits)))
            (setf decimal-digits (subseq decimal-digits 0 q)))))
      (when d
        (let ((l (- (length decimal-digits) decimal-position)))
          (cond ((< l d)
                 (setf decimal-digits
                       (concatenate 'vector
                                    decimal-digits
                                    (make-array (- d l)
                                                :initial-element 0))))
                ((> l d)
                 (round-decimal decimal d)))))
      (setf len (+ (if sign 2 1)
                   (length decimal-digits)))
      (when (and (= decimal-position (length decimal-digits))
                 (null d)
                 (or (zerop (length decimal-digits))
                     (null w)
                     (null overflowchar)
                     (< len w)))
        (setf decimal-digits (concatenate 'vector decimal-digits #(0)))
        (incf len))
      (when (and (zerop decimal-position)
                 (or (zerop (length decimal-digits))
                     (and
                      (< value (expt 10 (- k)))
                      (or (null w) (null d)
                          (> w (1+ d)))
                      (or (= decimal-position (length decimal-digits))
                          (null w)
                          (< len w)))))
        (setf decimal-digits (concatenate 'vector #(0) decimal-digits))
        (incf decimal-position)
        (incf len))
      (cond ((or (null w)
                 (null overflowchar)
                 (<= len w))
             (when w
               (loop repeat (max 0 (- w len))
                     do (write-char padchar *destination*)))
             (when sign
               (write-char sign *destination*))
             (print-decimal decimal)
             nil)
            (t
             (loop repeat w
                   do (write-char overflowchar *destination*))
             t)))))

(define-format-directive-interpreter f-directive
  (print-float-arg client
                   (lambda (client value digits exponent)
                     (print-fixed-arg client value digits exponent
                                      colonp at-signp w d k overflowchar padchar))))

(define-format-directive-compiler f-directive
  `((print-float-arg ,(incless:client-form client)
                     (lambda (client value digits exponent)
                       (print-fixed-arg client value digits exponent
                                        ,colonp ,at-signp w d k overflowchar padchar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.2 ~e Exponential floating point.

(define-directive #\e
    e-directive
    nil
    (named-parameters-directive)
    ((w :type (or null integer)
        :default-value nil)
     (d :type (or null integer)
        :defaule-value nil)
     (e :type (or null integer)
        :defaule-value nil)
     (k :type (or null integer)
        :default-value 1)
     (overflowchar :type (or null character)
                   :default-value nil)
     (padchar :type character
              :default-value #\Space)
     (exponentchar :type (or null character)
                   :default-value nil)))

(defun print-exponent-arg (client value digits exponent colonp at-signp w d e k overflowchar padchar exponentchar)
  (let ((decimal (make-instance 'decimal :digits digits))
        sign
        len exp)
    (with-accessors ((decimal-digits decimal-digits)
                     (decimal-position decimal-position))
        decimal
      (setf sign
            (cond ((minusp (float-sign value)) #\-)
                  ((and at-signp (plusp value)) #\+)))
      (setf exponent (if (zerop (aref decimal-digits 0))
                         0
                         (+ exponent (- k))))
      (setf exp (let ((*print-base* 10)
                      (*print-radix* nil)
                      (*print-escape* nil)
                      (*print-readably* nil))
                  (with-output-to-string (stream)
                    (incless:write-object client (abs exponent) stream))))
      (when (and e (< (length exp) e))
        (setf exp (concatenate 'string
                               (make-string (- e (length exp)) :initial-element #\0)
                               exp)))
      (cond ((minusp k)
             (setf decimal-digits (concatenate 'vector
                                               (make-array (- k) :initial-element 0)
                                               decimal-digits)
                   decimal-position 0))
            ((< (length decimal-digits) k)
             (setf decimal-digits
                   (concatenate 'vector decimal-digits
                                (make-array (- k (length decimal-digits)) :initial-element 0))
                   decimal-position (length decimal-digits)))
            (t
             (setf decimal-position k)))
      (when d
        (let ((l (- (length decimal-digits) decimal-position))
              (dp (cond ((zerop k)
                         d)
                        ((plusp k)
                         (- d k -1))
                        (t
                         (+ d k 1)))))
          (cond ((< l dp)
                 (setf decimal-digits
                       (concatenate 'vector
                                    decimal-digits
                                    (make-array (- dp l)
                                                :initial-element 0))))
                ((> l dp)
                 (round-decimal decimal dp)))))
      (setf len (+ (if sign 4 3)
                   (length exp)
                   (length decimal-digits)))
      (when (and w
                 (null d)
                 (> len w))
        (round-decimal decimal
                      (- w
                         (length decimal-digits)
                         (if sign 4 3)))
        (setf len (+ (if sign 4 3)
                     (length decimal-digits))))
      (when (and (= decimal-position (length decimal-digits))
                 (null d)
                 (or (null w)
                     (< len w)
                     #+(or)(null d)
                     #+(or)(> w (1+ d))))
        (setf decimal-digits (concatenate 'vector decimal-digits #(0)))
        (incf len))
      (when (and (zerop decimal-position)
                 (or (= decimal-position (length decimal-digits))
                     (null w)
                     (< len w)))
        (setf decimal-digits (concatenate 'vector #(0) decimal-digits))
        (incf decimal-position)
        (incf len))
      (cond ((or (null w)
                 (null overflowchar)
                 (<= len w))
             (when w
               (loop repeat (max 0 (- w len))
                     do (write-char padchar *destination*)))
             (when sign
               (write-char sign *destination*))
             (print-decimal decimal)
             (write-char (or exponentchar
                             (if (typep value *read-default-float-format*)
                                 #\e
                                 (etypecase value
                                   (short-float #\s)
                                   (single-float #\f)
                                   (double-float #\d)
                                   (long-float #\l))))
                         *destination*)
             (write-char (if (minusp exponent) #\- #\+) *destination*)
             (write-string exp *destination*))
            (t
             (loop repeat w
                   do (write-char overflowchar *destination*)))))))

(define-format-directive-interpreter e-directive
  (print-float-arg client
                   (lambda (client value digits exponent)
                     (print-exponent-arg client value digits exponent
                                         colonp at-signp w d e k
                                         overflowchar padchar exponentchar))))

(define-format-directive-compiler e-directive
  `((print-float-arg ,(incless:client-form client)
                     (lambda (client value digits exponent)
                       (print-exponent-arg client value digits exponent
                                           ,colonp ,at-signp w d e k
                                           overflowchar padchar exponentchar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.3 ~g General floating point.

(define-directive #\g
    g-directive
    nil
    (named-parameters-directive)
    ((w :type (or null integer)
        :default-value nil)
     (d :type (or null integer)
        :defaule-value nil)
     (e :type (or null integer)
        :defaule-value nil)
     (k :type (or null integer)
        :default-value 1)
     (overflowchar :type (or null character)
                   :default-value nil)
     (padchar :type character
              :default-value #\Space)
     (exponentchar :type (or null character)
                   :default-value nil)))

(defun print-general-arg (client value digits exponent
                          colonp at-signp w d e k
                          overflowchar padchar exponentchar)
  (unless d
    (let ((q (length (if (minusp exponent)
                         (- (length digits) exponent)
                         (max (length digits) exponent)))))
      (setq d (max q (min exponent 7)))))
  (let* ((ee (if e (+ e 2) 4))
         (ww (if w (- w ee) nil))
         (dd (- d exponent)))
    (cond ((<= 0 dd d)
           (let ((char (if (print-fixed-arg client value digits exponent
                                            colonp at-signp ww dd 0
                                            overflowchar padchar)
                           overflowchar
                           #\space)))
             (dotimes (i ee) (write-char char *destination*))))
          (t
           (print-exponent-arg client value digits exponent
                               colonp at-signp w d e k
                               overflowchar padchar exponentchar)))))

(define-format-directive-interpreter g-directive
  (print-float-arg client
                   (lambda (client value digits exponent)
                     (print-general-arg client value digits exponent
                                        colonp at-signp w d e k
                                        overflowchar padchar exponentchar))))

(define-format-directive-compiler g-directive
  `((print-float-arg ,(incless:client-form client)
                     (lambda (client value digits exponent)
                       (print-general-arg client value digits exponent
                                          ,colonp ,at-signp w d e k
                                          overflowchar padchar exponentchar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.4 ~$ Monetary floating point.

(define-directive #\$
    monetary-directive
    nil
    (named-parameters-directive)
    ((d :type integer
        :default-value 2)
     (n :type integer
        :default-value 1)
     (w :type (or null integer)
        :default-value nil)
     (padchar :type character
              :default-value #\Space)))

(defun print-monetary-arg (client value digits exponent
                           colonp at-signp d n w padchar)
  (let ((decimal (make-instance 'decimal :digits digits))
        sign
        len)
    (with-accessors ((decimal-digits decimal-digits)
                     (decimal-position decimal-position))
        decimal
      (setf sign
            (cond ((minusp (float-sign value)) #\-)
                  ((and at-signp (plusp value)) #\+)))
      (cond ((zerop (aref decimal-digits 0))
             (setf decimal-position 1))
            ((not (plusp exponent))
             (setf decimal-position 0
                   decimal-digits (concatenate 'vector
                                               (make-array (- exponent) :initial-element 0)
                                               decimal-digits)))
            ((<= exponent (length decimal-digits))
             (setf decimal-position exponent))
            (t
             (setf decimal-digits (concatenate 'vector
                                               decimal-digits
                                               (make-array (- exponent) :initial-element 0))
                   decimal-position exponent)))
      (let ((l (- (length decimal-digits) decimal-position)))
        (cond ((< l d)
               (setf decimal-digits
                     (concatenate 'vector
                                  decimal-digits
                                  (make-array (- d l)
                                              :initial-element 0))))
              ((> l d)
               (round-decimal decimal (1- d)))))
      (when (< decimal-position n)
        (setf decimal-digits (concatenate 'vector
                                          (make-array (- n decimal-position)
                                                      :initial-element 0)
                                          decimal-digits)
              decimal-position n))
      (setf len (+ (if sign 2 1)
                   (length decimal-digits)))
      (cond ((> len (if w (max w 100) 100))
             (print-exponent-arg client value digits exponent
                                 colonp at-signp w (+ d n -1) nil 1
                                 #\Space padchar nil))
            (t
             (when (and colonp sign)
               (write-char sign *destination*))
             (when w
               (loop repeat (max 0 (- w len))
                     do (write-char padchar *destination*)))
             (when (and (not colonp) sign)
               (write-char sign *destination*))
             (print-decimal decimal))))))

(define-format-directive-interpreter monetary-directive
  (print-float-arg client
                   (lambda (client value digits exponent)
                     (print-monetary-arg client value digits exponent
                                        colonp at-signp d n w padchar))))

(define-format-directive-compiler monetary-directive
  `((print-float-arg ,(incless:client-form client)
                     (lambda (client value digits exponent)
                       (print-monetary-arg ,client value digits exponent
                                           ,colonp ,at-signp d n w padchar)))))
