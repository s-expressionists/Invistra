;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3 Floating-point printers

(in-package #:invistra)

(defun print-float-arg (client func)
  (let ((value (pop-argument)))
    (if (or (complexp value)
            (and (floatp value)
                 #+abcl
                 (or (system:float-infinity-p value)
                     (system:float-nan-p value))
                 #+allegro
                 (or (excl:infinityp value)
                     (excl:nanp value))
                 #+ccl
                 (ccl::nan-or-infinity-p value)
                 #+(or clasp cmucl ecl)
                 (or (ext:float-infinity-p value)
                     (ext:float-nan-p value))
                 #+mezzano
                 (or (mezzano.extensions:float-infinity-p value)
                     (mezzano.extensions:float-nan-p value))
                 #+sbcl (or (sb-ext:float-infinity-p value)
                            (sb-ext:float-nan-p value)))
            (not (numberp value)))
        (let ((*print-base* 10)
              (*print-escape* nil)
              (*print-readably* nil))
          (incless:write-object client value *destination*))
        (let ((coerced-value (if (floatp value)
                                 value
                                 (coerce value 'single-float))))
          (multiple-value-call func
            client coerced-value
            (quaviver:float-triple client 10 coerced-value))))))

(defun round-away-from-zero (x n)
  (multiple-value-bind (q r)
      (truncate x n)
    (if (>= (* 2 r) n)
        (1+ q)
        q)))

(defun trim-fractional (significand digit-count fractional-position d)
  (let ((l (max 0 (- digit-count fractional-position))))
    (cond ((< l d)
           (if (zerop significand)
               (setf fractional-position (- d))
               (setf significand
                     (* significand
                        (expt 10
                              (+ (max 0
                                      (- fractional-position digit-count))
                                 (- d l))))
                     digit-count (quaviver.math:count-digits 10 significand))))
          ((> l d)
           (when (minusp fractional-position)
             (setf fractional-position
                   (max fractional-position (- 1 d))))
           (setf significand (round-away-from-zero significand
                                                   (expt 10 (- l d)))
                 digit-count (quaviver.math:count-digits 10 significand)))))
  (values significand digit-count fractional-position))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.1 ~f Fixed-format floating point.

(defclass f-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\F)) directive (end-directive t))
  (change-class directive 'f-directive))

(defmethod parameter-specifications ((client t) (directive f-directive))
  '((:name w :type (or null integer) :default nil)
    (:name d :type (or null integer) :default nil)
    (:name k :type (or null integer) :default 0)
    (:name overflowchar :type (or null character) :default nil)
    (:name padchar :type character :default #\Space)))

(defun print-fixed-arg (client value significand exponent sign
                        colon-p at-sign-p w d k overflowchar padchar)
  (declare (ignore client colon-p))
  (let* ((sign-char (cond ((minusp sign) #\-)
                          ((and at-sign-p (plusp sign)) #\+)))
         (digit-count (quaviver.math:count-digits 10 significand))
         (fractional-position (if (zerop significand)
                                  0
                                  (+ digit-count k exponent)))
         (leading-zeros 0)
         (my-significand significand))
    (flet ((compute-width ()
             (+ (if sign-char 2 1)
                leading-zeros
                (max digit-count fractional-position)
                (- (min 0 fractional-position)))))
      (when (and w
                 (null d)
                 (> (compute-width) w))
        (multiple-value-setq (my-significand digit-count fractional-position)
          (trim-fractional my-significand digit-count fractional-position
                           (min (max 0 (- digit-count fractional-position))
                                (max 0
                                     (- w
                                        (max 0 fractional-position)
                                        (if sign-char 2 1))))))
        (when (zerop my-significand)
          (setf fractional-position 1)))
      (when d
        (multiple-value-setq (my-significand digit-count fractional-position)
          (trim-fractional my-significand digit-count fractional-position d)))
      (when (and (>= fractional-position digit-count)
                 (null d)
                 (or (null w)
                     (null overflowchar)
                     (< (compute-width) w)))
        (if (zerop my-significand)
            (decf fractional-position)
            (setf my-significand (* my-significand
                                    (expt 10
                                          (+ fractional-position
                                             (- digit-count)
                                             1)))
                  digit-count (quaviver.math:count-digits 10 my-significand))))
      (when (and (not (plusp fractional-position))
                 (< value (expt 10 (- k)))
                 (or (null w) (null d)
                     (> w (1+ d)))
                 (or (null w)
                     (< (compute-width) w)))
        (setf leading-zeros 1))
      (cond ((or (null w)
                 (null overflowchar)
                 (<= (compute-width) w))
             (when w
               (loop repeat (max 0 (- w (compute-width)))
                     do (write-char padchar *destination*)))
             (when sign-char
               (write-char sign-char *destination*))
             (quaviver:write-digits 10 my-significand *destination*
                                    :leading-zeros leading-zeros
                                    :fractional-position fractional-position
                                    :fractional-marker #\.)
             nil)
            (t
             (loop repeat w
                   do (write-char overflowchar *destination*))
             t)))))

(defmethod interpret-item (client (directive f-directive) &optional parameters)
  (print-float-arg client
                   (lambda (client value digits exponent sign)
                     (apply #'print-fixed-arg
                            client value digits exponent sign
                            (colon-p directive) (at-sign-p directive)
                            parameters))))

(defmethod compile-item (client (directive f-directive) &optional parameters)
  `((print-float-arg ,(trinsic:client-form client)
                     (lambda (client value digits exponent sign)
                       (print-fixed-arg client value digits exponent sign
                                        ,(colon-p directive) ,(at-sign-p directive)
                                        ,@parameters)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.2 ~e Exponential floating point.

(defclass e-directive (directive) ())

(defmethod specialize-directive
    ((client t) (char (eql #\E)) directive (end-directive t))
  (change-class directive 'e-directive))

(defmethod parameter-specifications ((client t) (directive e-directive))
  '((:name w :type (or null integer) :default nil)
    (:name d :type (or null integer) :default nil)
    (:name e :type (or null integer) :default nil)
    (:name k :type (or null integer) :default 1)
    (:name overflowchar :type (or null character) :default nil)
    (:name padchar :type character :default #\Space)
    (:name exponentchar :type (or null character) :default nil)))

(defun print-exponent-arg (client value significand exponent sign
                           colon-p at-sign-p w d e k overflowchar padchar exponentchar)
  (declare (ignore client colon-p))
  (let* ((sign-char (cond ((minusp sign) #\-)
                          ((and at-sign-p (plusp sign)) #\+)))
         (digit-count (quaviver.math:count-digits 10 significand))
         (fractional-position k)
         (leading-zeros 0)
         (my-significand significand)
         (my-exponent (if (zerop significand)
                          0
                          (+ exponent digit-count (- k))))
         (exp-count (quaviver.math:count-digits 10 (abs my-exponent)))
         (leading-exp-zeros (- (or e exp-count) exp-count)))
    (flet ((compute-width ()
             (+ (if sign-char 4 3)
                leading-zeros
                (max digit-count fractional-position)
                (- (min 0 fractional-position))
                leading-exp-zeros
                exp-count)))
      (when d
        (multiple-value-setq (my-significand digit-count fractional-position)
          (trim-fractional my-significand digit-count fractional-position
                           (cond ((zerop k)
                                  d)
                                 ((plusp k)
                                  (- d k -1))
                                 (t
                                  (+ d k 1))))))
      (when (and w
                 (null d)
                 (> (compute-width) w))
        (multiple-value-setq (my-significand digit-count fractional-position)
          (trim-fractional my-significand digit-count fractional-position
                           (max 0
                                (- w
                                   (max 0 fractional-position)
                                   (if sign-char 4 3)
                                   exp-count)))))
      (when (and (= fractional-position digit-count)
                 (null d)
                 (or (null w)
                     (< (compute-width) w)
                     #+(or)(null d)
                     #+(or)(> w (1+ d))))
        (if (zerop significand)
            (setf fractional-position 0)
            (setf my-significand (* 10 my-significand)
                  digit-count (1+ digit-count))))
      (when (and (not (plusp fractional-position))
                 (or (null w)
                     (< (compute-width) w)))
        (setf leading-zeros 1))
      (cond ((or (null w)
                 (null overflowchar)
                 (<= (compute-width) w))
             (when w
               (loop repeat (max 0 (- w (compute-width)))
                     do (write-char padchar *destination*)))
             (when sign-char
               (write-char sign-char *destination*))
             (quaviver:write-digits 10 my-significand *destination*
                                    :leading-zeros leading-zeros
                                    :fractional-position fractional-position
                                    :fractional-marker #\.)
             (write-char (or exponentchar
                             (if (typep value *read-default-float-format*)
                                 #+abcl #\E #-abcl #\e
                                 (etypecase value
                                   (short-float #+abcl #\S #-abcl #\s)
                                   (single-float #+abcl #\F #-abcl #\f)
                                   (double-float #+abcl #\D #-abcl #\d)
                                   (long-float #+abcl #\L #-abcl #\l))))
                         *destination*)
             (write-char (if (minusp my-exponent) #\- #\+) *destination*)
             (quaviver:write-digits 10 (abs my-exponent) *destination*
                                    :leading-zeros leading-exp-zeros))
            (t
             (loop repeat w
                   do (write-char overflowchar *destination*)))))))

(defmethod interpret-item (client (directive e-directive) &optional parameters)
  (print-float-arg client
                   (lambda (client value digits exponent sign)
                     (apply #'print-exponent-arg
                            client value digits exponent sign
                            (colon-p directive) (at-sign-p directive)
                            parameters))))

(defmethod compile-item (client (directive e-directive) &optional parameters)
  `((print-float-arg ,(trinsic:client-form client)
                     (lambda (client value digits exponent sign)
                       (print-exponent-arg client value digits exponent sign
                                           ,(colon-p directive) ,(at-sign-p directive)
                                           ,@parameters)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.3 ~g General floating point.

(defclass g-directive (directive) ())

(defmethod specialize-directive
    ((client t) (char (eql #\G)) directive (end-directive t))
  (change-class directive 'g-directive))

(defmethod parameter-specifications ((client t) (directive g-directive))
  '((:name w :type (or null integer) :default nil)
    (:name d :type (or null integer) :default nil)
    (:name e :type (or null integer) :default nil)
    (:name k :type (or null integer) :default 1)
    (:name overflowchar :type (or null character) :default nil)
    (:name padchar :type character :default #\Space)
    (:name exponentchar :type (or null character) :default nil)))

(defun print-general-arg (client value significand exponent sign
                          colon-p at-sign-p w d e k
                          overflowchar padchar exponentchar)
  (unless d
    (let ((q (if (minusp exponent)
                 (- (quaviver.math:count-digits 10 significand) exponent)
                 (max (quaviver.math:count-digits 10 significand) exponent))))
      (setq d (max q (min exponent 7)))))
  (let* ((ee (if e (+ e 2) 4))
         (ww (if w (- w ee) nil))
         (dd (- d exponent)))
    (cond ((<= 0 dd d)
           (let ((char (if (print-fixed-arg client value significand exponent sign
                                            colon-p at-sign-p ww dd 0
                                            overflowchar padchar)
                           overflowchar
                           #\space)))
             (dotimes (i ee) (write-char char *destination*))))
          (t
           (print-exponent-arg client value significand exponent sign
                               colon-p at-sign-p w d e k
                               overflowchar padchar exponentchar)))))

(defmethod interpret-item (client (directive g-directive) &optional parameters)
  (print-float-arg client
                   (lambda (client value significand exponent sign)
                     (apply #'print-general-arg
                            client value significand exponent sign
                            (colon-p directive) (at-sign-p directive)
                            parameters))))

(defmethod compile-item (client (directive g-directive) &optional parameters)
  `((print-float-arg ,(trinsic:client-form client)
                     (lambda (client value significand exponent sign)
                       (print-general-arg client value significand exponent sign
                                          ,(colon-p directive) ,(at-sign-p directive)
                                          ,@parameters)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.4 ~$ Monetary floating point.

(defclass monetary-directive (directive) nil)

(defmethod specialize-directive
    ((client t) (char (eql #\$)) directive (end-directive t))
  (change-class directive 'monetary-directive))

(defmethod parameter-specifications
    ((client t) (directive monetary-directive))
  '((:name d :type integer :default 2)
    (:name n :type integer :default 1)
    (:name w :type (or null integer) :default nil)
    (:name padchar :type character :default #\Space)))

(defun print-monetary-arg (client value significand exponent sign
                           colon-p at-sign-p d n w padchar)
  (let* ((sign-char (cond ((minusp sign) #\-)
                          ((and at-sign-p (plusp sign)) #\+)))
         (digit-count (quaviver.math:count-digits 10 significand))
         (fractional-position (if (zerop significand) 1 (+ digit-count exponent)))
         (leading-zeros 0)
         (my-significand significand))
    (flet ((compute-width ()
             (+ (if sign-char 2 1)
                leading-zeros
                digit-count)))
      (multiple-value-setq (my-significand digit-count fractional-position)
        (trim-fractional my-significand digit-count fractional-position d))
      (setf leading-zeros (max 0 (- n (max 0 fractional-position))))
      (cond ((> (compute-width) (if w (max w 100) 100))
             (print-exponent-arg client value significand exponent sign
                                 colon-p at-sign-p w (+ d n -1) nil 1
                                 #\Space padchar nil))
            (t
             (when (and colon-p sign-char)
               (write-char sign-char *destination*))
             (when w
               (loop repeat (max 0 (- w (compute-width)))
                     do (write-char padchar *destination*)))
             (when (and (not colon-p) sign-char)
               (write-char sign-char *destination*))
             (quaviver:write-digits 10 my-significand *destination*
                                    :leading-zeros leading-zeros
                                    :fractional-position fractional-position
                                    :fractional-marker #\.))))))

(defmethod interpret-item (client (directive monetary-directive) &optional parameters)
  (print-float-arg client
                   (lambda (client value digits exponent sign)
                     (apply #'print-monetary-arg
                            client value digits exponent sign
                            (colon-p directive) (at-sign-p directive)
                            parameters))))

(defmethod compile-item (client (directive monetary-directive) &optional parameters)
  `((print-float-arg ,(trinsic:client-form client)
                     (lambda (client value digits exponent sign)
                       (print-monetary-arg client value digits exponent sign
                                           ,(colon-p directive) ,(at-sign-p directive)
                                           ,@parameters)))))
