;;;; 22.3.3 Floating-point printers

(in-package #:invistra)

(defun %format-float (client func value)
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
        (incless:write-object client value *format-output*))
      (let ((coerced-value (if (floatp value)
                               value
                               (coerce value 'single-float))))
        (multiple-value-call func
          client coerced-value
          (quaviver:float-triple client 10 coerced-value)))))

(defun round-away-from-zero (client value significand exponent sign n)
  (declare (ignore sign))
  (multiple-value-bind (q r)
      (truncate significand n)
    (let ((d (* 2 r)))
      (cond ((< d n) ; rounding down
             q)
            ((> d 10) ; we are rounding up on a digit that is not a 5 in the ones place.
             (1+ q))
            ((minusp exponent)
             (multiple-value-bind (significand2 exponent2)
                 (quaviver:float-triple client 2 value)
               (if (<= (ash significand (- exponent2))
                       (* significand2 (expt 10 (- exponent))))
                   (1+ q) ; base-10 significand was underestimate so round up
                   q))) ; base-10 significand was overestimate so round down
            (t
             (multiple-value-bind (significand2 exponent2)
                 (quaviver:float-triple client 2 value)
               (if (<= (* significand (expt 10 exponent))
                       (ash significand2 exponent2))
                   (1+ q) ; base-10 significand was underestimate so round up
                   q))))))) ; base-10 significand was overestimate so round down

(defun trim-fractional
    (client value significand exponent sign digit-count fractional-position d)
  (let ((l (max 0 (- digit-count fractional-position))))
    (cond ((= l d)
           (values significand
                   digit-count
                   fractional-position))
          ((> l d)
           (let* ((expected-digit-count (+ digit-count (- l) d))
                  (sig (round-away-from-zero client value significand exponent sign
                                             (expt 10 (- l d))))
                  (dc (quaviver.math:count-digits 10 sig)))
             (when (> dc expected-digit-count)
               (incf l (- dc expected-digit-count))
               (setf sig (round-away-from-zero client value significand exponent sign
                                               (expt 10 (- l d)))
                     dc (quaviver.math:count-digits 10 sig)))
             (values sig
                     dc
                     (if (minusp fractional-position)
                         (max fractional-position (- 1 d))
                         fractional-position))))
          ((zerop significand)
           (values significand
                   digit-count
                   (- d)))
          (t
           (setf significand
                 (* significand
                    (expt 10
                          (+ (max 0
                                  (- fractional-position digit-count))
                             (- d l)))))
           (values significand
                   (quaviver.math:count-digits 10 significand)
                   fractional-position)))))

;;; 22.3.3.1 ~f Fixed-format floating point.

(defclass fixed-format-directive (directive) nil)

(defmethod specialize-directive ((client client) (char (eql #\F)) directive)
  (change-class directive 'fixed-format-directive))

(defmethod parameter-specifications
    ((client client) (directive fixed-format-directive))
  '((:name w
     :type (or null integer)
     :default nil
     :bind nil)
    (:name d
     :type (or null integer)
     :default nil
     :bind nil)
    (:name k
     :type (or null integer)
     :default 0
     :bind nil)
    (:name overflowchar
     :type (or null character)
     :default nil
     :bind nil)
    (:name padchar
     :type character
     :default #\Space
     :bind nil)))

(defmethod traverse-item ((client client) (directive fixed-format-directive))
  (go-to-argument 1))

(defun %format-fixed-format-float
    (client colon-p at-sign-p w d k overflowchar padchar value significand exponent sign)
  (declare (ignore colon-p))
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
          (trim-fractional client value my-significand exponent sign digit-count fractional-position
                           (min (max 0 (- digit-count fractional-position))
                                (max 0
                                     (- w
                                        (max 0 fractional-position)
                                        (if sign-char 2 1))))))
        (when (zerop my-significand)
          (setf fractional-position 1)))
      (when d
        (multiple-value-setq (my-significand digit-count fractional-position)
          (trim-fractional client value my-significand exponent sign digit-count fractional-position d)))
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
                     do (write-char padchar *format-output*)))
             (when sign-char
               (write-char sign-char *format-output*))
             (quaviver:write-digits 10 my-significand *format-output*
                                    :leading-zeros leading-zeros
                                    :fractional-position fractional-position
                                    :fractional-marker #\.)
             nil)
            (t
             (loop repeat w
                   do (write-char overflowchar *format-output*))
             t)))))

(defun format-fixed-format-float (client colon-p at-sign-p w d k overflowchar padchar value)
  (%format-float client
                 (lambda (client value significand exponent sign)
                   (%format-fixed-format-float client colon-p at-sign-p w d k overflowchar
                                               padchar value significand exponent sign))
                 value))

(defmethod interpret-item
    ((client client) (directive fixed-format-directive) &optional parameters)
  (multiple-value-call #'format-fixed-format-float
    client (colon-p directive) (at-sign-p directive) (values-list parameters) (pop-argument)))

(defmethod compile-item
    ((client client) (directive fixed-format-directive) &optional parameters)
  `((format-fixed-format-float ,(trinsic:client-form client) ,(colon-p directive)
                               ,(at-sign-p directive) ,@parameters ,(pop-argument-form))))

;;; 22.3.3.2 ~e Exponential floating point.

(defclass exponential-directive (directive) ())

(defmethod specialize-directive ((client client) (char (eql #\E)) directive)
  (change-class directive 'exponential-directive))

(defmethod parameter-specifications ((client client) (directive exponential-directive))
  '((:name w
     :type (or null integer)
     :default nil
     :bind nil)
    (:name d
     :type (or null integer)
     :default nil
     :bind nil)
    (:name e
     :type (or null integer)
     :default nil
     :bind nil)
    (:name k
     :type (or null integer)
     :default 1
     :bind nil)
    (:name overflowchar
     :type (or null character)
     :default nil
     :bind nil)
    (:name padchar
     :type character
     :default #\Space
     :bind nil)
    (:name exponentchar
     :type (or null character)
     :default nil
     :bind nil)))

(defmethod traverse-item ((client client) (directive exponential-directive))
  (go-to-argument 1))

(defun %format-exponential-float
    (client colon-p at-sign-p w d e k overflowchar padchar exponentchar value significand
     exponent sign)
  (declare (ignore colon-p))
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
          (trim-fractional client value my-significand exponent sign digit-count fractional-position
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
          (trim-fractional client value my-significand exponent sign digit-count fractional-position
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
                     do (write-char padchar *format-output*)))
             (when sign-char
               (write-char sign-char *format-output*))
             (quaviver:write-digits 10 my-significand *format-output*
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
                         *format-output*)
             (write-char (if (minusp my-exponent) #\- #\+) *format-output*)
             (quaviver:write-digits 10 (abs my-exponent) *format-output*
                                    :leading-zeros leading-exp-zeros))
            (t
             (loop repeat w
                   do (write-char overflowchar *format-output*)))))))

(defun format-exponential-float
    (client colon-p at-sign-p w d e k overflowchar padchar exponentchar value)
  (%format-float client
                 (lambda (client value significand exponent sign)
                   (%format-exponential-float client colon-p at-sign-p w d e k overflowchar
                                              padchar exponentchar value significand exponent
                                              sign))
                 value))

(defmethod interpret-item
    ((client client) (directive exponential-directive) &optional parameters)
  (multiple-value-call #'format-exponential-float
    client (colon-p directive) (at-sign-p directive) (values-list parameters) (pop-argument)))

(defmethod compile-item
    ((client client) (directive exponential-directive) &optional parameters)
  `((format-exponential-float ,(trinsic:client-form client) ,(colon-p directive)
                              ,(at-sign-p directive) ,@parameters ,(pop-argument-form))))

;;; 22.3.3.3 ~g General floating point.

(defclass general-directive (directive) ())

(defmethod specialize-directive ((client client) (char (eql #\G)) directive)
  (change-class directive 'general-directive))

(defmethod parameter-specifications ((client client) (directive general-directive))
  '((:name w
     :type (or null integer)
     :default nil
     :bind nil)
    (:name d
     :type (or null integer)
     :default nil
     :bind nil)
    (:name e
     :type (or null integer)
     :default nil
     :bind nil)
    (:name k
     :type (or null integer)
     :default 1
     :bind nil)
    (:name overflowchar
     :type (or null character)
     :default nil
     :bind nil)
    (:name padchar
     :type character
     :default #\Space
     :bind nil)
    (:name exponentchar
     :type (or null character)
     :default nil
     :bind nil)))

(defmethod traverse-item ((client client) (directive general-directive))
  (go-to-argument 1))

(defun %format-general-float
    (client colon-p at-sign-p w d e k overflowchar padchar exponentchar value significand
     exponent sign)
  (unless d
    (let ((q (if (minusp exponent)
                 (- (quaviver.math:count-digits 10 significand) exponent)
                 (max (quaviver.math:count-digits 10 significand) exponent))))
      (setq d (max q (min exponent 7)))))
  (let* ((ee (if e (+ e 2) 4))
         (ww (if w (- w ee) nil))
         (dd (- d exponent)))
    (cond ((<= 0 dd d)
           (let ((char (if (%format-fixed-format-float client colon-p at-sign-p ww dd 0
                                                       overflowchar padchar value significand
                                                       exponent sign)
                           overflowchar
                           #\space)))
             (dotimes (i ee) (write-char char *format-output*))))
          (t
           (%format-exponential-float client colon-p at-sign-p w d e k overflowchar padchar
                                      exponentchar value significand exponent sign)))))

(defun format-general-float
    (client colon-p at-sign-p w d e k overflowchar padchar exponentchar value)
  (%format-float client
                 (lambda (client value significand exponent sign)
                   (%format-general-float client colon-p at-sign-p w d e k overflowchar padchar
                                          exponentchar value significand exponent sign))
                 value))

(defmethod interpret-item
    ((client client) (directive general-directive) &optional parameters)
  (multiple-value-call #'format-general-float
    client (colon-p directive) (at-sign-p directive) (values-list parameters) (pop-argument)))

(defmethod compile-item
    ((client client) (directive general-directive) &optional parameters)
  `((format-general-float ,(trinsic:client-form client) ,(colon-p directive)
                          ,(at-sign-p directive) ,@parameters ,(pop-argument-form))))

;;; 22.3.3.4 ~$ Monetary floating point.

(defclass monetary-directive (directive) nil)

(defmethod specialize-directive ((client client) (char (eql #\$)) directive)
  (change-class directive 'monetary-directive))

(defmethod parameter-specifications
    ((client client) (directive monetary-directive))
  '((:name d
     :type integer
     :default 2
     :bind nil)
    (:name n
     :type integer
     :default 1
     :bind nil)
    (:name w
     :type (or null integer)
     :default nil
     :bind nil)
    (:name padchar
     :type character
     :default #\Space
     :bind nil)))

(defun %format-monetary-float
    (client colon-p at-sign-p d n w padchar value significand exponent sign)
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
        (trim-fractional client value my-significand exponent sign digit-count fractional-position d))
      (setf leading-zeros (max 0 (- n (max 0 fractional-position))))
      (cond ((> (compute-width) (if w (max w 100) 100))
             (%format-exponential-float client value significand exponent sign
                                        colon-p at-sign-p w (+ d n -1) nil 1
                                        #\Space padchar nil))
            (t
             (when (and colon-p sign-char)
               (write-char sign-char *format-output*))
             (when w
               (loop repeat (max 0 (- w (compute-width)))
                     do (write-char padchar *format-output*)))
             (when (and (not colon-p) sign-char)
               (write-char sign-char *format-output*))
             (quaviver:write-digits 10 my-significand *format-output*
                                    :leading-zeros leading-zeros
                                    :fractional-position (if (zerop my-significand)
                                                             (1+ fractional-position)
                                                             fractional-position)
                                    :fractional-marker #\.))))))

(defun format-monetary-float
    (client colon-p at-sign-p d n w padchar value)
  (%format-float client
                 (lambda (client value significand exponent sign)
                   (%format-monetary-float client colon-p at-sign-p d n w padchar value
                                           significand exponent sign))
                 value))

(defmethod interpret-item
    ((client client) (directive monetary-directive) &optional parameters)
  (multiple-value-call #'format-monetary-float
    client (colon-p directive) (at-sign-p directive) (values-list parameters) (pop-argument)))

(defmethod compile-item
    ((client client) (directive monetary-directive) &optional parameters)
  `((format-monetary-float ,(trinsic:client-form client) ,(colon-p directive)
                           ,(at-sign-p directive) ,@parameters ,(pop-argument-form))))
