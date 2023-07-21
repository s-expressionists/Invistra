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
      (let ((coerced-value (if (rationalp value)
                               (coerce value 'single-float)
                               value)))
        (multiple-value-call func client coerced-value (burger-dybvig-2 coerced-value))))))

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

(defun print-fixed-arg (client value digits exponent colonp at-signp w d k overflowchar padchar)
  (declare (ignore client colonp))
  (let (pre
        post sign
        len)
    (flet ((round-post (count)
             (if (plusp count)
                 (setf (cdr (nthcdr (1- count) post)) nil)
                 (setf post nil))))
      (setf sign
            (cond ((minusp (float-sign value)) #\-)
                  ((and at-signp (plusp value)) #\+)))
      (incf exponent k)
      (cond ((and (zerop (car digits))))
            ((not (plusp exponent))
             (setf post
                   (nconc (make-list (- exponent) :initial-element 0)
                          digits)))
            ((<= exponent (length digits))
             (let ((pair (nthcdr (1- exponent) digits)))
               (setf post (cdr pair)
                     (cdr pair) nil
                     pre digits)))
            (t
             (setf pre
                   (nconc digits
                          (make-list (- exponent (length digits))
                                     :initial-element 0)))))
      (when d
        (let ((l (length post)))
          (cond ((< l d)
                 (setf post
                       (nconc post
                              (make-list (- d l)
                                         :initial-element 0))))
                ((> l d)
                 (round-post (1- d))))))
      (setf len (+ (if sign 2 1)
                   (length pre)
                   (length post)))
      (when (and w
                 (null d)
                 (> len w))
        (round-post (- w
                       (length pre)
                       (if sign 3 2)))
        (setf len (+ (if sign 2 1)
                     (length pre)
                     (length post))))
      (when (and (null post)
                 (null d)
                 (or (null w)
                     (< len w)
                     (null d)
                     (> w (1+ d))))
        (push 0 post)
        (incf len))
      (when (and (null pre)
                 (or (null post)
                     (null w)
                     (< len w)))
        (push 0 pre)
        (incf len))
      (cond ((or (null w)
                 (null overflowchar)
                 (<= len w))
             (when w
               (loop repeat (max 0 (- w len))
                     do (write-char padchar *destination*)))
             (when sign
               (write-char sign *destination*))
             (when pre
               (loop for digit in pre
                     do (write-char (aref *digits* digit) *destination*)))
             (write-char #\. *destination*)
             (when post
               (loop for digit in post
                     do (write-char (aref *digits* digit) *destination*)))
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
  (let (pre
        post sign
        len
        exp)
    (flet ((round-post (count)
             (when (> (length post) count)
               (if (plusp count)
                   (setf (cdr (nthcdr (1- count) post)) nil)
                   (setf post nil)))))
      (setf sign
            (cond ((minusp (float-sign value)) #\-)
                  ((and at-signp (plusp value)) #\+)))
      (setf exponent (if (zerop (car digits))
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
             (setf post
                   (nconc (make-list (- k) :initial-element 0)
                          digits)))
            ((< (length digits) k)
             (setf pre
                   (nconc digits
                          (make-list (- k (length digits)) :initial-element 0))))
            (t
             (setf pre (subseq digits 0 k)
                   post (subseq digits k))))
      (when d
        (let ((l (length post))
              (dp (cond ((zerop k)
                         d)
                        ((plusp k)
                         (- d k -1))
                        (t
                         (+ d k 1)))))
          (cond ((< l dp)
                 (setf post
                       (nconc post
                              (make-list (- dp l)
                                         :initial-element 0))))
                ((> l dp)
                 (round-post (1- dp))))))
      (setf len (+ (if sign 4 3)
                   (length exp)
                   (length pre)
                   (length post)))
      (when (and w
                 (null d)
                 (> len w))
        (round-post (- w
                       (length pre)
                       (length exp)
                       (if sign 4 3)))
        (setf len (+ (if sign 4 3)
                     (length pre)
                     (length exp)
                     (length post))))
      (when (and (null post)
                 (null d)
                 (or (null w)
                     (< len w)
                     #+(or)(null d)
                     #+(or)(> w (1+ d))))
        (push 0 post)
        (incf len))
      (when (and (null pre)
                 (or (null post)
                     (null w)
                     (< len w)))
        (push 0 pre)
        (incf len))
      (cond ((or (null w)
                 (null overflowchar)
                 (<= len w))
             (when w
               (loop repeat (max 0 (- w len))
                     do (write-char padchar *destination*)))
             (when sign
               (write-char sign *destination*))
             (when pre
               (loop for digit in pre
                     do (write-char (aref *digits* digit) *destination*)))
             (write-char #\. *destination*)
             (when post
               (loop for digit in post
                     do (write-char (aref *digits* digit) *destination*)))
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
  (let (pre
        post sign
        len)
    (flet ((round-post (count)
             (if (plusp count)
                 (setf (cdr (nthcdr (1- count) post)) nil)
                 (setf post nil))))
      (setf sign
            (cond ((minusp (float-sign value)) #\-)
                  ((and at-signp (plusp value)) #\+)))
      (cond ((and (zerop (car digits))))
            ((not (plusp exponent))
             (setf post
                   (nconc (make-list (- exponent) :initial-element 0)
                          digits)))
            ((<= exponent (length digits))
             (let ((pair (nthcdr (1- exponent) digits)))
               (setf post (cdr pair)
                     (cdr pair) nil
                     pre digits)))
            (t
             (setf pre
                   (nconc digits
                          (make-list (- exponent (length digits))
                                     :initial-element 0)))))
      (let ((l (length post)))
        (cond ((< l d)
               (setf post
                     (nconc post
                            (make-list (- d l)
                                       :initial-element 0))))
              ((> l d)
               (round-post (1- d)))))
      (let ((l (length pre)))
        (when (< l n)
          (setf pre (nconc (make-list (- n l)
                                      :initial-element 0)
                           pre))))
      (setf len (+ (if sign 2 1)
                   (length pre)
                   (length post)))
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
             (when pre
               (loop for digit in pre
                     do (write-char (aref *digits* digit) *destination*)))
             (write-char #\. *destination*)
             (when post
               (loop for digit in post
                     do (write-char (aref *digits* digit) *destination*))))))))

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
