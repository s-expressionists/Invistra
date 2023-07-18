;;; A portable implementation of the Common Lisp FORMAT function.
;;;
;;; Status:
;;;
;;;   Not all directives are implemented.
;;;
;;; TODO:
;;;
;;;   * Implement the last couple of directives.
;;;
;;;   * Implement the directive compiler.
;;;
;;;   * Improve some of the condition reports.
;;;
;;;   * We might want to use reader conditionals to determine how
;;;     to handle things like counting colums (for the TAB directive),
;;;     because it could be costly (and/or imprecise) to count each
;;;     character output.
;;;
;;;
;;; To think about:
;;;
;;;   * Should we use ASSERT as opposed to ERROR to get correctable
;;;     errors?
;;;
;;;   * Should we put in restarts?
;;;
;;;   * What do we do about the possibility that the syntax categories
;;;     of some characters might be altered (for ignored newline
;;;     directive)?

(cl:in-package #:invistra)

;;; During runtime, this variable is bound to a stream to which
;;; all the output goes.
(defvar *destination*)

(defun interpret-items (client items)
  (loop for item across items
        do (if (stringp item)
               (write-string item *destination*)
               (interpret-format-directive client item))))

;;; Runtime environment

;;; A vector of all the arguments that were passed to this
;;; invocation of FORMAT.
(defvar *arguments*)

(defvar *previous-arguments*)

(defvar *previous-argument-index*)

(defvar *remaining-argument-count*)

(defvar *pop-argument-hook*)

(defvar *escape-hook*)

(defvar *catch-tag*)

(defmacro with-arguments (arguments &body body)
  `(let* ((*arguments* ,arguments)
          (*previous-argument-index* 0)
          (*remaining-argument-count* (length *arguments*))
          (*previous-arguments* (make-array *remaining-argument-count*
                                            :adjustable t :fill-pointer 0))
          (*pop-argument-hook* (lambda ()
                                 (pop *arguments*)))
          (*escape-hook* (lambda ()
                           (unless (or *arguments*
                                       (< *previous-argument-index* (length *previous-arguments*)))
                             (throw *catch-tag* nil)))))
     ,@body))

(defun compute-parameter-value (directive parameter-spec)
  (let* ((parameter-name (car parameter-spec))
         (compile-time-value (funcall parameter-name directive)))
    (cond ((null compile-time-value)
           ;; The parameter was not given at all in the format control
           ;; string, neither as a constant value, nor as a value to
           ;; be acquired at runtime (# or V).  We must use a default
           ;; value if it has any.
           (getf (cdr parameter-spec) :default-value))
          ((eq compile-time-value :argument-reference)
           ;; The parameter was given the explicit value V in the
           ;; format control string, meaning we use the next argument
           ;; to acquire the value of the parameter.  We must test
           ;; that there are more arguments, consume the next one, and
           ;; check that the type of the argument acquired is correct.
           (or (consume-next-argument `(or null
                                           ,(getf (cdr parameter-spec) :type)))
               (getf (cdr parameter-spec) :default-value)))
          ((eq compile-time-value :remaining-argument-count)
           ;; The parameter was given the explicit value # in the
           ;; format control string, meaning we use the number of
           ;; remaining arguments as the value of the parameter.
           (unless (typep *remaining-argument-count*
                          (getf (cdr parameter-spec) :type))
             (error 'argument-type-error
                    :expected-type (getf (cdr parameter-spec) :type)
                    :datum *remaining-argument-count*))
           *remaining-argument-count*)
          (t
           ;; The parameter was given an explicit value (number or
           ;; character) in the format control string, and this is the
           ;; value we want.
           compile-time-value))))

;;; The directive interpreter.

(defmethod interpret-format-directive (client directive)
  (declare (ignore client))
  (error 'unknown-format-directive
         :control-string (control-string directive)
         :tilde-position (start directive)
         :index (1- (end directive))))

(defmacro define-format-directive-interpreter (class-name &body body)
  `(defmethod interpret-format-directive (client (directive ,class-name))
     (declare (ignorable client))
     (with-accessors ((control-string control-string)
                      (start start)
                      (end end)
                      (colonp colonp)
                      (at-signp at-signp))
       directive
       (let ,(loop for parameter-spec in (parameter-specs class-name)
                   collect `(,(car parameter-spec)
                              (compute-parameter-value directive ',parameter-spec)))
         ,@body))))

(defun consume-next-argument (type)
  (unless (< *previous-argument-index* (length *previous-arguments*))
    (let (exited)
      (unwind-protect
           (progn
             (funcall *escape-hook*)
             (setf exited t))
        (unless exited
          (error 'no-more-arguments)))
      (vector-push-extend (funcall *pop-argument-hook*) *previous-arguments*)))
  (when (= *previous-argument-index* (length *previous-arguments*))
    (error 'no-more-arguments))
  (let ((arg (aref *previous-arguments* *previous-argument-index*)))
    (incf *previous-argument-index*)
    (decf *remaining-argument-count*)
    (unless (typep arg type)
      (error 'argument-type-error
             :expected-type type
             :datum arg))
    arg))

(defun go-to-argument (index &optional absolute)
  (when absolute
    (incf *remaining-argument-count* *previous-argument-index*)
    (setf *previous-argument-index* 0))
  (cond ((zerop index)
         (aref *previous-arguments* *previous-argument-index*))
        ((plusp index)
         (prog ()
          next
            (decf index)
            (when (zerop index)
              (return (consume-next-argument t)))
            (consume-next-argument t)
            (go next)))
        (t
         (let ((new-arg-index (+ *previous-argument-index* index)))
           (when (minusp new-arg-index)
             (error 'go-to-out-of-bounds
                    :what-argument new-arg-index
                    :max-arguments *remaining-argument-count*))
           (decf *remaining-argument-count* index)
           (setf *previous-argument-index* new-arg-index)
           (aref *previous-arguments* *previous-argument-index*)))))

(defmacro define-format-directive-compiler (class-name &body body)
  `(defmethod compile-format-directive (client (directive ,class-name))
     (declare (ignorable client))
     (with-accessors ((control-string control-string)
                      (start start)
                      (end end)
                      (colonp colonp)
                      (at-signp at-signp)
                      (given-parameters given-parameters)
                      ,@(loop for parameter-spec in (parameter-specs class-name)
                              collect `(,(car parameter-spec) ,(car parameter-spec))))
       directive
       ,@body)))

(defun compile-time-value (directive slot-name)
  (or (slot-value directive slot-name)
      (getf (cdr (find slot-name
                       (parameter-specs (class-name (class-of directive)))
                       :key #'car))
            :default-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code for individual directives

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1 Basic output

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.1 ~c Character

(define-directive #\c c-directive nil (named-parameters-directive) ())

(define-format-directive-interpreter c-directive
  (let ((char (consume-next-argument 'character)))
    (cond ((and (not colonp) (not at-signp))
           ;; Neither colon nor at-sign.
           ;; The HyperSpec says to do what WRITE-CHAR does.
           (write-char char *destination*))
          ((not at-signp)
           ;; We have only a colon modifier.
           ;; The HyperSpec says to do what WRITE-CHAR does for
           ;; printing characters, and what char-name does otherwise.
           ;; The definition of "printing char" is a graphic character
           ;; other than space.
           (if (and (graphic-char-p char) (not (eql char #\Space)))
               (write-char char *destination*)
               (write-string (char-name char) *destination*)))
          ((not colonp)
           ;; We have only an at-sign modifier.
           ;; The HyperSpec says to print it the way the Lisp
           ;; reader can understand, which I take to mean "use PRIN1".
           ;; It also says to bind *PRINT-ESCAPE* to t.
           (let ((*print-escape* t))
             (incless:write-object client char *destination*)))
          (t
           ;; We have both a colon and and at-sign.
           ;; The HyperSpec says to do what ~:C does, but
           ;; also to mention unusual shift keys on the
           ;; keyboard required to type the character.
           ;; I don't see how to do that, so we do the same
           ;; as for ~:C.
           (if (and (graphic-char-p char) (not (eql char #\Space)))
               (write-char char *destination*)
               (write-string (char-name char) *destination*))))))

(define-format-directive-compiler c-directive
  `((let ((char (consume-next-argument 'character)))
      ,(cond ((and (not colonp) (not at-signp))
              `(write-char char *destination*))
             ((not at-signp)
              `(if (and (graphic-char-p char) (not (eql char #\Space)))
                   (write-char char *destination*)
                   (write-string (char-name char) *destination*)))
             ((not colonp)
              `(let ((*print-escape* t))
                 (incless:write-object ,(incless:client-form client) char *destination*)))
             (t
              `(if (and (graphic-char-p char) (not (eql char #\Space)))
                   (write-char char *destination*)
                   (write-string (char-name char) *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.2 ~% Newline.

(define-directive #\% percent-directive nil (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter percent-directive
  (loop repeat how-many
        do (terpri *destination*)))

(define-format-directive-compiler percent-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (case how-many
      (0 '())
      (1 '((terpri *destination*)))
      (2 '((terpri *destination*)
           (terpri *destination*)))
      (otherwise
        `((loop repeat how-many
                do (terpri *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.3 ~& Fresh line and newlines.

(define-directive #\& ampersand-directive nil (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter ampersand-directive
  (unless (zerop how-many)
    (fresh-line *destination*)
    (loop repeat (1- how-many)
          do (terpri *destination*))))

(define-format-directive-compiler ampersand-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (case how-many
      ((:argument-reference :remaining-argument-count)
       `((unless (zerop how-many)
           (fresh-line *destination*)
           (loop repeat (1- how-many)
                 do (terpri *destination*)))))
      (0 nil)
      (1 `((fresh-line *destination*)))
      (2 `((fresh-line *destination*)
           (terpri *destination*)))
      (otherwise
       `((fresh-line *destination*)
         (loop repeat ,(1- how-many)
               do (terpri *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.4 ~| Page separators.

(define-directive #\| vertical-bar-directive nil (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter vertical-bar-directive
  (loop repeat how-many
        do (write-char #\Page *destination*)))

(define-format-directive-compiler vertical-bar-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (case how-many
      (0 nil)
      (1 `((write-char #\Page *destination*)))
      (2 `((write-char #\Page *destination*)
           (write-char #\Page *destination*)))
      (otherwise
       `((loop repeat how-many
               do (write-char #\Page *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.5 ~~ Tildes.

(define-directive #\~ tilde-directive nil (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter tilde-directive
  (loop repeat how-many
        do (write-char #\~ *destination*)))

(define-format-directive-compiler tilde-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (case how-many
      (0 nil)
      (1 `((write-char #\~ *destination*)))
      (2 `((write-char #\~ *destination*)
           (write-char #\~ *destination*)))
      (otherwise
       `((loop repeat how-many
               do (write-char #\~ *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2 Radix control

(defun print-radix-arg (client radix colonp at-signp mincol padchar commachar comma-interval)
  (let ((argument (consume-next-argument t)))
    (if (not (integerp argument))
        (let ((*print-base* 10)
              (*print-escape* nil)
              (*print-readably* nil))
          (incless:write-object client argument *destination*))
        (let* ((string (let ((*print-base* radix)
                             (*print-radix* nil)
                             (*print-escape* nil)
                             (*print-readably* nil))
                         (with-output-to-string (stream)
                           (incless:write-object client (abs argument) stream))))
               (comma-length (if colonp
                                 (max 0 (floor (1- (length string)) comma-interval))
                                 0))
               (sign-length (if (or at-signp (minusp argument)) 1 0))
               (total-length (+ (length string) comma-length sign-length))
               (pad-length (max 0 (- mincol total-length))))
          ;; Print the padding.
          (loop repeat pad-length
                do (write-char padchar *destination*))
          ;; Possibliy print a sign.
          (cond ((minusp argument)
                 (write-char #\- *destination*))
                (at-signp
                 (write-char #\+ *destination*))
                (t nil))
          ;; Print the string in reverse order
          (loop for index downfrom (1- (length string)) to 0
                for c across string
                do (write-char c *destination*)
                do (when (and colonp
                              (plusp index)
                              (zerop (mod index comma-interval)))
                     (write-char commachar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.1 ~r Radix.

(define-directive #\r r-directive nil (named-parameters-directive)
    ((radix :type (integer 2 36) :default-value nil)
     (mincol :type integer :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type (integer 1) :default-value 3)))

;;; Print an integer as roman numerals to the stream.
;;; The integer must be strictly greater than zero,
;;; and strictly less than 4000.
(defun print-as-roman (integer stream)
  (declare (type (integer 1) integer))
  (multiple-value-bind (thousands rest) (floor integer 1000)
    (loop repeat thousands
          do (write-char #\M stream))
    (multiple-value-bind (hundreds rest) (floor rest 100)
      (write-string (case hundreds
                      (0 "") (1 "C") (2 "CC") (3 "CCC") (4 "CD")
                      (5 "D" ) (6 "DC") (7 "DCC") (8 "DCCC") (9 "CM"))
                    stream)
      (multiple-value-bind (tenths rest) (floor rest 10)
        (write-string (case tenths
                        (0 "") (1 "X") (2 "XX") (3 "XXX") (4 "XL")
                        (5 "L" ) (6 "LX") (7 "LXX") (8 "LXXX") (9 "XC"))
                      stream)
        (write-string (case rest
                        (0 "") (1 "I") (2 "II") (3 "III") (4 "IV")
                        (5 "V" ) (6 "VI") (7 "VII") (8 "VIII") (9 "IX"))
                      stream)))))

;;; Print an integer as old roman numerals to the stream.
;;; The integer must be strictly greater than zero,
;;; and strictly less than 4000.
(defun print-as-old-roman (integer stream)
  (declare (type (integer 1) integer))
  (multiple-value-bind (thousands rest) (floor integer 1000)
    (loop repeat thousands
          do (write-char #\M stream))
    (multiple-value-bind (hundreds rest) (floor rest 100)
      (write-string (case hundreds
                      (0 "") (1 "C") (2 "CC") (3 "CCC") (4 "CCCC")
                      (5 "D" ) (6 "DC") (7 "DCC") (8 "DCCC") (9 "DCCCC"))
                    stream)
      (multiple-value-bind (tenths rest) (floor rest 10)
        (write-string (case tenths
                        (0 "") (1 "X") (2 "XX") (3 "XXX") (4 "XXXX")
                        (5 "L" ) (6 "LX") (7 "LXX") (8 "LXXX") (9 "LXXXX"))
                      stream)
        (write-string (case rest
                        (0 "") (1 "I") (2 "II") (3 "III") (4 "IIII")
                        (5 "V" ) (6 "VI") (7 "VII") (8 "VIII") (9 "VIIII"))
                      stream)))))

(defparameter *cardinal-ones*
  #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defparameter *cardinal-teens*
  #("ten" "eleven" "twelve" "thirteen" "fourteen"
    "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(defparameter *cardinal-tens*
  #(nil nil "twenty" "thirty" "forty"
    "fifty" "sixty" "seventy" "eighty" "ninety"))

(defparameter *groups-of-three*
  #(nil "thousand" "million" "billion" "trillion" "quadrillion"
    "quintillion" "sextillion" "septillion" "octillion" "nonillion"
    "decillion" "undecillion" "duodecillion" "tredecillion"
    "quattuordecillion" "quindecillion" "sexdecillion"
    "septendecillion" "octodecillion" "novemdecillion" "vigintillion"))

;;; Print a cardinal number between 1 and 99.
(defun print-cardinal-tenths (n stream)
  (cond ((< n 10)
         (write-string (aref *cardinal-ones* n) stream))
        ((< n 20)
         (write-string (aref *cardinal-teens* (- n 10)) stream))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (write-string (aref *cardinal-tens* tens) stream)
           (unless (zerop ones)
             (write-char #\- stream)
             (write-string (aref *cardinal-ones* ones) stream))))))

;;; Print a cardinal number between 1 and 999.
(defun print-cardinal-hundreds (n stream)
  (cond ((< n 100)
         (print-cardinal-tenths n stream))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (write-string (aref *cardinal-ones* hundreds) stream)
           (write-string " hundred" stream)
           (unless (zerop rest)
             (write-char #\Space stream)
             (print-cardinal-tenths rest stream))))))

;;; Print a cardinal number n such that 0 < n < 10^65.
(defun print-cardinal-non-zero (n stream magnitude)
  (multiple-value-bind (thousands rest) (floor n 1000)
    (unless (zerop thousands)
      (print-cardinal-non-zero thousands stream (1+ magnitude)))
    (unless (or (zerop thousands) (zerop rest))
      (write-char #\Space stream))
    (unless (zerop rest)
      (print-cardinal-hundreds rest stream)
      (unless (zerop magnitude)
        (write-char #\Space stream)
        (write-string (aref *groups-of-three* magnitude) stream)))))

;;; Print a cardinal number n such that - 10^65 < n < 10^65.
(defun print-cardinal-number (n stream)
  (cond ((minusp n)
         (write-string "negative " stream)
         (print-cardinal-non-zero (- n) stream 0))
        ((zerop n)
         (write-string "zero" stream))
        (t
         (print-cardinal-non-zero n stream 0))))

(defparameter *ordinal-ones*
  #(nil "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"))

(defparameter *ordinal-teens*
  #("tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
    "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))

(defparameter *ordinal-tens*
  #(nil nil "twentieth" "thirtieth" "fortieth"
    "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

;;; Print an ordinal number between 1 and 99.
(defun print-ordinal-tenths (n stream)
  (cond ((< n 10)
         (write-string (aref *ordinal-ones* n) stream))
        ((< n 20)
         (write-string (aref *ordinal-teens* (- n 10)) stream))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (cond ((zerop ones)
                  (write-string (aref *ordinal-tens* tens) stream))
                 (t
                  (write-string (aref *cardinal-tens* tens) stream)
                  (write-char #\- stream)
                  (write-string (aref *ordinal-ones* ones) stream)))))))

;;; Print an ordinal number n such that 0 < n < 1000.
(defun print-ordinal-hundreds (n stream)
  (cond ((< n 100)
         (print-ordinal-tenths n stream))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (write-string (aref *cardinal-ones* hundreds) stream)
           (write-string " hundred" stream)
           (cond ((zerop rest)
                  (write-string "th" stream))
                 (t
                  (write-char #\Space stream)
                  (print-ordinal-tenths rest stream)))))))

;;; Print an ordinal number n such that 0 < n < 10^65.
(defun print-ordinal-non-zero (n stream)
  (multiple-value-bind (hundreds rest) (floor n 100)
    (cond ((zerop rest)
           ;; Hudreds is nonzero.
           (print-cardinal-non-zero n stream 0)
           (write-string "th" stream))
          ((zerop hundreds)
           (print-ordinal-hundreds rest stream))
          (t
           ;; They are both nonzero.
           (print-cardinal-non-zero (* 100 hundreds) stream 0)
           (write-char #\Space stream)
           (print-ordinal-tenths rest stream)))))

;;; Print an ordninal number n such that - 10^65 < n < 10^65.
(defun print-ordinal-number (n stream)
  (cond ((minusp n)
         (write-string "negative " stream)
         (print-ordinal-non-zero (- n) stream))
        ((zerop n)
         (write-string "zeroth" stream))
        (t
         (print-ordinal-non-zero n stream))))

(define-format-directive-interpreter r-directive
  (cond ((not (null radix))
         (print-radix-arg client radix colonp at-signp mincol padchar commachar comma-interval))
        ((and colonp at-signp)
         (print-as-old-roman (consume-next-argument '(integer 1))
                             *destination*))
        (at-signp
         (print-as-roman (consume-next-argument '(integer 1))
                         *destination*))
        (colonp
         (print-ordinal-number (consume-next-argument
                                `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                               *destination*))
        (t
         (print-cardinal-number (consume-next-argument
                                 `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                                *destination*))))

(define-format-directive-compiler r-directive
  (let ((print-number-radix `(print-radix-arg ,(incless:client-form client)
                                              radix ,colonp ,at-signp mincol
                                              padchar commachar comma-interval))
        (print-null-radix (cond ((and colonp at-signp)
                                 `(print-as-old-roman (consume-next-argument '(integer 1))
                                                      *destination*))
                                (at-signp
                                 `(print-as-roman (consume-next-argument '(integer 1))
                                                  *destination*))
                                (colonp
                                 `(print-ordinal-number (consume-next-argument
                                                         `(integer ,(1+ (- (expt 10 65)))
                                                                   ,(1- (expt 10 65))))
                                                        *destination*))
                                (t
                                 `(print-cardinal-number (consume-next-argument
                                                          `(integer ,(1+ (- (expt 10 65)))
                                                                    ,(1- (expt 10 65))))
                                                         *destination*)))))
  (cond ((numberp radix)
         (list print-number-radix))
        ((null radix)
         (list print-null-radix))
        (t
         `((if (null radix)
               ,print-null-radix
               ,print-number-radix))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.2 ~d Decimal.

(define-directive #\d d-directive nil (named-parameters-directive)
    ((mincol :type integer :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type (integer 1) :default-value 3)))

(define-format-directive-interpreter d-directive
  (print-radix-arg client 10 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler d-directive
  `((print-radix-arg ,(incless:client-form client) 10 ,colonp ,at-signp mincol padchar commachar comma-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.3 ~b Binary.

(define-directive #\b b-directive nil (named-parameters-directive)
    ((mincol :type integer :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type (integer 1) :default-value 3)))

(define-format-directive-interpreter b-directive
  (print-radix-arg client 2 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler b-directive
  `((print-radix-arg ,(incless:client-form client) 2 ,colonp ,at-signp mincol padchar commachar comma-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.4 ~o Octal.

(define-directive #\o o-directive nil (named-parameters-directive)
    ((mincol :type integer :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type (integer 1) :default-value 3)))

(define-format-directive-interpreter o-directive
  (print-radix-arg client 8 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler o-directive
  `((print-radix-arg ,(incless:client-form client) 8 ,colonp ,at-signp mincol padchar commachar comma-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.5 ~x Hexadecimal.

(define-directive #\x x-directive nil (named-parameters-directive)
    ((mincol :type integer :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type (integer 1) :default-value 3)))

(define-format-directive-interpreter x-directive
  (print-radix-arg client 16 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler x-directive
  `((print-radix-arg ,(incless:client-form client) 16 ,colonp ,at-signp mincol padchar commachar comma-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3 Floating-point printers

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

(defun print-fixed-arg (client colonp at-signp w d k overflowchar padchar)
  (let ((value (consume-next-argument t)))
    (cond ((or (complexp value)
               (not (numberp value)))
           (print-radix-arg client 10 colonp at-signp 0 padchar nil nil))
          (t
           (let (pre
                 post sign
                 len)
             (flet ((round-post (count)
                      (if (plusp count)
                          (setf (cdr (nthcdr (1- count) post)) nil)
                          (setf post nil))))
               (when (rationalp value)
                 (setf value (coerce value 'single-float)))
               (setf sign
                     (cond ((minusp (float-sign value)) #\-)
                           ((and at-signp (plusp value)) #\+)))
               (multiple-value-bind (digits exponent)
                   (burger-dybvig-2 value)
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
                                do (write-char (aref *digits* digit) *destination*))))
                       (t
                        (loop repeat w
                              do (write-char overflowchar *destination*)))))))))))

(define-format-directive-interpreter f-directive
  (print-fixed-arg client colonp at-signp w d k overflowchar padchar))

(define-format-directive-compiler f-directive
  `((print-fixed-arg ,(incless:client-form client) ,colonp ,at-signp w d k overflowchar padchar)))

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

(defun print-exponent-arg (client colonp at-signp w d e k overflowchar padchar exponentchar)
  (let ((value (consume-next-argument t)))
    (cond ((or (complexp value)
               (not (numberp value)))
           (print-radix-arg client 10 colonp at-signp 0 padchar nil nil))
          (t
           (let (pre
                 post sign
                 len
                 exp)
             (flet ((round-post (count)
                      (when (> (length post) count)
                        (if (plusp count)
                            (setf (cdr (nthcdr (1- count) post)) nil)
                            (setf post nil)))))
               (when (rationalp value)
                 (setf value (coerce value 'single-float)))
               (setf sign
                     (cond ((minusp (float-sign value)) #\-)
                           ((and at-signp (plusp value)) #\+)))
               (multiple-value-bind (digits exponent)
                   (burger-dybvig-2 value)
                 (setf exponent (if (zerop (car digits))
                                    0
                                    (+ exponent k -2)))
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
                 (if (minusp k)
                     (setf post
                           (nconc (make-list (- k) :initial-element 0)
                                  digits))
                     (setf pre (subseq digits 0 k)
                           post (subseq digits k)))
                 (when d
                   (let ((l (length post)))
                     (cond ((< l d)
                            (setf post
                                  (nconc post
                                         (make-list (- d l)
                                                    :initial-element 0))))
                           ((> l d)
                            (round-post (1- d))))))
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
                              do (write-char overflowchar *destination*)))))))))))

(define-format-directive-interpreter e-directive
  (print-exponent-arg client
                      colonp at-signp
                      w d e k overflowchar padchar exponentchar))

(define-format-directive-compiler e-directive
  `((print-exponent-arg ,(incless:client-form client)
                        ,colonp ,at-signp
                        w d e k overflowchar padchar exponentchar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.3 ~g General floating point.

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.4 ~$ Monetary floating point.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4 Printer operations

(defun print-a-or-s (raw-output at-signp mincol colinc minpad padchar)
  (let ((pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
    (if at-signp
        (progn (loop repeat pad-length do (write-char padchar *destination*))
               (write-string raw-output *destination*))
        (progn (write-string raw-output *destination*)
               (loop repeat pad-length do (write-char padchar *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.1 ~a Aesthetic.

(define-directive #\a a-directive nil (named-parameters-directive)
    ((mincol :type integer :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type integer :default-value 0)
     (padchar :type character :default-value #\Space)))

(define-format-directive-interpreter a-directive
  (let ((*print-escape* nil)
        (*print-readably* nil)
        (arg (consume-next-argument t)))
    (print-a-or-s (if (and colonp (null arg))
                      "()"
                      (with-output-to-string (stream)
                        (incless:write-object client arg stream)))
                  at-signp mincol colinc minpad padchar)))

(define-format-directive-compiler a-directive
  `((let* ((*print-escape* nil)
           (*print-readably* nil)
           (arg (consume-next-argument t))
           (raw-output
             ,(if colonp
                  `(if (null arg)
                       "()"
                       (with-output-to-string (stream)
                         (incless:write-object ,(incless:client-form client) arg stream)))
                  `(with-output-to-string (stream)
                     (incless:write-object ,(incless:client-form client) arg stream))))
           (pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
      ,@(if at-signp
            `((loop repeat pad-length
                    do (write-char padchar *destination*))
              (write-string raw-output *destination*))
            `((write-string raw-output *destination*)
              (loop repeat pad-length
                    do (write-char padchar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.2 ~s Standard.

(define-directive #\s s-directive nil (named-parameters-directive)
    ((mincol :type integer :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type integer :default-value 0)
     (padchar :type character :default-value #\Space)))

(define-format-directive-interpreter s-directive
  (let ((*print-escape* t)
        (arg (consume-next-argument t)))
    (print-a-or-s (if (and colonp (null arg))
                      "()"
                      (with-output-to-string (stream)
                        (incless:write-object client arg stream)))
                  at-signp mincol colinc minpad padchar)))

(define-format-directive-compiler s-directive
  `((let* ((*print-escape* t)
           (arg (consume-next-argument t))
           (raw-output
             ,(if colonp
                  `(if (null arg)
                       "()"
                       (with-output-to-string (stream)
                         (incless:write-object ,(incless:client-form client) arg stream)))
                  `(with-output-to-string (stream)
                     (incless:write-object ,(incless:client-form client) arg stream))))
           (pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
      ,@(if at-signp
            `((loop repeat pad-length
                    do (write-char padchar *destination*))
              (write-string raw-output *destination*))
            `((write-string raw-output *destination*)
              (loop repeat pad-length
                    do (write-char padchar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.3 ~w Write.

(define-directive #\w w-directive nil (named-parameters-directive) ())

(define-format-directive-interpreter w-directive
  (cond ((and colonp at-signp)
         (let ((*print-pretty* t)
               (*print-level* nil)
               (*print-length* nil))
           (incless:write-object client (consume-next-argument t) *destination*)))
        (colonp
         (let ((*print-pretty* t))
           (incless:write-object client (consume-next-argument t) *destination*)))
        (at-signp
         (let ((*print-level* nil)
               (*print-length* nil))
           (incless:write-object client (consume-next-argument t) *destination*)))
        (t
         (incless:write-object client (consume-next-argument t) *destination*))))

(define-format-directive-compiler w-directive
  (cond ((and colonp at-signp )
         `((let ((*print-pretty* t)
                 (*print-level* nil)
                 (*print-length* nil))
             (incless:write-object ,(incless:client-form client) (consume-next-argument t) *destination*))))
        (colonp
         `((let ((*print-pretty* t))
             (incless:write-object ,(incless:client-form client) (consume-next-argument t) *destination*))))
        (at-signp
         `((let ((*print-level* nil)
                 (*print-length* nil))
             (incless:write-object ,(incless:client-form client) (consume-next-argument t) *destination*))))
        (t
         `((incless:write-object ,(incless:client-form client) (consume-next-argument t) *destination*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5 Pretty printer operations

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.1 ~_ Conditional newline

(define-directive #\_ underscore-directive nil (named-parameters-directive) ())

(define-format-directive-interpreter underscore-directive
  (inravina:pprint-newline client *destination*
                           (cond ((and colonp at-signp) :mandatory)
                                 (colonp :fill)
                                 (at-signp :miser)
                                 (t :linear))))

(define-format-directive-compiler underscore-directive
  `((inravina:pprint-newline ,(incless:client-form client) *destination*
                             ,(cond ((and colonp at-signp) :mandatory)
                                    (colonp :fill)
                                    (at-signp :miser)
                                    (t :linear)))))

(define-directive #\>
    end-logical-block-directive
    nil
    (named-parameters-directive end-structured-directive-mixin)
    ())

(define-format-directive-interpreter end-logical-block-directive
  ;; do nothing
  nil)

(define-format-directive-compiler end-logical-block-directive
    ;; do nothing
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.2 ~< Logical block

(define-directive #\<
    logical-block-directive
    end-logical-block-directive
    (named-parameters-directive structured-directive-mixin)
    ())

(defmethod check-directive-syntax progn ((directive logical-block-directive))
  (flet ((check-fix (items)
           (when (notevery #'stringp items)
             (error "Directives are not allowed in logical block prefix or suffix"))))
    (when (> (length (clauses directive)) 3)
      (error "Logical block only allows three clauses"))
    (when (> (length (clauses directive)) 1)
      (check-fix (aref (clauses directive) 0)))
    (when (= (length (clauses directive)) 3)
      (check-fix (aref (clauses directive) 2))))
  (let* ((last-clause (aref (clauses directive) (1- (length (clauses directive)))))
         (last-item (aref last-clause (1- (length last-clause)))))
    (when (at-signp last-item)
      (loop with index = (if (= (length (clauses directive)) 1) 0 1)
            with current = (aref (clauses directive) index)
            with result = (make-array (* 2 (length current)) :adjustable t :fill-pointer 0)
            for item across current
            finally (setf (aref (clauses directive) index) result)
            if (stringp item)
              do (loop with start = 0
                       with in-blank-p = nil
                       for char across item
                       for index from 0
                       for blankp = (and (find char #(#\Space #\Tab #\Page #\Return)) t)
                       finally (vector-push-extend (subseq item start) result)
                               (when in-blank-p
                            (vector-push-extend (make-instance 'underscore-directive :colonp t :at-signp nil) result))
                       when (and in-blank-p (not blankp))
                         do (vector-push-extend (subseq item start index) result)
                            (vector-push-extend (make-instance 'underscore-directive :colonp t :at-signp nil) result)
                            (setf start index)
                       do (setf in-blank-p blankp))
            else
              do (vector-push-extend item result)))))

(define-format-directive-interpreter logical-block-directive
  (let ((prefix (cond ((and (> (length (clauses directive)) 1)
                            (> (length (aref (clauses directive) 0)) 1))
                       (aref (aref (clauses directive) 0) 0))
                      (colonp
                       "(")
                      (t
                       "")))
        (suffix (cond ((and (= (length (clauses directive)) 3)
                            (> (length (aref (clauses directive) 2)) 1))
                       (aref (aref (clauses directive) 2) 0))
                      (colonp
                       ")")
                      (t
                       "")))
        (per-line-prefix-p (at-signp (aref (aref (clauses directive) 0)
                                           (1- (length (aref (clauses directive) 0))))))
        (object (unless at-signp (consume-next-argument t))))
    (flet ((interpret-body (*destination* escape-hook pop-argument-hook)
             (if at-signp
                 (interpret-items client (aref (clauses directive)
                                               (if (= (length (clauses directive)) 1)
                                                   0
                                                   1)))
                 (let* ((*remaining-argument-count* (dotted-list-length object))
                        (*previous-arguments* (make-array *remaining-argument-count*
                                                          :adjustable t :fill-pointer 0))
                        (*previous-argument-index* 0)
                        (*escape-hook* escape-hook)
                        (*pop-argument-hook* pop-argument-hook))
                   (interpret-items client (aref (clauses directive)
                                                 (if (= (length (clauses directive)) 1)
                                                     0
                                                     1)))))))
      (if per-line-prefix-p
          (inravina:execute-pprint-logical-block client *destination*
                                                 object #'interpret-body
                                                 :per-line-prefix prefix :suffix suffix)
          (inravina:execute-pprint-logical-block client *destination*
                                                 object #'interpret-body
                                                 :prefix prefix :suffix suffix)))))

(define-format-directive-compiler logical-block-directive
  (let ((prefix (cond ((and (> (length (clauses directive)) 1)
                            (> (length (aref (clauses directive) 0)) 1))
                       (aref (aref (clauses directive) 0) 0))
                      (colonp
                       "(")
                      (t
                       "")))
        (suffix (cond ((and (= (length (clauses directive)) 3)
                            (> (length (aref (clauses directive) 2)) 1))
                       (aref (aref (clauses directive) 2) 0))
                      (colonp
                       ")")
                      (t
                       "")))
        (per-line-prefix-p (at-signp (aref (aref (clauses directive) 0)
                                           (1- (length (aref (clauses directive) 0)))))))
    (if at-signp
        `((inravina:execute-pprint-logical-block ,(incless:client-form client) *destination*
                                                 nil
                                                 (lambda (*destination* escape-hook pop-argument-hook)
                                                   (declare (ignore escape-hook pop-argument-hook))
                                                   (catch *catch-tag*
                                                     ,@(compile-items client (aref (clauses directive)
                                                                                   (if (= (length (clauses directive)) 1)
                                                                                       0
                                                                                       1)))))
                                                 ,(if per-line-prefix-p :per-line-prefix :prefix) ,prefix
                                                 :suffix ,suffix))
        `((let* ((object (consume-next-argument t))
                 (*remaining-argument-count* (dotted-list-length object))
                 (*previous-arguments* (make-array *remaining-argument-count*
                                                   :adjustable t :fill-pointer 0))
                 (*previous-argument-index* 0))
            (inravina:execute-pprint-logical-block ,(incless:client-form client) *destination*
                                                   object
                                                   (lambda (*destination* *escape-hook* *pop-argument-hook*)

                                                     ,@(compile-items client (aref (clauses directive)
                                                                                   (if (= (length (clauses directive)) 1)
                                                                                       0
                                                                                       1))))
                                                   ,(if per-line-prefix-p :per-line-prefix :prefix) ,prefix
                                                   :suffix ,suffix))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.3 ~i Indent

(define-directive #\i i-directive nil (named-parameters-directive)
    ((how-many :type (integer 0) :default-value 0)))

(define-format-directive-interpreter i-directive
  (inravina:pprint-indent client *destination*
                          (if colonp :current :block)
                          how-many))

(define-format-directive-compiler i-directive
  `((inravina:pprint-indent ,(incless:client-form client) *destination*
                            ,(if colonp :current :block)
                            how-many)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.4 ~/ Call function

;;; This directive is particular in two different ways.  First, as
;;; with the "ignored newline" directive, there are characters
;;; belonging to the directive beyond the directive character itself,
;;; which means the standard mechanism of parsing it cannot be used.
;;; Second, this directive takes an arbitrary number of parameters.
;;;
;;; So, define-format-directive-interpreter cannot be used, since its
;;; main purpose is to give lexical access to each parameter by name.

(define-directive #\/ call-function-directive nil (directive)
    ()
  (%function-name :accessor function-name))

(defmethod check-directive-syntax progn ((directive call-function-directive))
  ;; Check that there is at most one package marker in the function name.
  ;; Also, compute a symbol from the name.
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp))
    directive
    ;; To figure out where the name of the function starts and ends,
    ;; we cannot search from the beginning of the directive, because
    ;; the given parameters can contain arbitrary characters following
    ;; a single quote (indicating a character parameter).  However,
    ;; we know that the last character of the directive is the trailing
    ;; #\/ of the function name, and the one preceding that is the
    ;; #\/ preceding the function name.
    (let ((pos1 (1+ (position #\/ control-string :end (1- end) :from-end t)))
          (pos2 (1- end)))
      (let ((position-of-first-package-marker
             (position #\: control-string :start pos1 :end pos2))
            (position-of-last-package-marker
             (position #\: control-string :start pos1 :end pos2 :from-end t)))
        (when (and (not (null position-of-first-package-marker))
                   (> position-of-last-package-marker
                      (1+ position-of-first-package-marker)))
          (error 'too-many-package-markers
                 :directive directive))
        ;; The HyperSpec says that all the characters of the function
        ;; name are treated as if they were upper-case.  It would
        ;; probably be smarter to follow the readtable-case of the
        ;; current readtable, but that's not what the spec says.
        (let ((package-name
               (if (null position-of-first-package-marker)
                   "COMMON-LISP-USER"
                   (string-upcase
                    (subseq control-string
                            pos1
                            position-of-first-package-marker))))
              (symbol-name
               (string-upcase
                (subseq control-string
                        (if (null position-of-first-package-marker)
                            pos1
                            (1+ position-of-last-package-marker))
                        pos2))))
          (let ((package (find-package package-name)))
            (when (null package)
              (error 'no-such-package
                     :directive directive))
            (multiple-value-bind (symbol status)
                (find-symbol symbol-name package)
              (when (or (null status)
                        (eq status :inherited))
                (error 'no-such-symbol
                       :directive directive))
              (when (and (= position-of-first-package-marker
                            position-of-last-package-marker)
                         (eq status :internal))
                (error 'symbol-not-external
                       :directive directive))
              (setf (function-name directive)
                    symbol))))))))

(defmethod interpret-format-directive (client (directive call-function-directive))
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp)
                   (at-signp at-signp)
                   (given-parameters given-parameters)
                   (function-name function-name))
    directive
    (let ((param-args
           (loop for parameter in given-parameters
                 collect (cond ((eq parameter :remaining-argument-count)
                                *remaining-argument-count*)
                               ((eq parameter :argument-reference)
                                (consume-next-argument t))
                               (t parameter)))))
      (apply function-name
             *destination*
             (consume-next-argument t)
             colonp
             at-signp
             param-args))))

;;; This is not quite right.  We should probably look up the
;;; function name at runtime as opposed to compile time.
(defmethod compile-format-directive (client (directive call-function-directive))
  (declare (ignorable client))
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp)
                   (at-signp at-signp)
                   (given-parameters given-parameters)
                   (function-name function-name))
      directive
    (let ((param-args
            (loop for parameter in given-parameters
                  collect (case parameter
                            (:remaining-argument-count
                             '*remaining-argument-count*)
                            (:argument-reference
                             `(consume-next-argument t))
                            (otherwise
                             parameter)))))
      `((,function-name *destination*
                        (consume-next-argument t)
                        ,colonp
                        ,at-signp
                        ,@param-args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6 Layout control

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.1 ~TAB Tabulate

(define-directive #\t tabulate-directive nil (named-parameters-directive)
    ((colnum :type (integer 1) :default-value 1)
     (colinc :type (integer 1) :default-value 1)))

(defun format-relative-tab (client colnum colinc)
  (if (inravina:pretty-stream-p *destination*)
      (inravina:pprint-tab client *destination* :line-relative colnum colinc)
      (let* ((cur (trivial-stream-column:line-column *destination*)))
        (trivial-stream-column:advance-to-column (if (and cur (plusp colinc))
                                                     (* (ceiling (+ cur colnum) colinc) colinc)
                                                     colnum)
                                                 *destination*))))

(defun format-absolute-tab (client colnum colinc)
  (if (inravina:pretty-stream-p *destination*)
      (inravina:pprint-tab client *destination* :line colnum colinc)
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
         (inravina:pprint-tab client *destination*
                              (if atsignp :section-relative :section)
                              colnum colinc))
        (atsignp
         (format-relative-tab client colnum colinc))
        (t
         (format-absolute-tab client colnum colinc))))

(define-format-directive-compiler tabulate-directive
  (cond (colonp
         `(inravina:pprint-tab ,(incless:client-form client) *destination*
                               ,(if atsignp :section-relative :section)
                               colnum colinc))
        (atsignp
         `(format-relative-tab ,(incless:client-form client) colnum colinc))
        (t
         `(format-absolute-tab ,(incless:client-form client) colnum colinc))))

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

(define-format-directive-interpreter justification-directive
    ;; do nothing
    nil)

(define-format-directive-compiler justification-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7 Control-flow operations

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.1 ~* Go to

(define-directive #\* go-to-directive nil (named-parameters-directive at-most-one-modifier-mixin)
    ((param :type (integer 0))))

(define-format-directive-interpreter go-to-directive
  (cond (colonp
         ;; Back up in the list of arguments.
         ;; The default value for the parameter is 1.
         (go-to-argument (- (or param 1))))
        (at-signp
         ;; Go to an absolute argument number.
         ;; The default value for the parameter is 0.
         (go-to-argument (or param 0) t))
        (t
         ;; Skip the next arguments.
         ;; The default value for the parameter is 1.
         (go-to-argument (or param 1)))))

(define-format-directive-compiler go-to-directive
  (cond (colonp
         ;; Back up in the list of arguments.
         ;; The default value for the parameter is 1.
         `((go-to-argument (- (or param 1)))))
        (at-signp
         ;; Go to an absolute argument number.
         ;; The default value for the parameter is 0.
         `((go-to-argument (or param 0) t)))
        (t
         ;; Skip the next arguments.
         ;; The default value for the parameter is 1.
         `((go-to-argument (or param 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.1 ~; Clause separator

;;; This one is out of order to allow a clean compilation

(define-directive #\; semicolon-directive nil (named-parameters-directive only-colon-mixin) ())

(defmethod structured-separator-p ((directive semicolon-directive))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.3 ~] End of conditional expression

(define-directive #\] end-conditional-directive nil (named-parameters-directive no-modifiers-mixin end-structured-directive-mixin) ())

(define-format-directive-interpreter end-conditional-directive
    ;; do nothing
    nil)

(define-format-directive-compiler end-conditional-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.2 ~[ Conditional expression

(define-directive #\[ conditional-directive end-conditional-directive
  (named-parameters-directive structured-directive-mixin at-most-one-modifier-mixin)
    ((param :type integer))
  (%last-clause-is-default-p :initform nil :accessor last-clause-is-default-p))

(defmethod check-directive-syntax progn ((directive conditional-directive))
  ;; Check that, if a parameter is given, then there are
  ;; no modifiers.
  (when (and (not (null (given-parameters directive)))
             (or (colonp directive) (at-signp directive)))
    (error 'modifier-and-parameter
           :directive directive))
  ;; Check that, if a colon modifier was given, then
  ;; there should be a single clause separator (two clauses).
  (when (and (colonp directive)
             (/= (length (clauses directive)) 2))
    (error 'colon-modifier-requires-two-clauses))
  ;; Check that, if an at-sign modifier was given, then
  ;; there should be a no clause separators (a single clause).
  (when (and (at-signp directive)
             (/= (length (clauses directive)) 1))
    (error 'at-sign-modifier-requires-one-clause))
  (let ((pos (position-if (lambda (items)
                            (let ((last (aref items (1- (length items)))))
                              (and (structured-separator-p last)
                                   (colonp last))))
                          (clauses directive))))
    ;; Check that, if a modifier is given, then there should
    ;; be no clause separator with colon modifier.
    (when (and (or (colonp directive) (at-signp directive))
               pos)
      (error 'clause-separator-with-colon-modifier-not-allowed
             :directive directive))
    (when (and pos
               (< pos (- (length (clauses directive)) 2)))
      (error 'illegal-clause-separators
             :directive directive))
    (setf (last-clause-is-default-p directive) (and pos t))))

(define-format-directive-interpreter conditional-directive
  (cond (at-signp
         (when (consume-next-argument t)
           (go-to-argument -1)
           (interpret-items client (aref (clauses directive) 0))))
        (colonp
         (interpret-items client
                          (aref (clauses directive)
                                (if (consume-next-argument t) 1 0))))
        (t
         ;; If a parameter was given, use it,
         ;; else use the next argument.
         (let ((val (or param (consume-next-argument 'integer))))
           (if (or (minusp val)
                   (>= val (length (clauses directive))))
               ;; Then the argument is out of range
               (when (last-clause-is-default-p directive)
                 ;; Then execute the default-clause
                 (interpret-items client
                                  (aref (clauses directive)
                                        (1- (length (clauses directive))))))
               ;; Else, execute the corresponding clause
               (interpret-items client
                                (aref (clauses directive) val)))))))

(define-format-directive-compiler conditional-directive
  (cond (at-signp
         `((when (consume-next-argument t)
           (go-to-argument -1)
           ,@(compile-items client (aref (clauses directive) 0)))))
        (colonp
         `((cond ((consume-next-argument t)
                  ,@(compile-items client (aref (clauses directive) 1)))
                 (t
                  ,@(compile-items client (aref (clauses directive) 0))))))
        (t
         ;; If a parameter was given, use it,
         ;; else use the next argument.
         `((let ((val (or param (consume-next-argument 'integer))))
             (if (or (minusp val)
                     (>= val ,(length (clauses directive))))
                 ;; Then the argument is out of range
                 ,(when (last-clause-is-default-p directive)
                    ;; Then execute the default-clause
                    `(progn ,@(compile-items client
                                             (aref (clauses directive)
                                                   (1- (length (clauses directive)))))))
                 ;; Else, execute the corresponding clause
                 (case val
                   ,@(loop for i from 0 below (length (clauses directive))
                           for clause across (clauses directive)
                           collect `(,i ,@(compile-items client
                                                         (aref (clauses directive) i)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.5 ~} End of iteration

(define-directive #\} end-iteration-directive nil (named-parameters-directive only-colon-mixin end-structured-directive-mixin) ())

(define-format-directive-interpreter end-iteration-directive
    ;; do nothing
    nil)

(define-format-directive-compiler end-iteration-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.4 ~{ Iteration

(define-directive #\{ iteration-directive end-iteration-directive
  (named-parameters-directive structured-directive-mixin)
    ((iteration-limit :type (integer 0))))

(define-format-directive-interpreter iteration-directive
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let* ((items (aref (clauses directive) 0))
         (oncep (colonp (aref items (1- (length items))))))
    (if (= (length items) 1)
        (let ((control (consume-next-argument '(or string function))))
          (cond ((and colonp at-signp)
                 ;; The remaining arguments should be lists.  Each argument
                 ;; is used in a different iteration.
                 (if (functionp control)
                     (catch *catch-tag*
                       (loop for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             when (or (not oncep) (plusp index))
                               do (funcall *escape-hook*)
                             do (apply control *destination* (consume-next-argument 'list))))
                     (catch *catch-tag*
                       (loop for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             when (or (not oncep) (plusp index))
                               do (funcall *escape-hook*)
                             do (with-arguments (consume-next-argument 'list)
                                  (catch *catch-tag*
                                     (format-with-runtime-arguments client control)))))))
                (colonp
                 ;; We use one argument, and that should be a list of sublists.
                 ;; Each sublist is used as arguments for one iteration.
                 (if (functionp control)
                     (let ((arg (consume-next-argument 'list)))
                       (if (null iteration-limit)
                           (loop for args in arg ; a bit unusual naming perhaps
                                 do (apply control *destination* args))
                           (loop for args in arg ; a bit unusual naming perhaps
                                 repeat iteration-limit
                                 do (apply control *destination* args))))
                     (let ((arg (consume-next-argument 'list)))
                       (flet ((one-iteration (args)
                                (unless (listp args)
                                  (error 'argument-type-error
                                         :expected-type 'list
                                         :datum args))
                                (with-arguments args
                                  (catch *catch-tag*
                                     (format-with-runtime-arguments client control)))))
                         (if (null iteration-limit)
                             (loop for args in arg ; a bit unusual naming perhaps
                                   do (one-iteration args))
                             (loop for args in arg ; a bit unusual naming perhaps
                                   repeat iteration-limit
                                   do (one-iteration args)))))))
                (at-signp
                 (if (functionp control)
                     nil
                     (loop for index from 0
                           while (or (null iteration-limit)
                                     (< index iteration-limit))
                           when (or (not oncep) (plusp index))
                             do (funcall *escape-hook*)
                           do (format-with-runtime-arguments client control))))
                (t
                 ;; no modifiers
                 ;; We use one argument, and that should be a list.
                 ;; The elements of that list are used by the iteration.
                 (if (functionp control)
                     (loop for args = (consume-next-argument 'list)
                             then (apply control *destination* args)
                           for index from 0
                           while (and (or (null iteration-limit)
                                          (< index iteration-limit))
                                      (or (and oncep (zerop index)) args)))
                     (with-arguments (consume-next-argument 'list)
                       (catch *catch-tag*
                         (loop for index from 0
                               while (or (null iteration-limit)
                                         (< index iteration-limit))
                               when (or (not oncep) (plusp index))
                                 do (funcall *escape-hook*)
                               do (format-with-runtime-arguments client control))))))))
        (cond ((and colonp at-signp)
               ;; The remaining arguments should be lists.  Each argument
               ;; is used in a different iteration.
               (catch *catch-tag*
                 (loop for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       when (or (not oncep) (plusp index))
                         do (funcall *escape-hook*)
                       do (with-arguments (consume-next-argument 'list)
                            (catch *catch-tag*
                              (interpret-items client items))))))
              (colonp
               ;; We use one argument, and that should be a list of sublists.
               ;; Each sublist is used as arguments for one iteration.
               (let ((arg (consume-next-argument 'list)))
                 (flet ((one-iteration (args)
                          (unless (listp args)
                            (error 'argument-type-error
                                   :expected-type 'list
                                   :datum args))
                          (with-arguments args
                            (catch *catch-tag*
                              (interpret-items client items)))))
                   (if (null iteration-limit)
                       (loop for args in arg ; a bit unusual naming perhaps
                             do (one-iteration args))
                       (loop for args in arg ; a bit unusual naming perhaps
                             repeat iteration-limit
                             do (one-iteration args))))))
              (at-signp
               (loop for index from 0
                     while (or (null iteration-limit)
                               (< index iteration-limit))
                     when (or (not oncep) (plusp index))
                       do (funcall *escape-hook*)
                     do (interpret-items client items)))
              (t
               ;; no modifiers
               ;; We use one argument, and that should be a list.
               ;; The elements of that list are used by the iteration.
               (with-arguments (consume-next-argument 'list)
                 (catch *catch-tag*
                   (loop for index from 0
                         while (or (null iteration-limit)
                                   (< index iteration-limit))
                         when (or (not oncep) (plusp index))
                           do (funcall *escape-hook*)
                         do (interpret-items client items)))))))))

(define-format-directive-compiler iteration-directive
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let* ((items (aref (clauses directive) 0))
         (oncep (colonp (aref items (1- (length items))))))
    (if (= (length items) 1)
        (cond ((and colonp at-signp)
               ;; The remaining arguments should be lists.  Each argument
               ;; is used in a different iteration.
               `((catch *catch-tag*
                   (loop with control = (consume-next-argument '(or function string))
                         for index from 0
                         while (or (null iteration-limit)
                                   (< index iteration-limit))
                         ,@(if oncep
                               '(when (plusp index) do (funcall *escape-hook*))
                               '(do (funcall *escape-hook*)))
                         do (if (functionp control)
                                (apply control *destination* (consume-next-argument 'list))
                                (catch *catch-tag*
                                  (with-arguments (consume-next-argument 'list)
                                    (format-with-runtime-arguments ,(incless:client-form client)
                                                                   control))))))))
              (colonp
               ;; We use one argument, and that should be a list of sublists.
               ;; Each sublist is used as arguments for one iteration.
               `((let* ((control (consume-next-argument '(or function string)))
                        (arg (consume-next-argument 'list)))
                   (flet ((one-iteration (args)
                            (unless (listp args)
                              (error 'argument-type-error
                                     :expected-type 'list
                                     :datum args))
                            (if (functionp control)
                                (apply control *destination* args)
                                (catch *catch-tag*
                                  (with-arguments args
                                    (format-with-runtime-arguments ,(incless:client-form client)
                                                                   control))))))
                     (loop for args in arg ; a bit unusual naming perhaps
                           for index from 0
                           while (or (null iteration-limit)
                                     (< index iteration-limit))
                           do (one-iteration args))))))
              (at-signp
               `((loop with control = (consume-next-argument 'string)
                       for index from 0
                       while (or (null iteration-limit)
                                 (< index iteration-limit))
                       ,@(if oncep
                             '(when (plusp index) do (funcall *escape-hook*))
                             '(do (funcall *escape-hook*)))
                       do (format-with-runtime-arguments ,(incless:client-form client)
                                                         control))))
              (t
               ;; no modifiers
               ;; We use one argument, and that should be a list.
               ;; The elements of that list are used by the iteration.
               `((let ((control (consume-next-argument '(or function string))))
                   (if (functionp control)
                       (loop for args = (consume-next-argument 'list)
                               then (apply control *destination* args)
                             for index from 0
                             while (and (or (null iteration-limit)
                                            (< index iteration-limit))
                                        ,(if oncep '(or (zerop index) args) 'args)))
                       (with-arguments (consume-next-argument 'list)
                         (catch *catch-tag*
                           (loop for index from 0
                                 while (or (null iteration-limit)
                                           (< index iteration-limit))
                                 ,@(if oncep
                                       '(when (plusp index) do (funcall *escape-hook*))
                                       '(do (funcall *escape-hook*)))
                                 do (format-with-runtime-arguments ,(incless:client-form client)
                                                                   control)))))))))
        (let ((compiled-items (compile-items client items)))
          (cond ((and colonp at-signp)
                 ;; The remaining arguments should be lists.  Each argument
                 ;; is used in a different iteration.
                 `((catch *catch-tag*
                     (loop for index from 0
                           while (or (null iteration-limit)
                                     (< index iteration-limit))
                           ,@(if oncep
                                 '(when (plusp index) do (funcall *escape-hook*))
                                 '(do (funcall *escape-hook*)))
                           do (catch *catch-tag*
                                (with-arguments (consume-next-argument 'list)
                                  ,@compiled-items))))))
                (colonp
                 ;; We use one argument, and that should be a list of sublists.
                 ;; Each sublist is used as arguments for one iteration.
                 `((let ((arg (consume-next-argument 'list)))
                     (flet ((one-iteration (args)
                              (unless (listp args)
                                (error 'argument-type-error
                                       :expected-type 'list
                                       :datum args))
                              (catch *catch-tag*
                                (with-arguments args
                                  ,@compiled-items))))
                       (loop for args in arg ; a bit unusual naming perhaps
                             for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             do (one-iteration args))))))
                (at-signp
                 `((loop for index from 0
                         while (or (null iteration-limit)
                                   (< index iteration-limit))
                         ,@(if oncep
                               '(when (plusp index) do (funcall *escape-hook*))
                               '(do (funcall *escape-hook*)))
                         ,@(when compiled-items
                             (list* 'do compiled-items)))))
                (t
                 ;; no modifiers
                 ;; We use one argument, and that should be a list.
                 ;; The elements of that list are used by the iteration.
                 `((with-arguments (consume-next-argument 'list)
                     (catch *catch-tag*
                       (loop for index from 0
                             while (or (null iteration-limit)
                                       (< index iteration-limit))
                             ,@(if oncep
                                   '(when (plusp index) do (funcall *escape-hook*))
                                   '(do (funcall *escape-hook*)))
                             ,@(when compiled-items
                                 (list* 'do compiled-items))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.6 ~? Recursive processing

(define-directive #\? recursive-processing-directive nil (named-parameters-directive only-at-sign-mixin) ())

(define-format-directive-interpreter recursive-processing-directive
  (if at-signp
      ;; reuse the arguments from the parent control-string
      (format-with-runtime-arguments client
                                     (consume-next-argument 'string))
      ;;
      (apply #'format
             client
             *destination*
             (consume-next-argument 'string)
             (consume-next-argument 'list))))

(define-format-directive-compiler recursive-processing-directive
  (if at-signp
      ;; reuse the arguments from the parent control-string
      `((format-with-runtime-arguments ,(incless:client-form client)
                                       (consume-next-argument 'string)))
      ;;
      `((apply #'format
               ,(incless:client-form client)
               *destination*
               (consume-next-argument 'string)
               (consume-next-argument 'list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8 Miscellaneous operations

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.2 ~) End of case conversion

(define-directive #\)
    end-case-conversion-directive
    nil
    (named-parameters-directive
     no-modifiers-mixin end-structured-directive-mixin)
    ())

(define-format-directive-interpreter end-case-conversion-directive
    ;; do nothing
    nil)

(define-format-directive-compiler end-case-conversion-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.1 ~( Case conversion

(define-directive #\(
    case-conversion-directive
    end-case-conversion-directive
    (named-parameters-directive structured-directive-mixin)
    ())

(define-format-directive-interpreter case-conversion-directive
  (let ((*destination* (cond ((and colonp at-signp)
                              (make-instance 'upcase-stream :target *destination*))
                             (colonp
                              (make-instance 'capitalize-stream :target *destination*))
                             (at-signp
                              (make-instance 'first-capitalize-stream :target *destination*))
                             (t
                              (make-instance 'downcase-stream :target *destination*)))))
    (interpret-items client (aref (clauses directive) 0))))

(define-format-directive-compiler case-conversion-directive
  `((let ((*destination* ,(cond ((and colonp at-signp)
                                 '(make-instance 'upcase-stream :target *destination*))
                                (colonp
                                 '(make-instance 'capitalize-stream :target *destination*))
                                (at-signp
                                 '(make-instance 'first-capitalize-stream :target *destination*))
                                (t
                                 '(make-instance 'downcase-stream :target *destination*)))))
      ,@(compile-items client (aref (clauses directive) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.3 ~p Plural

(define-directive #\p plural-directive nil (named-parameters-directive) ())

(define-format-directive-interpreter plural-directive
  (when colonp
    (go-to-argument -1))
  (if at-signp
      (write-string (if (eql (consume-next-argument t) 1)
                        "y"
                        "ies")
                    *destination*)
      (unless (eql (consume-next-argument t) 1)
        (write-char #\s *destination*))))

(define-format-directive-compiler plural-directive
  `(,@(when colonp
        `((go-to-argument -1)))
    ,(if at-signp
         `(write-string (if (eql (consume-next-argument t) 1)
                            "y"
                            "ies")
                        *destination*)
         `(unless (eql (consume-next-argument t) 1)
            (write-char #\s *destination*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9 Miscellaneous pseudo-operations

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.1 ~; Clause separator

;;; see above

(define-directive #\; semicolon-directive nil (named-parameters-directive only-colon-mixin) ())

(define-format-directive-interpreter semicolon-directive
  nil)

(define-format-directive-compiler semicolon-directive
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.2 ~^ Escape upward

(define-directive #\^ circumflex-directive nil (named-parameters-directive)
    ((p1 :type (or character integer))
     (p2 :type (or character integer))
     (p3 :type (or character integer))))

(defmethod check-directive-syntax progn
  ((directive circumflex-directive))
  (let ((parameters (given-parameters directive)))
    (when (and (second parameters) (not (first parameters)))
      (error 'parameter-omitted
             :parameter1 1
             :parameter2 2))
    (when (and (third parameters) (not (second parameters)))
      (error 'parameter-omitted
             :parameter2 2
             :parameter3 3))))

(define-format-directive-interpreter circumflex-directive
  (let ((parameters (given-parameters directive)))
    (cond ((not (first parameters))
           (funcall *escape-hook*))
          ((not (second parameters))
           (when (eql 0 p1)
             (throw *catch-tag* nil)))
          ((not (third parameters))
           (when (eql p1 p2)
             (throw *catch-tag* nil)))
          (t
           (when (<= p1 p2 p3)
             (throw *catch-tag* nil))))))

(define-format-directive-compiler circumflex-directive
  (cond ((null p1)
         `((funcall *escape-hook*)))
        ((null p2)
         `((cond ((null p1)
                  (funcall *escape-hook*))
                 ((eql 0 p1)
                  (throw *catch-tag* nil)))))
        ((null p3)
         `((cond ((and (null p1) (null p2))
                  (funcall *escape-hook*))
                 ((or (and (null p1) (eql 0 p2))
                      (and (eql 0 p1) (null p2))
                      (and p1 p2 (eql p1 p2)))
                  (throw *catch-tag* nil)))))
        (t
         `((cond ((and (null p1) (null p2) (null p3))
                  (funcall *escape-hook*))
                 ((or (and (null p1) (null p2) (eql 0 p3))
                      (and (null p1) (eql 0 p2) (null p3))
                      (and (eql 0 p1) (null p2) (null p3))
                      (and (null p1) p2 p3 (eql p2 p3))
                      (and (null p2) p1 p3 (eql p1 p3))
                      (and (null p3) p1 p2 (eql p1 p2))
                      (and p1 p2 p3 (<= p1 p2 p3)))
                  (throw *catch-tag* nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.3 ~Newline Igored newline

(define-directive #\Newline newline-directive nil (named-parameters-directive at-most-one-modifier-mixin) ())

(define-format-directive-interpreter newline-directive
  (cond (colonp
         ;; Remove the newline but print the following whitespace.
         (let ((start (1+ (position #\Newline control-string :start start))))
           (loop for index from start below end
                 do (write-char (char control-string index) *destination*))))
        (at-signp
         ;; Print the newline, but remove the following whitespace.
         (write-char #\Newline *destination*))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))

(define-format-directive-compiler newline-directive
  (cond (colonp
         ;; Remove the newline but print the following whitespace.
         (let ((start (1+ (position #\Newline control-string :start start))))
           `((write-string ,(subseq control-string start end) *destination*))))
        (at-signp
         ;; Print the newline, but remove the following whitespace.
         `((write-char #\Newline *destination*)))
        (t
         ;; Ignore both the newline and the following whitespace.
         nil)))

;;; The reason we define this function is that the ~? directive
;;; (recursive processing), when a @ modifier is used, reuses
;;; the arguments of the parent control string, so we need
;;; to call a version of format that doesn't initialize the
;;; *arguments* runtime environment variable.
(defun format-with-runtime-arguments (client control-string)
  (let ((*catch-tag* (list nil)))
    (catch *catch-tag*
      (interpret-items client
                       (structure-items (split-control-string control-string) nil)))))

(defun format (client destination control &rest args)
  (let ((*destination* (cond ((or (streamp destination)
                                  (and (stringp destination)
                                       (array-has-fill-pointer-p destination)))
                              destination)
                             ((null destination)
                              (make-string-output-stream))
                             ((eq destination t)
                              *standard-output*)
                             (t
                              (error 'invalid-destination
                                     :destination destination)))))
    (if (functionp control)
        (apply control *destination* args)
        (with-arguments args
          (format-with-runtime-arguments client control)))
    (if (null destination)
        (get-output-stream-string *destination*)
        nil)))
