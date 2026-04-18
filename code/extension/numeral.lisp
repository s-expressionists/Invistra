(in-package #:invistra-extension)

(defclass numeral-directive (invistra:directive)
  ())

;; binds
(defmethod invistra:parameter-specifications ((client client) (directive numeral-directive))
  '((:type character
     :default #\A)
    (:type integer
     :default 0)
    (:type character
     :default #\Space)
    (:type character
     :default #\,)
    (:type integer
     :default 3)))

(defmethod invistra:specialize-directive ((client client) (char (eql #\N)) directive)
  (change-class directive 'numeral-directive))

(defgeneric numeral-pattern (name))

(defmethod numeral-pattern ((name (eql #\A)))
  '#1=(#("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
        . #1#))

(defmethod numeral-pattern ((name (eql #\O)))
  '(#(nil "I" "II" "III" "IIII")
    #("" "V")
    #("" "X" "XX" "XXX" "XXXX")
    #("" "L")
    #("" "C" "CC" "CCC" "CCCC")
    #("" "D")
    #("" "M" "MM" "MMM" "MMMM")))

(defmethod numeral-pattern ((name (eql #\R)))
  '(#(nil "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX")
    #("" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC")
    #("" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM")
    #("" "M" "MM" "MMM" "MMMM")))

;;; etruscan
(defmethod numeral-pattern ((name (eql #\E)))
  '(#(nil "𐌠" "𐌠𐌠" "𐌠𐌠𐌠" "𐌠𐌠𐌠𐌠")
    #("" "𐌡")
    #("" "𐌢" "𐌢𐌢" "𐌢𐌢𐌢" "𐌢𐌢𐌢𐌢")
    #("" "𐌣")
    #("" "𐌟" "𐌟𐌟" "𐌟𐌟𐌟" "𐌟𐌟𐌟𐌟")))

;;; Kaktovik
(defmethod numeral-pattern ((name (eql #\K)))
  '#1=(#("𝋀" "𝋁" "𝋂" "𝋃" "𝋄"
         "𝋅" "𝋆" "𝋇" "𝋈" "𝋉"
         "𝋊" "𝋋" "𝋌" "𝋍" "𝋎"
         "𝋏" "𝋐" "𝋑" "𝋒" "𝋓")
        . #1#))

(defmethod numeral-pattern ((name (eql #\M)))
  '#1=(#("𝋠" "𝋡" "𝋢" "𝋣" "𝋤"
         "𝋥" "𝋦" "𝋧" "𝋨" "𝋩"
         "𝋪" "𝋫" "𝋬" "𝋭" "𝋮"
         "𝋯" "𝋰" "𝋱" "𝋲" "𝋳")
        . #1#))

(defun print-numeral-arg (client colon-p at-sign-p name mincol padchar commachar comma-interval)
  (prog ((q (invistra:pop-argument))
         (pattern (numeral-pattern (char-upcase name)))
         (r 0)
         (c 0)
         parts
         (comma-part (string commachar))
         result
         pad-length
         place)
   repeat
     (setq place (pop pattern))
     (multiple-value-setq (q r)
       (floor q (length place)))
     (push (aref place r) parts)
     (unless (zerop q)
       (when colon-p
         (setq c (mod (incf c) comma-interval))
         (when (zerop c)
           (push (string comma-part) parts)))
       (go repeat))
     (setf result (apply #'concatenate 'string parts)
           pad-length (max 0
                           (- mincol
                              (inravina:stream-measure-string invistra:*format-output*
                                                              result))))
     (write-string result invistra:*format-output*)))

(defmethod invistra:interpret-item
    ((client client) (directive numeral-directive) &optional parameters)
  (apply #'print-numeral-arg client
         (invistra:colon-p directive) (invistra:at-sign-p directive)
         parameters))

(defmethod invistra:compile-item
    ((client client) (directive numeral-directive) &optional parameters)
  `((print-numeral-arg ,(trinsic:client-form client)
                       ,(invistra:colon-p directive) ,(invistra:at-sign-p directive)
                       ,@parameters)))
