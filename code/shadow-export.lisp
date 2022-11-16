(cl:in-package #:invistra)

(defparameter *symbols* '(#:format))

(cl:loop
   with package = (find-package '#:invistra)
   for symbol in *symbols*
   do (shadow (symbol-name symbol))
      (export (find-symbol (symbol-name symbol) package)))

