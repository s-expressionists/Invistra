(in-package #:invistra-numeral/test)

(defclass numeral-client (incless-extrinsic:extrinsic-client) ())

(defvar *kaktovik-numeral-pattern*
  '#1=(#("𝋀" "𝋁" "𝋂" "𝋃" "𝋄"
         "𝋅" "𝋆" "𝋇" "𝋈" "𝋉"
         "𝋊" "𝋋" "𝋌" "𝋍" "𝋎"
         "𝋏" "𝋐" "𝋑" "𝋒" "𝋓")
        . #1#))

(invistra-numeral:define-numeral-directive numeral-client #\K *kaktovik-numeral-pattern*)

(defmacro my-formatter (control-string)
  (let ((incless-extrinsic:*client* (make-instance 'numeral-client)))
    (invistra:formatter incless-extrinsic:*client* control-string)))
