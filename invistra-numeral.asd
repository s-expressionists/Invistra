(cl:in-package #:asdf-user)

(defsystem "invistra-numeral"
  :description "Additional numeral printers for Invistra"
  :license "BSD"
  :author "Tarn W. Burton"
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("invistra")
  :components ((:module code
                :pathname "code/numeral/"
                :serial t
                :components ((:file "packages")
                             (:file "numeral")))))

(defsystem "invistra-numeral/test"
  :description "Tests for additional numeral printers for Invistra"
  :license "BSD"
  :author "Tarn W. Burton"
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("invistra-extrinsic"
               "invistra-numeral"
               "parachute")
  :components ((:module code
                :pathname "code/numeral/test/"
                :serial t
                :components ((:file "packages")
                             (:file "directive")
                             (:file "test")))))
