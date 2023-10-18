(cl:in-package #:asdf-user)

(defsystem :invistra-intrinsic
  :description "System for loading Invistra intrinsically into an implementation."
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on (:invistra)
  :components ((:module code
                :pathname "code/intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
