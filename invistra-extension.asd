(cl:in-package #:asdf-user)

(defsystem "invistra-extension"
  :description "Extensions for Invistra."
  :license "BSD"
  :author ("Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("invistra")
  :components ((:module code
                :pathname "code/extension/"
                :serial t
                :components ((:file "packages")
                             (:file "arguments")))))
