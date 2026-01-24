(cl:in-package #:asdf-user)

(defsystem "invistra-extension-extrinsic"
  :description "System for loading Invistra extension extrinsically into an implementation."
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("invistra-extension"
               "inravina-extension-extrinsic")
  :components ((:module code
                :pathname "code/extension-extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
