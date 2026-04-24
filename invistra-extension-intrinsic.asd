(cl:in-package #:asdf-user)

(defsystem "invistra-extension-intrinsic"
  :description "System for loading Invistra intrinsically into an implementation."
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
<<<<<<< HEAD
  :depends-on ("invistra-extension"
               "inravina-extension-intrinsic")
=======
  :depends-on ("invistra"
               "inravina-extension-intrinsic"
               "quaviver/liebler")
>>>>>>> 3f00aaf (Update dependencies)
  :components ((:module code
                :pathname "code/extension-intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
