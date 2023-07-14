(cl:in-package #:asdf-user)

(defsystem "invistra-shim"
  :description "System for loading Invistra as a shim into an implementation."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("incless-native"
               "inravina-native"
               "invistra"
               "trivial-package-locks")
  :components ((:module code
                :pathname "code/shim/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
