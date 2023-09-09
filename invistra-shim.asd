(cl:in-package #:asdf-user)

(defsystem "invistra-shim"
  :description "System for loading Invistra as a shim into an implementation."
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("incless-native"
               "inravina-native"
               "invistra"
               "trivial-package-locks")
  :in-order-to ((test-op (test-op "invistra-shim/test")))
  :components ((:module code
                :pathname "code/shim/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(defsystem "invistra-shim/test"
  :description "ANSI Test system for Invistra"
  :license "MIT"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :depends-on ("alexandria" "invistra-shim")
  :perform (test-op (op c)
             (symbol-call :invistra-shim/test :test))
  :components ((:module "code"
                :pathname "code/shim/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))
               (:module "expected-failures"
                :pathname "code/shim/test/expected-failures"
                :components ((:static-file "default.sexp")
                             (:static-file "clasp.sexp")
                             (:static-file "ecl.sexp")
                             (:static-file "sbcl.sexp")))))
