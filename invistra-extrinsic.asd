(cl:in-package #:asdf-user)

(defsystem "invistra-extrinsic"
  :description "System for loading Invistra extrinsically into an implementation."
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("invistra"
               "inravina-extrinsic")
  :in-order-to ((asdf:test-op (asdf:test-op "invistra-extrinsic/test")))
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(defsystem "invistra-extrinsic/test"
  :description "Test system for Invistra"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("invistra-extrinsic"
               "parachute"
               "ansi-test-harness")
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :invistra-extrinsic/test/ansi :test)
             (uiop:symbol-call :parachute :test :invistra-extrinsic/test/regression))
  :components ((:module code
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "format")
                             (:file "ansi-test")
                             (:static-file "expected-failures.sexp")))))
