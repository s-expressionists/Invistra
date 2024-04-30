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

(asdf:defsystem "invistra-extrinsic/unit-test"
  :description "Unit testing suite for Invistra."
  :author "Tarn W. Burton"
  :license "BSD"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("alexandria"
               "invistra-extrinsic"
               "parachute")
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :invistra-extrinsic/unit-test))
  :components ((:module code
                :pathname "code/extrinsic/unit-test/"
                :serial t
                :components ((:file "packages")
                             (:file "utilities")
                             (:file "test")
                             (:file "cdr-7")))))

(defsystem "invistra-extrinsic/ansi-test"
  :description "ANSI Test system for Invistra"
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
             (uiop:symbol-call :invistra-extrinsic/test/ansi :test))
  :components ((:module code
                :pathname "code/extrinsic/ansi-test/"
                :serial t
                :components ((:file "packages")
                             (:file "format")
                             (:file "test")
                             (:static-file "expected-failures.sexp")))))
