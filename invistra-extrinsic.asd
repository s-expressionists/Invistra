(cl:in-package #:asdf-user)

(defsystem "invistra-extrinsic"
  :description "System for loading Invistra extrinsically into an implementation."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on ("invistra"
               "inravina-extrinsic")
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

#+(or)(defsystem :invistra-extrinsic/test
  :description "Test system for Invistra"
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on (:invistra/extrinsic :lisp-unit)
  :perform (asdf:test-op (op c) (uiop:symbol-call :invistra/test :format-test))
  :components ((:module test
                :serial t
                :components ((:file "packages")
                             (:file "format")))))
