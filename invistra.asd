(cl:in-package #:asdf-user)

(defsystem :invistra
  :description "A portable and extensible Common Lisp FORMAT implementation"
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on (:acclimation)
  :in-order-to ((asdf:test-op (asdf:test-op #:invistra/test)))
  :components ((:module code
                :serial t
                :components ((:file "packages")
                             (:file "utilities")
                             (:file "generic-functions")
                             (:file "directive")
                             (:file "parse-control-string")
                             (:file "split-control-string")
                             (:file "structure-items")
                             (:file "control-string-compiler")
                             (:file "format-with-runtime-arguments")
                             (:file "conditions")
                             (:file "condition-reporters-en")))))

(defsystem :invistra/intrinsic
  :description "System for loading Invistra intrinsically into an implementation."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on (:invistra)
  :components ((:module code
                :serial t
                :components ((:file "format")
                             (:file "format-define-compiler-macro")))))

(defsystem :invistra/extrinsic
  :description "System for loading Invistra extrinsically into an implementation."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Invistra"
  :bug-tracker "https://github.com/s-expressionists/Invistra/issues"
  :depends-on (:invistra)
  :components ((:module code
                :serial t
                :components ((:file "shadow-export")
                             (:file "format")
                             (:file "format-define-compiler-macro")))))

(defsystem :invistra/test
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
