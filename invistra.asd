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
  #+(or):in-order-to #+(or)((asdf:test-op (asdf:test-op #:invistra/test)))
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
                             (:file "format")
                             (:file "compiler-macro")
                             (:file "conditions")
                             (:file "condition-reporters-en")))))

