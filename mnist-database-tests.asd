(defsystem "mnist-database-tests"
  :author "Mark Cox"
  :description "A collection of tests for the mnist-database system."
  :depends-on ("mnist-database" "lisp-unit" "temporary-file")
  :serial t
  :components ((:module "tests"
			:serial t
			:components ((:file "packages")
				     (:file "asdf")
				     (:file "creation")))))
