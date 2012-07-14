(defsystem "mnist-database"
  :author "Mark Cox"
  :description "An API for reading data from the MNIST database."
  :depends-on ("com.gigamonkeys.binary-data")
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "label-file")
				     (:file "image-file")
				     (:file "images-and-labels")
				     (:file "experiments")))))
