(defsystem "mnist-database-download"
  :author "Mark Cox"
  :description "Introduces a package MNIST-DATABASE.DOWNLOAD that downloads the MNIST Database."
  :depends-on ("drakma" "gzip-stream" "mnist-database")
  :components ((:module "src"
			:serial t
			:components ((:module "download"
					      :serial t
					      :components ((:file "packages")
							   (:file "download")))))))
