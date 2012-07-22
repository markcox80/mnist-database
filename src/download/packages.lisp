(defpackage "MNIST-DATABASE.DOWNLOAD"
  (:use "COMMON-LISP"
	"MNIST-DATABASE")
  (:export #:download-mnist-database)
  (:documentation "The MNIST-DATABASE.DOWNLOAD package provides a
  method of downloading the entire MNIST database from the database
  author's website http://yann.lecun.com/exdb/mnist/.

  The package consists of a single function
  DOWNLOAD-MNIST-DATABASE."))
