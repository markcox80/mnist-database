(in-package "MNIST-DATABASE")

(defvar *path-to-mnist-database* nil
  "Set to a directory that contains the MNIST database.")

(defun path-to-mnist-database ()
  (or *path-to-mnist-database*
      (handler-case (make-pathname :host "MNIST-DATABASE"
				   :directory '(:absolute))
	(error ()
	  nil))
      (error "MNIST-DATABASE:*PATH-TO-MNIST-DATABASE* and is unset. MNIST-DATABASE logical host can also be used.")))

(defun training-image-data-pathname ()
  (make-pathname :name "train-images-idx3-ubyte"
		 :defaults (path-to-mnist-database)))

(defun training-label-data-pathname ()
  (make-pathname :name "train-labels-idx1-ubyte"
		 :defaults (path-to-mnist-database)))

(defun testing-image-data-pathname ()
  (make-pathname :name "t10k-images-idx3-ubyte"
		 :defaults (path-to-mnist-database)))

(defun testing-label-data-pathname ()
  (make-pathname :name "t10k-labels-idx1-ubyte"
		 :defaults (path-to-mnist-database)))

(defun map-training-data (function)
  "Call FUNCTION with each image and label available in the MNIST training data."
  (with-image-and-label-data (data (training-image-data-pathname)
				   (training-label-data-pathname))
    (map-images-and-labels function data)))

(defun map-testing-data (function)
  "Call FUNCTION with each image and a label available in the MNIST testing data."
  (with-image-and-label-data (data (testing-image-data-pathname)
				   (testing-label-data-pathname))
    (map-images-and-labels function data)))
