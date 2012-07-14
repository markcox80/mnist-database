(defpackage "MNIST-DATABASE"
  (:use "COMMON-LISP")
  (:export #:number-of-items
	   #:data-pathname
	   #:close-data

	   #:open-label-data
	   #:label
	   #:map-labels

	   #:do-with-label-data
	   #:with-label-data

	   #:number-of-images

	   #:open-image-data
	   #:image
	   #:map-images
	   
	   #:do-with-image-data
	   #:with-image-data))
