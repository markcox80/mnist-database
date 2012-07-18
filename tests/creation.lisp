(in-package "MNIST-DATABASE.TESTS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-temporary-pathname (function)
    (let ((temporary-pathname (temporary-file:with-open-temporary-file (out :keep t)
				(truename out))))
      (unwind-protect
	   (funcall function temporary-pathname)
	(when (probe-file temporary-pathname)
	  (delete-file temporary-pathname)))))

  (defmacro with-temporary-pathname ((var) &body body)
    `(do-with-temporary-pathname #'(lambda (,var)
				     ,@body))))

(defun convert-to-image (array)
  (assert (= 2 (array-rank array)))
  (let ((rv (make-array (array-dimensions array) :element-type '(unsigned-byte 8))))
    (dotimes (i (array-dimension array 0))
      (dotimes (j (array-dimension array 1))
	(setf (aref rv i j) (aref array i j))))
    rv))

(defun image-equal (a b)
  (and (equal (array-dimensions a)
	      (array-dimensions b))
       (block element-check
	 (dotimes (i (array-dimension a 0))
	   (dotimes (j (array-dimension a 1))
	     (unless (= (aref a i j)
			(aref b i j))
	       (return-from element-check nil))))
	 t)))

(define-test creation/images
  (let ((images (list #2A((1 2 3) (4 5 6))
		      #2A((6 5 4) (3 2 1))
		      #2A((1 6 2) (5 3 4)))))
    (with-temporary-pathname (path)
      ;; cannot open this file because WITH-TEMPORARY-PATHNAME creates the file.
      (assert-error 'error (mnist-database:create-image-data-file path '(2 3) images))

      ;; cannot save images as the (ARRAY-ELEMENT-TYPE IMAGES) is not of type (unsigned-byte 8).
      (assert-error 'error (mnist-database:create-image-data-file path '(2 3) images :if-exists :supersede))

      (mnist-database:create-image-data-file path '(2 3) images
					     :key #'convert-to-image
					     :if-exists :supersede)

      (mnist-database:with-image-data (data path)
	(assert-equal 3 (mnist-database:number-of-images data))
	(assert-equal 3 (mnist-database:number-of-items data))

	(assert-equal '(2 3) (mnist-database:image-dimensions data))
	
	;; sanity check on IMAGE-EQUAL
	(assert-false (image-equal (elt images 0) (elt images 1)))

	;; check random access
	(dolist (i (list 2 1 1 0 2))	  
	  (assert-true (image-equal (elt images i)
				    (mnist-database:image data i))))

	;; check sequential access
	(let ((index 0))
	  (mnist-database:map-images #'(lambda (image)
					 (assert-true (image-equal (elt images index)
								   image))
					 (incf index))
				     data))))))

(define-test creation/labels
  (let ((image-labels (list 0 1 2 3)))
    (with-temporary-pathname (path)
      ;; cannot open this file becase WITH-TEMPORARY-PATHNAME creates the file.
      (assert-error 'error (mnist-database:create-label-data-file path image-labels))
      
      ;; cannot create this file as the image labels are not between 0 and 9
      (assert-error 'error (mnist-database:create-label-data-file path image-labels :key #'(lambda (x)
											     (+ x 10))))

      (mnist-database:create-label-data-file path image-labels :if-exists :supersede)

      (mnist-database:with-label-data (data path)
	(assert-equal 4 (mnist-database:number-of-items data))
	
	;; check random access
	(dolist (i (list 3 1 2 0 3))
	  (assert-equal (elt image-labels i) (mnist-database:label data i)))

	;; check sequential access
	(let ((index 0))
	  (mnist-database:map-labels #'(lambda (label)
					 (assert-equal (elt image-labels index) label)
					 (incf index))
				     data))))))
