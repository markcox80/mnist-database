(in-package "MNIST-DATABASE")

(defgeneric image-data (data)
  (:documentation "The object encapsulating the image data."))

(defgeneric label-data (data)
  (:documentation "The object encapsulating the label data."))

(defgeneric map-images-and-labels (function data)
  (:documentation "Call FUNCTION for all images and labels in DATA."))

(defclass image-and-label-data ()
  ((image-data
    :initarg :image-data
    :reader image-data)
   (label-data
    :initarg :label-data
    :reader label-data)))

(defmethod close-data ((data image-and-label-data))
  (close-data (image-data data))
  (close-data (label-data data)))

(defmethod number-of-items ((data image-and-label-data))
  (assert (= (number-of-items (label-data data))
	     (number-of-images (image-data data))))
  (number-of-items (label-data data)))

(defmethod number-of-images ((data image-and-label-data))
  (number-of-items data))

(defun open-image-and-label-data (images-pathname labels-pathname &key (if-does-not-exist :error))
  (let ((image-data nil)
	(label-data nil))
    (handler-case 
	(progn
	  (setf image-data (open-image-data images-pathname :if-does-not-exist if-does-not-exist)
		label-data (open-label-data labels-pathname :if-does-not-exist if-does-not-exist))
	  (unless (= (number-of-items label-data)
		     (number-of-images image-data))
	    (error "Number of images and items do not match."))

	  (make-instance 'image-and-label-data
			 :image-data image-data
			 :label-data label-data))
      (error (c)
	(when image-data
	  (close-data image-data))
	(when label-data
	  (close-data label-data))
	(error c)))))

(defmethod image ((data image-and-label-data) index)
  (image (image-data data) index))

(defmethod label ((data image-and-label-data) index)
  (label (label-data data) index))

(defmethod map-images-and-labels (function (data image-and-label-data))
  (let ((image-data (image-data data))
	(label-data (label-data data)))

    (image-data-position image-data 0)
    (label-data-position label-data 0)

    (dotimes (i (number-of-items data))
      (let* ((image (make-array (list (number-of-rows-per-image image-data)
				      (number-of-columns-per-image image-data))
				:element-type '(unsigned-byte 8)))
	     (bytes-read (read-sequence (make-array (array-total-size image)
						    :element-type (array-element-type image)
						    :displaced-to image)
					(data-stream image-data))))
	(assert (= bytes-read (bytes-per-image image-data)))
	(funcall function image (read-unsigned-byte (data-stream label-data))))))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-image-and-label-data (function images-pathname labels-pathname &key (if-does-not-exist :error))
    (with-open-data (d (open-image-and-label-data images-pathname labels-pathname :if-does-not-exist if-does-not-exist))
      (funcall function d)))

  (defmacro with-image-and-label-data ((var images-pathname labels-pathname &key (if-does-not-exist :error)) &body body)
    `(do-with-image-and-label-data #'(lambda (,var)
				       ,@body)
       ,images-pathname ,labels-pathname :if-does-not-exist ,if-does-not-exist)))

(defmethod map-images-and-labels (function (sexp list))
  (labels ((operation (data)
	     (map-images-and-labels function data)))
    (apply #'do-with-image-and-label-data #'operation sexp)))

(defmethod map-images (function (data image-and-label-data))
  (map-images function (image-data data)))

(defmethod map-labels (function (data image-and-label-data))
  (map-labels function (label-data data)))
