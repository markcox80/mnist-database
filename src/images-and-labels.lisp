(in-package "MNIST-DATABASE")

(defgeneric image-data (data)
  (:documentation "The object encapsulating the image data."))

(defgeneric label-data (data)
  (:documentation "The object encapsulating the label data."))

(defgeneric map-images-and-labels (function data)
  (:documentation "Call FUNCTION for all images and labels in DATA.

FUNCTION must accept two arguments, the image and the
label. MAP-IMAGES-AND-LABELS is undefined if a call to IMAGE or LABEL
is made using the object DATA."))

(defclass image-and-label-data ()
  ((image-data
    :initarg :image-data
    :reader image-data
    :documentation "The object to use to retrieve images from. This
    object is obtained from OPEN-IMAGE-DATA.")

   (label-data
    :initarg :label-data
    :reader label-data
    :documentation "The object to use to retrieve labels from. This
    object is obtained from OPEN-LABEL-DATA."))
  
  (:documentation "This class encapsulates the resources necessary for
  retrieving data from coupled image and label IDX files.

Instances of this class are created using
OPEN-IMAGE-AND-LABEL-DATA. The function CLOSE-DATA must be called on
instances of IMAGE-AND-LABEL-DATA in order to release any allocated
system resources."))

(defmethod close-data ((data image-and-label-data))
  (close-data (image-data data))
  (close-data (label-data data)))

(defmethod number-of-items ((data image-and-label-data))
  (assert (= (number-of-items (label-data data))
	     (number-of-images (image-data data))))
  (number-of-items (label-data data)))

(defmethod number-of-images ((data image-and-label-data))
  (number-of-items data))

(defun open-image-and-label-data (images-pathname labels-pathname)
  "Open the image and label IDX files for reading. The object returned
satisfies both the image IDX protocol and label IDX protocol, as well
as a joint protocol."
  (let ((image-data nil)
	(label-data nil))
    (handler-case 
	(progn
	  (setf image-data (open-image-data images-pathname)
		label-data (open-label-data labels-pathname))
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
  "Redirect the IMAGE call to the stored image data object."
  (image (image-data data) index))

(defmethod label ((data image-and-label-data) index)
  "Redirect the LABEL call to the stored label data object."
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
  (defun do-with-image-and-label-data (function images-pathname labels-pathname)
    "Call FUNCTION using a data object obtained from
OPEN-IMAGE-AND-LABEL-DATA. When control leaves FUNCTION, either
normally or abnormally, the function CLOSE-DATA is called on the
created data object."
    (with-open-data (d (open-image-and-label-data images-pathname labels-pathname))
      (funcall function d)))

  (defmacro with-image-and-label-data ((var images-pathname labels-pathname) &body body)
    "A more convenient interface to DO-WITH-IMAGE-AND-LABEL-DATA."
    `(do-with-image-and-label-data #'(lambda (,var)
				       ,@body)
       ,images-pathname ,labels-pathname)))

(defmethod map-images-and-labels (function (sexp list))
  "The list SEXP contains the pathnames to the image and label IDX
files. The IDX files are opened using WITH-IMAGE-AND-LABEL-DATA and a
call to MAP-IAMGES-AND-LABELS is made."
  (labels ((operation (data)
	     (map-images-and-labels function data)))
    (apply #'do-with-image-and-label-data #'operation sexp)))

(defmethod map-images (function (data image-and-label-data))
  "Redirect MAP-IMAGES to the stored image data object."
  (map-images function (image-data data)))

(defmethod map-labels (function (data image-and-label-data))
  "Redirect MAP-LABELS to the stored label data object."
  (map-labels function (label-data data)))
