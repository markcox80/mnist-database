(in-package "MNIST-DATABASE")

(defgeneric number-of-rows-per-image (data)
  (:documentation "The number of rows in each image contained in DATA."))

(defgeneric number-of-columns-per-image (data)
  (:documentation "The number of columns in each image contained in DATA."))

(defgeneric number-of-images (data)
  (:documentation "Return the number of images contained in DATA."))

(defgeneric image (data index)
  (:documentation "Return the grayscale image in DATA at position INDEX."))

(defgeneric map-images (function data)
  (:documentation "Call FUNCTION for each image in DATA."))

(defclass image-data ()
  ((number-of-items
    :initarg :number-of-items
    :reader number-of-items)
   (rows
    :initarg :rows
    :reader number-of-rows-per-image)
   (columns
    :initarg :columns
    :reader number-of-columns-per-image)
   (stream
    :initarg :stream
    :reader data-stream)
   (pathname
    :initarg :pathname
    :reader data-pathname)))

(defmethod number-of-images ((data image-data))
  (number-of-items data))

(defun open-image-data (pathname &key (if-does-not-exist :error))
  (let ((stream (open pathname :element-type '(unsigned-byte 8) :if-does-not-exist if-does-not-exist)))
    (unless (= 2051 (read-32-bit-integer stream))
      (error "Magic number 2051 has not been found."))
    (make-instance 'image-data
		   :number-of-items (read-32-bit-integer stream)
		   :rows (read-32-bit-integer stream)
		   :columns (read-32-bit-integer stream)
		   :stream stream
		   :pathname pathname)))

(defun bytes-per-image (image-data)
  (declare (type image-data image-data))
  (* (number-of-rows-per-image image-data)
     (number-of-columns-per-image image-data)))

(defun image-data-position (image-data index)
  "Seek to the position in the data file to read image INDEX."
  (declare (type image-data image-data))
  (unless (and (>= index 0) (< index (number-of-images image-data)))
    (error "Invalid image index ~d" index))

  ;; 4 32 bit integers == 16 bytes
  (or (file-position (data-stream image-data) (+ 16 (* index (bytes-per-image image-data))))
      (error "Unable to seek to image ~d in file ~S" index (data-pathname image-data))))

(defmethod image ((image-data image-data) index) 
  (image-data-position image-data index)
  (let* ((rv (make-array (list (number-of-rows-per-image image-data)
			       (number-of-columns-per-image image-data))
			 :element-type '(unsigned-byte 8)))
	 (bytes-read (read-sequence (make-array (array-total-size rv)
				       :element-type '(unsigned-byte 8)
				       :displaced-to rv)
				    (data-stream image-data))))
    (assert (= bytes-read (bytes-per-image image-data)))
    rv))

(defmethod map-images (function (data image-data))
  (image-data-position data 0)
  (dotimes (i (number-of-images data))
    (let* ((image      (make-array (list (number-of-rows-per-image data)
					 (number-of-columns-per-image data))
				   :element-type '(unsigned-byte 8)))
	   (bytes-read (read-sequence (make-array (array-total-size image)
						  :element-type (array-element-type image)
						  :displaced-to image)
				      (data-stream data))))
      (assert (= bytes-read (array-total-size image)))
      (funcall function image)))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-image-data (function pathname &key (if-does-not-exist :error))
    (with-open-data (d (open-image-data pathname :if-does-not-exist if-does-not-exist))
      (funcall function d)))

  (defmacro with-image-data ((var pathname &key (if-does-not-exist :error)) &body body)
    `(do-with-image-data #'(lambda (,var)
			     ,@body)
       ,pathname :if-does-not-exist ,if-does-not-exist)))

(defmethod map-images (function (pathname string))
  (with-image-data (data pathname)
    (map-images function data)))
