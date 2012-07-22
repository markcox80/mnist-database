;; Copyright (c) 2012, Mark Cox
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.

;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package "MNIST-DATABASE")

(defgeneric number-of-rows-per-image (data)
  (:documentation "The number of rows in each image contained in DATA."))

(defgeneric number-of-columns-per-image (data)
  (:documentation "The number of columns in each image contained in DATA."))

(defgeneric image-dimensions (data)
  (:documentation "The dimensions of the array containing the image data."))

(defgeneric number-of-images (data)
  (:documentation "The number of images contained in DATA."))

(defgeneric image (data index)
  (:documentation "Obtain the grayscale image in DATA at position INDEX.

The integer value INDEX should satisfy 0 <= INDEX < (NUMBER-OF-IMAGES DATA)."))

(defgeneric map-images (function data)
  (:documentation "Call FUNCTION for each image in DATA.

FUNCTION must accept one argument, the read image. Each time FUNCTION
is called, a new image is created. The argument passed to function is
of type (simple-array (unsigned-byte 8) (* *)).

MAP-IMAGES is undefined if IMAGE is called inside FUNCTION."))

(defclass image-data ()
  ((number-of-items
    :initarg :number-of-items
    :reader number-of-items
    :documentation "The number of items stored in the image IDX file
    according to the IDX file header.")
   (rows
    :initarg :rows
    :reader number-of-rows-per-image
    :documentation "The number of rows for each image in the IDX
    file.")
   (columns
    :initarg :columns
    :reader number-of-columns-per-image
    :documentation "The number of columns for each image in the IDX
    file.")
   (stream
    :initarg :stream
    :reader data-stream
    :documentation "The stream used to read data from the IDX file.")
   (pathname
    :initarg :pathname
    :reader data-pathname
    :documentation "The pathname to the opened IDX file."))
  (:documentation "This class encapsulates the resources necessary for
  reading information from an image IDX file.

Instances of this class can be used as the data object in the image
IDX protocol.

Instances of this class are created using OPEN-IMAGE-DATA. All system
resources allocated during OPEN-IMAGE-DATA are released when
CLOSE-DATA is invoked on an instance.

Instances of this class should only be constructed from within the
MNIST-DATABASE package."))

(defmethod number-of-images ((data image-data))
  (number-of-items data))

(defmethod image-dimensions (data)
  (list (number-of-rows-per-image data)
	(number-of-columns-per-image data)))

(defun open-image-data (pathname)
  "Open the image IDX file at PATHNAME for reading. The object
returned satisfies the image IDX object protocol."
  (let ((stream (open pathname :element-type '(unsigned-byte 8))))
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
  (defun do-with-image-data (function pathname)
    "Call FUNCTION with a data object obtained from (OPEN-IMAGE-DATA
PATHNAME). When control leaves FUNCTION, either normally or
abnormally, the data object is automatically closed using CLOSE-DATA."
    (with-open-data (d (open-image-data pathname))
      (funcall function d)))

  (defmacro with-image-data ((var pathname) &body body)
    "A more convenient interface to DO-WITH-IMAGE-DATA."
    `(do-with-image-data #'(lambda (,var)
			     ,@body)
       ,pathname)))

(defmethod map-images (function (pathname string))
  "Open the image IDX file at PATHNAME and call MAP-IMAGES on the data
object."
  (with-image-data (data pathname)
    (map-images function data)))
