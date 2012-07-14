(in-package "MNIST-DATABASE")

(defgeneric number-of-items (data)
  (:documentation "Returns the number of items in DATA."))

(defgeneric data-stream (data)
  (:documentation "Return the stream used to extract the actual data
  contained in DATA."))

(defgeneric data-pathname (data)
  (:documentation ""))

(defgeneric close-data (data)
  (:documentation "Close resources used to read from DATA."))

(defgeneric map-labels (function data)
  (:documentation "Call FUNCTION for each label in DATA."))

(defgeneric label (data index)
  (:documentation "Return the label at INDEX."))

;; default implementations
(defmethod close-data ((data t))
  (close (data-stream data)))

;; label data files
(defclass label-data ()
  ((number-of-items
    :initarg :number-of-items
    :reader number-of-items)
   (stream
    :initarg :stream
    :reader data-stream)
   (pathname
    :initarg :pathname
    :reader data-pathname)))

(defun read-32-bit-integer (stream)
  (com.gigamonkeys.binary-data:read-value 'com.gigamonkeys.binary-data.common-datatypes:u4 stream))

(defun read-unsigned-byte (stream)
  (com.gigamonkeys.binary-data:read-value 'com.gigamonkeys.binary-data.common-datatypes:u1 stream))

(defun open-label-data (pathname &key (if-does-not-exist :error))
  (let ((stream (open pathname :element-type '(unsigned-byte 8) :if-does-not-exist if-does-not-exist)))
    (unless (= 2049 (read-32-bit-integer stream))
      (error "Magic number 2049 has not been found."))

    (make-instance 'label-data
		   :number-of-items (read-32-bit-integer stream)
		   :stream stream
		   :pathname pathname)))

(defun label-data-position (label-data index)
  "Seek to position in the label data file to read the INDEXth label."
  (declare (type label-data label-data))
  (unless (and (>= index 0) (< (number-of-items label-data)))
    (error "Label index ~d is invalid." index))
  
  ;; The position of the first label in file is 2 32 bit integers, or
  ;; 8 bytes.
  (or (file-position (data-stream label-data) (+ 8 index))
      (error "Unable to seek to label index.")))

(defun valid-label-p (label)
  (and (>= label 0)
       (<= label 9)))

(defmethod label ((data label-data) index)
  (label-data-position data index)
  (let ((rv (read-unsigned-byte (data-stream data))))
    (assert (valid-label-p rv))
    rv))

(defmethod map-labels (function (data label-data))   
  (label-data-position data 0)
  (dotimes (i (number-of-items data))
    (let ((label (read-unsigned-byte (data-stream data))))
      (assert (valid-label-p label))
      (funcall function label)))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-open-data (function data)
    (unwind-protect
	 (funcall function data)
      (close-data data)))

  (defmacro with-open-data ((var data) &body body)
    `(do-with-open-data #'(lambda (,var)
			    ,@body)
       ,data)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-label-data (function pathname &key (if-does-not-exist :error))
    (with-open-data (d (open-label-data pathname :if-does-not-exist if-does-not-exist))
      (funcall function d)))

  (defmacro with-label-data ((var pathname &key (if-does-not-exist :error)) &body body)
    `(do-with-label-data #'(lambda (,var)
			     ,@body)
       ,pathname :if-does-not-exist ,if-does-not-exist)))

(defmethod map-labels (function (data string))
  (with-label-data (obj data)
    (map-labels function obj)))
