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

(defgeneric number-of-items (data)
  (:documentation "The number of entries in the opened IDX file object
  DATA. This number is extracted from the header of the IDX file."))

(defgeneric data-stream (data)
  (:documentation "Return the stream used to extract the actual data
  contained in the IDX file."))

(defgeneric data-pathname (data)
  (:documentation "The pathname to the IDX opened IDX file."))

(defgeneric close-data (data)
  (:documentation "Close any resources required to read from DATA."))

(defgeneric map-labels (function data)
  (:documentation "Call FUNCTION for each label in DATA.

FUNCTION must accept one argument, the label. The argument is always
of type (INTEGER 0 9).

MAP-LABELS is undefined if the function LABEL is called inside
FUNCTION using the same DATA object.
"))

(defgeneric label (data index)
  (:documentation "Return the label in position INDEX of DATA.

The integer value INDEX should satisfy 0 <= INDEX < (NUMBER-OF-ITEMS DATA)"))

;; default implementations
(defmethod close-data ((data t))
  "Close the stream used to extract data from the IDX file."
  (close (data-stream data)))

;; label data files
(defclass label-data ()
  ((number-of-items
    :initarg :number-of-items
    :reader number-of-items
    :documentation "The number of items in the IDX file according to
    its header.")
   (stream
    :initarg :stream
    :reader data-stream
    :documentation "The stream used to extract data from the IDX
    file.")
   (pathname
    :initarg :pathname
    :reader data-pathname
    :documentation "The pathname to the IDX file."))
  (:documentation "This class encapsulates all resources necessary for
  retrieving information from a label IDX file.

This object can be used as the DATA argument in the label IDX protocol.

Instances of this class are createding using the OPEN-LABEL-DATA
function. All system resources allocated are released when calling
CLOSE-DATA on an instance.

Instances of this class should only be constructed from within the
MNIST-DATABASE package."))

(defun read-32-bit-integer (stream)
  "Read a 32 bit integer at the current file position in STREAM."
  (com.gigamonkeys.binary-data:read-value 'com.gigamonkeys.binary-data.common-datatypes:u4 stream))

(defun read-unsigned-byte (stream)
  "Read an unsigned byte at the current file position in STREAM."
  (com.gigamonkeys.binary-data:read-value 'com.gigamonkeys.binary-data.common-datatypes:u1 stream))

(defun open-label-data (pathname)
  "Open an IDX label file at PATHNAME for reading."
  (let ((stream (open pathname :element-type '(unsigned-byte 8))))
    (unless (= 2049 (read-32-bit-integer stream))
      (error "Magic number 2049 has not been found."))

    (make-instance 'label-data
		   :number-of-items (read-32-bit-integer stream)
		   :stream stream
		   :pathname pathname)))

(defun label-data-position (label-data index)
  "Seek to position in the label data file to read the INDEXth label.

I am not overly fond of this function name, but I have chosen it in
order to be consistent with the FILE-POSITION function."
  (declare (type label-data label-data))
  (unless (and (>= index 0) (< (number-of-items label-data)))
    (error "Label index ~d is invalid." index))
  
  ;; The position of the first label in file is 2 32 bit integers, or
  ;; 8 bytes.
  (or (file-position (data-stream label-data) (+ 8 index))
      (error "Unable to seek to label index.")))

(defun valid-label-p (label)
  "Returns T if LABEL is of type (INTEGER 0 9)."
  (typep label '(integer 0 9)))

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
    "Execute FUNCTION with DATA and return its result. When control
leaves FUNCTION, either normally or abnormally, the data object is
automatically closed using CLOSE-DATA."
    (unwind-protect
	 (funcall function data)
      (close-data data)))
  
  (defmacro with-open-data ((var data) &body body)
    "A more convenient interface to DO-WITH-OPEN-DATA."
    `(do-with-open-data #'(lambda (,var)
			    ,@body)
       ,data)))

(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defun do-with-label-data (function pathname)
    "Execute FUNCTION using an opened data object obtained by
OPEN-LABEL-DATA. When control leaves FUNCTION, either normally or
abnormally, the data object is automatically closed using CLOSE-DATA."
    (with-open-data (d (open-label-data pathname))
      (funcall function d)))

  (defmacro with-label-data ((var pathname) &body body)
    "A more convenient interface to DO-WITH-LABEL-DATA."
    `(do-with-label-data #'(lambda (,var)
			     ,@body)
       ,pathname)))

(defmethod map-labels (function (data string))
  "Open the IDX label file at pathname and call MAP-LABELS."
  (with-label-data (obj data)
    (map-labels function obj)))
