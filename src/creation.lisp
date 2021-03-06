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

(defun write-32-bit-integer (stream integer)
  "Write the 32 bit INTEGER to STREAM according to the IDX file format."
  (declare (type (unsigned-byte 32) integer))
  (com.gigamonkeys.binary-data:write-value 'com.gigamonkeys.binary-data.common-datatypes:u4 stream integer))

(defun write-unsigned-byte (stream integer)
  "Write an unsigned byte INTEGER to STREAM."
  (declare (type (unsigned-byte 8) integer))
  (com.gigamonkeys.binary-data:write-value 'com.gigamonkeys.binary-data.common-datatypes:u1 stream integer))

(defun create-image-data-file (pathname dimensions sequence &key (key #'identity) (if-exists :error))
  "Create a new image IDX file at PATHNAME containing the images in
SEQUENCE. The images stored in the file have dimension DIMENSIONS.

The keyword KEY can be used to specify a function which obtains an
image given an element of SEQUENCE. The function is called exactly
once for each element in SEQUENCE.

The keyword IF-EXISTS determines what actions are to occur if there
exists a file at PATHNAME. If IF-EXISTS is :ERROR, then an error is
signalled. If IF-EXISTS is :SUPERSEDE, then the file is overwritten
with a new IDX file."
  (declare (type list dimensions)
	   (type (member nil :error :supersede) if-exists))
  (assert (= 2 (length dimensions)))
  (assert (every #'integerp dimensions))
  (let ((number-of-pixels (* (first dimensions) (second dimensions))))
    (with-open-file (stream pathname :direction :output :if-exists (or if-exists :error) :element-type '(unsigned-byte 8))
      (write-32-bit-integer stream 2051)
      (write-32-bit-integer stream (length sequence))
      (write-32-bit-integer stream (first dimensions))
      (write-32-bit-integer stream (second dimensions))

      (map nil #'(lambda (item)
		   (let ((image (funcall key item)))
		     (assert (equal dimensions (array-dimensions image)))
		     (assert (equal '(unsigned-byte 8) (array-element-type image)))
		     (write-sequence (make-array number-of-pixels :element-type '(unsigned-byte 8) :displaced-to image)
				     stream)))
	   sequence)))
  pathname)

(defun create-label-data-file (pathname sequence &key (key #'identity) (if-exists :error))
  "Create a new label IDX file at PATHNAME containing the labels in
SEQUENCE. 

The keyword KEY can be used to specify a function which obtains a
label given an element of SEQUENCE. The function is called exactly
once for each element in SEQUENCE.

The keyword IF-EXISTS determines what actions are to occur if there
exists a file at PATHNAME. If IF-EXISTS is :ERROR, then an error is
signalled. If IF-EXISTS is :SUPERSEDE, then the file is overwritten
with a new IDX file."
  (with-open-file (stream pathname :direction :output :if-exists (or if-exists :error) :element-type '(unsigned-byte 8))
    (write-32-bit-integer stream 2049)
    (write-32-bit-integer stream (length sequence))

    (map nil #'(lambda (item)
		 (let ((label (funcall key item)))
		   (assert (valid-label-p label))
		   (write-unsigned-byte stream label)))
	 sequence))
  nil)
