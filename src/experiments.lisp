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

(defvar *path-to-mnist-database* nil
  "Set to a directory that contains the MNIST database.

This value should be not used directly, please use the function
PATH-TO-MNIST-DATABASE.")

(defun path-to-mnist-database ()
  "Obtain the directory pathname to the files found on the MNIST
database website (http://yann.lecun.com/exdb/mnist/).

The path returned is determined in the following order (i) the value
of *PATH-TO-MNIST-DATABASE* (ii) a logical pathname using the host
MNIST-DATABASE.

An error is signalled if no path can be determined."
  (or *path-to-mnist-database*
      (handler-case (make-pathname :host "MNIST-DATABASE"
				   :directory '(:absolute))
	(error ()
	  nil))
      (error "MNIST-DATABASE:*PATH-TO-MNIST-DATABASE* and is unset. MNIST-DATABASE logical host can also be used.")))

(defun training-image-data-pathname ()
  "Return the pathname of the image data that is used during
training."
  (make-pathname :name "train-images-idx3-ubyte"
		 :defaults (path-to-mnist-database)))

(defun training-label-data-pathname ()
  "Return the pathname of the label data that is used during
training."
  (make-pathname :name "train-labels-idx1-ubyte"
		 :defaults (path-to-mnist-database)))

(defun testing-image-data-pathname ()
  "Return the pathname of the image data to be used during testing."
  (make-pathname :name "t10k-images-idx3-ubyte"
		 :defaults (path-to-mnist-database)))

(defun testing-label-data-pathname ()
  "Return the pathname of the label data to be used during testing."
  (make-pathname :name "t10k-labels-idx1-ubyte"
		 :defaults (path-to-mnist-database)))

(defun map-training-data (function)
  "Call FUNCTION with each image and label available in the MNIST training data.

FUNCTION must accept two arguments, the training data image and its
corresponding label.

See MAP-IMAGES-AND-LABELS for any other requirements on FUNCTION."
  (with-image-and-label-data (data (training-image-data-pathname)
				   (training-label-data-pathname))
    (map-images-and-labels function data)))

(defun map-testing-data (function)
  "Call FUNCTION with each image and a label available in the MNIST
testing data.

FUNCTION must accept two arguments, the testing data image and its
corresponding label.

See MAP-IMAGES-AND-LABELS for any other requirements on FUNCTION."
  (with-image-and-label-data (data (testing-image-data-pathname)
				   (testing-label-data-pathname))
    (map-images-and-labels function data)))
