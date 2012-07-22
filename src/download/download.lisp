(in-package "MNIST-DATABASE.DOWNLOAD")

(defparameter *database-information* '(("train-images-idx3-ubyte.gz" "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz")
				       ("train-labels-idx1-ubyte.gz" "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz")
				       ("t10k-images-idx3-ubyte.gz"  "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz")
				       ("t10k-labels-idx1-ubyte.gz"  "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz"))

  "This parameter contains the information needed to download the
  entire MNIST database from http://yann.lecun.com/exdb/mnist/.

  The value of this parameter should be a list containing lists of the
  type (list LOCAL-PATHNAME URI). The first element LOCAL-NAME is a
  relative pathname which specifies where to save the data at URI.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-open-stream/maybe-close (function stream close?)
    "Call FUNCTION with STREAM. When control leaves FUNCTION, either
normally or abnormally, the STREAM is closed if and only if CLOSE? is
non-NIL."
    (unwind-protect
	 (funcall function stream)
      (when close?
	(close stream))))
  
  (defmacro with-open-stream/maybe-close ((var stream-form close-form) &body body)
    "A more convenient interface to DO-WITH-OPEN-STREAM/MAYBE-CLOSE."
    `(do-with-open-stream/maybe-close #'(lambda (,var)
					  ,@body)
       ,stream-form ,close-form)))

(defun download-file (uri pathname &key (if-exists :error))
  "Download the data at URI to PATHNAME. 

IF-EXISTS can be one of :ERROR or :SUPERSEDE."
  (with-open-file (out pathname :direction :output :element-type '(unsigned-byte 8) :if-exists if-exists)
    (multiple-value-bind (stream status header-alist reply-uri reply-stream close-stream? reason-phrase)
	(drakma:http-request uri :want-stream t)
      (declare (ignore header-alist reply-uri reply-stream))
      (unless (= 200 status)
	(error "Failed to fetch file at url ~S because ~S" uri reason-phrase))
      (with-open-stream/maybe-close (in stream close-stream?)
	(let ((buffer (make-array 1000000 :element-type '(unsigned-byte 8))))
	  (loop
	     :for bytes-read := (read-sequence buffer in)
	     :until (zerop bytes-read)
	     :do
	     (write-sequence buffer out :end bytes-read)))))
    pathname))

(defun gunzip-file (input-pathname output-pathname &key (if-exists :error) (delete-input t))
  "Uncompress the compressed gzip file at INPUT-PATHNAME to
OUTPUT-PATHNAME.

IF-EXISTS can be one of :ERROR or :SUPERSEDE.

Set DELETE-INPUT to T if the file at INPUT-PATHNAME is to be deleted
upon successful decompression."
  (when (probe-file output-pathname)
    (ecase if-exists
      (:error
       (error "File exists at path: ~A" output-pathname))
      (:supersede
       (delete-file output-pathname))))
  (gzip-stream:gunzip input-pathname output-pathname)
  (when delete-input
    (delete-file input-pathname)))

(defun download-mnist-database (&key (directory (path-to-mnist-database)) (if-exists :error))
  "Download the URIs contained in *DATABASE-INFORMATION* to DIRECTORY.

The keyword argument IF-EXISTS determines what action is to occur if
there exists a file in DIRECTORY with the same name as the name
specified in *DATABASE-INFORMATION*. IF-EXISTS be one of :ERROR
or :SUPERSEDE.

The keyword argument DIRECTORY determines the directory in which to
save the downloaded database data."
  (let ((rv nil))
    (fresh-line *standard-output*)
    (pprint-logical-block (*standard-output* nil :per-line-prefix ";; ")
      (format *standard-output* "Downloading MNIST Database.~%")
      (dolist (item *database-information*)
	(destructuring-bind (download-file-name uri) item
	  (let ((gz-pathname (merge-pathnames download-file-name directory))
		(pathname    (make-pathname :name (pathname-name download-file-name)
					    :defaults directory)))
	    (format *standard-output* " - Downloading URI ~S to ~S.~%" uri (namestring gz-pathname))
	    (download-file uri gz-pathname :if-exists if-exists)
	    (format *standard-output* " - Uncompressing ~S to ~S.~%" (namestring gz-pathname) (namestring pathname))
	    (gunzip-file gz-pathname pathname :if-exists if-exists)
	    (push pathname rv)))))
    (nreverse rv)))
