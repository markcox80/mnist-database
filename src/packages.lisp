(defpackage "MNIST-DATABASE"
  (:use "COMMON-LISP")
  (:export #:number-of-items
	   #:data-pathname
	   #:close-data

	   #:open-label-data
	   #:label
	   #:map-labels

	   #:do-with-label-data
	   #:with-label-data

	   #:number-of-images

	   #:open-image-data
	   #:image
	   #:map-images
	   #:number-of-rows-per-image
	   #:number-of-columns-per-image
	   #:image-dimensions
	   
	   #:do-with-image-data
	   #:with-image-data

	   #:map-images-and-labels
	   #:open-image-and-label-data
	   
	   #:do-with-image-and-label-data
	   #:with-image-and-label-data

	   #:*path-to-mnist-database*
	   #:path-to-mnist-database
	   
	   #:training-image-data-pathname
	   #:testing-image-data-pathname
	   #:training-label-data-pathname
	   #:testing-label-data-pathname
	   
	   #:map-training-data
	   #:map-testing-data

	   #:create-image-data-file
	   #:create-label-data-file)
  (:documentation "The MNIST-DATABASE package provides an API for
working with the MNIST database of handwritten digits. This database
is available from http://yann.lecun.com/exdb/mnist/. 

The data consists of a number of files that are structured using the
IDX file format. The available files are split in to two sets, the
training and testing data sets. Each set has two IDX files, one
containing the images, and one containing the image labels (i.e. which
digit the image is of).

This package provides functinality for reading the data in the label
and image files. In order to retrieve the data, the file must be
opened using one of OPEN-LABEL-DATA or OPEN-IMAGE-DATA. The object
returned is used to retrieve information from the data file. The
function CLOSE-DATA must be called on the returned object when
information from the file is no longer necessary. The macros
WITH-*-DATA are provided for convenience.

The functions MAP-LABELS and LABEL can be used to retrieve information
from label files. The LABEL function is for scenarios where random
access to the label file is required. MAP-LABELS is for sequential
access to the file. Similar functions MAP-IMAGES and IMAGE are
provided for retrieving images from image IDX files.

The function OPEN-IMAGE-AND-LABEL-DATA can be used in scenarios where
the images and labels are used simultaneously. The functions IMAGE and
LABEL defined previously can also be called on the object returned by
OPEN-IMAGE-AND-LABEL-DATA. Simultaneous sequential access to the image
and labels can be performed using MAP-IMAGES-AND-LABELS.

In addition to accessing the data, some helper functions a provided
that correspond to the experiment design defined by the database
authors. See MAP-TRAINING-DATA and MAP-TESTING-DATA. In order for
these functions to work, the variable *PATH-TO-MNIST-DATABASE* must be
set to a valid directory pathname. One can also assign logical
pathname translations for the virtual host MNIST-DATABASE. See the
documentation string for the function PATH-TO-MNIST-DATABASE for more
information.

The functions CREATE-IMAGE-DATA-FILE and CREATE-LABEL-DATA-FILE can be
used to create new IDX image and label files.
"))

; Local Variables:
; mode: lisp
; mode: flyspell
; End:
