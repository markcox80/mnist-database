(in-package "CL-USER")

(defmethod asdf:perform ((operation asdf:test-op) (component (eql (asdf:find-system "mnist-database-tests"))))
  (lisp-unit:run-all-tests "MNIST-DATABASE.TESTS"))
