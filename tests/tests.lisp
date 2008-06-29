(in-package #:simple-http-test)

(deftestsuite simple-http-test ()
  ())

(deftestsuite test-http-resolve (simple-http-test)
  ())

(addtest (test-http-resolve)
  common-lisp.net
  (destructuring-bind (code headers stream url)
      (http-resolve "http://common-lisp.net/")
    (declare (ignore headers))
    (unwind-protect
	 (progn
	   (ensure-same url "http://common-lisp.net/" :test 'string=)
	   (ensure-same code 200))
      (close stream))))

(addtest (test-http-resolve)
  www.common-lisp.net
  (destructuring-bind (code headers stream url)
      (http-resolve "http://www.common-lisp.net/")
    (declare (ignore headers))
    (unwind-protect
	 (progn
	   (ensure-same url "http://common-lisp.net/" :test 'string=)
	   (ensure-same code 200))
      (close stream))))

(addtest (test-http-resolve)
  www.common-lisp.net/
  (destructuring-bind (code headers stream url)
      (http-resolve "http://www.common-lisp.net")
    (declare (ignore headers))
    (unwind-protect
	 (progn
	   (ensure-same url "http://common-lisp.net/" :test 'string=)
	   (ensure-same code 200))
      (close stream))))




