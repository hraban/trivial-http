(in-package #:trivial-http-test)

(deftestsuite trivial-http-test ()
  ())

(deftestsuite test-http-resolve (trivial-http-test)
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

;;;;

(deftestsuite test-http-head (trivial-http-test)
  ())

(addtest (test-http-head)
  bad-url
  ;;?? probably want our own condition
  (ensure-error
    (http-head "http://asdfdaf")))

(addtest (test-http-head)
  url-needs-resolution
  (destructuring-bind (response headers)
      (http-head "http://www.common-lisp.net")
    (ensure-same response 302 :test #'=)
    (ensure-same (search "text/html"
			 (header-value :content-type headers) :test 'char=)
		 0
		 :test '=)))

(addtest (test-http-head)
  url-good
  (destructuring-bind (response headers)
      (http-head "http://common-lisp.net")
    (ensure-same response 200 :test #'=)
    (ensure-same (search "text/html"
			 (header-value :content-type headers) :test 'char=)
		 0
		 :test '=)))

;;;;


(deftestsuite test-http-get (trivial-http-test)
  ())

(addtest (test-http-get)
  bad-url
  ;;?? probably want our own condition
  (ensure-error
    (http-get "http://asdfdaf")))

(addtest (test-http-get)
  url-needs-resolution
  (destructuring-bind (response headers stream)
      (http-get "http://www.common-lisp.net")
    (unwind-protect
	 (progn
	   (ensure-same response 302 :test #'=)
	   (ensure-same 
	    (search "text/html"
		    (header-value :content-type headers) :test 'char=)
	    0
	    :test '=)
	   (ensure (< (trivial-http::download-stream stream "/tmp/x.test") 
		      1024)))
      (close stream))))

(addtest (test-http-get)
  url-good
  (destructuring-bind (response headers stream)
      (http-get "http://common-lisp.net")
    (unwind-protect
	 (progn
	   (ensure-same response 200 :test #'=)
	   (ensure-same 
	    (search "text/html"
		    (header-value :content-type headers) :test 'char=)
	    0
	    :test '=)
	   (ensure (> (trivial-http::download-stream stream "/tmp/x.test") 
		      1024)))
      (close stream))))
