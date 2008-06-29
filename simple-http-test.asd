#|
Author: Gary King

See file COPYING for details
|#

(defpackage #:simple-http-test-system (:use #:cl #:asdf))
(in-package #:simple-http-test-system)

(defsystem simple-http-test
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for simple-http"
  :components ((:module 
		"setup"
		:pathname "tests/"
		:components 
		((:file "package")
		 (:file "tests" :depends-on ("package"))))
	       (:module 
		"tests"
		:depends-on ("setup")
		:components ()))
  :depends-on (:lift :simple-http))


