;;;; Silly emacs, this is -*- Lisp -*- (or thereabouts)
(in-package #:common-lisp-user)

(defpackage #:simple-http-system 
  (:use #:common-lisp #:asdf))

(in-package #:simple-http-system)

;;this is necessary due to a bug in SBCL
#+sbcl
(require :sb-bsd-sockets)

(defsystem simple-http
  :name "simple-http"
  :author "Brian Mastenbrook and Gary King"
  :licence "MIT"
  :description "Simple support for HTTP GET, POST and more."
  :version "1.1.0"
  :depends-on (:usocket)
  :components ((:module 
		"dev"
		:components ((:file "package")
			     (:file "variables"
				    :depends-on ("package"))
			     (:file "simple-http" 
				    :depends-on ("variables"))
			     (:file "base64"
				    :depends-on ("variables"))
			     (:static-file "notes.text"))))
  :in-order-to ((test-op (load-op simple-http-test)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic))
  :depends-on ())

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'simple-http))))
  (values nil))
