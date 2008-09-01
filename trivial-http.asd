;;;; Silly emacs, this is -*- Lisp -*- (or thereabouts)
(in-package #:common-lisp-user)

(defpackage #:trivial-http-system 
  (:use #:common-lisp #:asdf))

(in-package #:trivial-http-system)

;;this is necessary due to a bug in SBCL
#+sbcl
(require :sb-bsd-sockets)

(defsystem trivial-http
  :name "trivial-http"
  :author "Brian Mastenbrook and Gary King"
  :licence "MIT"
  :description "Simple support for HTTP GET, POST and more."
  :version "1.3.0"
  :depends-on (:usocket)
  :components ((:module 
		"dev"
		:components ((:file "package")
			     (:file "variables"
				    :depends-on ("package"))
			     (:file "trivial-http" 
				    :depends-on ("variables"))
			     (:file "base64"
				    :depends-on ("variables"))
			     (:static-file "notes.text"))))
  :in-order-to ((test-op (load-op trivial-http-test)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic))
  :depends-on ())

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'trivial-http))))
  (values nil))
