;;;; Silly emacs, this is -*- Lisp -*- (or thereabouts)
(in-package #:common-lisp-user)

(defpackage #:simple-http-asdf 
  (:use #:common-lisp #:asdf))

(in-package #:simple-http-asdf)

;;this is necessary due to a bug in SBCL
#+sbcl
(require :sb-bsd-sockets)

(defsystem simple-http
    :name "simple-http"
    :author "Brian Mastenbrook"
    :licence "MIT"
    :description "Simple support for HTTP GET and POST."
    :depends-on (:trivial-sockets)
    :components ((:module "dev"
                          :components ((:file "simple-http")
                                       
                                       (:static-file "notes.text")))))