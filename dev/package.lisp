(in-package #:common-lisp-user)

(defpackage #:trivial-http
  (:use #:cl #:usocket)
  (:nicknames #:shttp)
  (:export
   #:*http-debug*
   #:http-get 
   #:http-post 
   #:http-head
   #:http-download
   #:http-resolve
   #:escape-url-query
           
   #:header-value
   #:header-pair
           
   ;; conditions
   #:download-error
   #:download-url
   #:download-command
   #:download-response

   #:incompatible-stream-error
   #:stream-from
   #:stream-to

   #:mismatched-download-size-error
   #:download-length-claimed
   #:download-length-downloaded
   #:*user-agent*)
  (:documentation 
   "trivial-http is a simple networking library for doing HTTP POST 
and GET over a socket interface. It establishes a package trivial-http, 
also called SHTTP, from which the following functions are exported: 
http-get, http-post, escape-url-query and http-head."))
