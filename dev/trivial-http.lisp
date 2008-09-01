(in-package #:trivial-http)

;;; ---------------------------------------------------------------------------
;;; constants
;;; ---------------------------------------------------------------------------

(defconstant +crlf+
  (if (boundp '+crlf+)
      (symbol-value '+crlf+)
      (concatenate 'string
                   (string (code-char 13))
                   (string (code-char 10)))))

;;; ---------------------------------------------------------------------------
;;; conditions
;;; ---------------------------------------------------------------------------

(define-condition trivial-http-error (error)
  ())

;; from ASDF-Install
(define-condition download-error (trivial-http-error)
  ((url :initform "?" :initarg :url :reader download-url)
   (command :initform "?" :initarg :command :reader download-command)
   (response :initform "?" :initarg :response :reader download-response))
  (:report (lambda (c s)
	     (format s "Server responded ~A for ~A ~A"
		     (download-response c)
		     (download-command c)
                     (download-url c)))))

(define-condition incompatible-stream-error (trivial-http-error)
  ((from :initarg :from :reader stream-from)
   (to :initarg :to :reader stream-to))
  (:report (lambda (c s)
	     (format s "Incompatible streams ~A and ~A." 
		     (stream-from c) (stream-to c)))))

(define-condition mismatched-download-size-error (download-error)
                  ((length-claimed 
		    :initarg :length-claimed
		    :reader download-length-claimed)
                   (length-downloaded
		    :initarg :length-downloaded
		    :reader download-length-downloaded))
  (:report (lambda (c s)
	     (format s "There was problem when downloading ~A. The header claimed size was ~D but only downloaded ~D"
		     (download-url c)
                     (download-length-claimed c) 
                     (download-length-downloaded c)))))

;;; ---------------------------------------------------------------------------
;;; http commands
;;; ---------------------------------------------------------------------------

(defun write-standard-headers (command url host stream)
  (write-string command stream)
  (write-char #\Space stream)
  (write-string url stream)
  (write-string " HTTP/1.0" stream)
  (write-crlf stream)
  (write-string "Host: " stream)
  (write-string host stream)
  (write-crlf stream)
  (write-string "User-Agent: " stream)
  (write-string *user-agent* stream)
  (write-crlf stream)
  (write-string "Accept: " stream)
  (write-string "*/*" stream)
  (write-additional-headers stream)
  (write-crlf stream)
  (write-crlf stream)
  (force-output stream))

(defun http-head (url)
  "Returns a list of two elements: a response code as an integer and an association list of headers returned from the server."
  (let* ((host (url-host url))
         (port (url-port url))
	 (socket (socket-connect host port))
	 (stream (socket-stream socket)))
    (unwind-protect
	 (progn
	   (write-standard-headers "HEAD" url host stream)
	   (prog1
	       (list
		(response-read-code stream)
		(response-read-headers stream))))
      (socket-close socket))))

(defun http-get (url)
  "returns a list of three elements: a response code as integer, an association list of headers returned from the server, and a stream from which the response can be read."
  (let* ((host (url-host url))
         (port (url-port url))
	 (socket (socket-connect host port))
	 (stream (socket-stream socket)))
    (write-standard-headers "GET" url host stream)
    (list
     (response-read-code stream)
     (response-read-headers stream)
     stream)))

;; as extensible as mud
(defun write-additional-headers (stream)
  (when (and *proxy-password* *proxy-user*)
    (write-crlf stream)
    (write-string "Proxy-Authorization: Basic" stream)
    (write-string (base64-encode
		   (format nil "~A:~A" *proxy-user* *proxy-password*)) stream)))

(defun write-crlf (stream)
  (write-char (code-char 13) stream)
  (write-char (code-char 10) stream))

(defun http-post (url content-type content &key headers (debug? *http-debug*))
  "given a URL, a MIME content type, and the content as a character 
stream, POST to the URL and return the list of three elements as 
described for [http-get][]."
  (let* ((host (url-host url))
         (port (url-port url))
	 (socket (socket-connect host port))
	 (http-stream (socket-stream socket))
	 (stream http-stream))
    (when debug?
      (setf stream (make-broadcast-stream stream debug?)))
    (format stream "POST ~A HTTP/1.0~AHost: ~A~AUser-Agent: simple HTTP for Common Lisp~A" url +crlf+ host +crlf+ +crlf+)
    (when headers
      (loop for (n . v) in headers do
	   (format stream "~A: ~A~A" n v +crlf+)))
    (format stream "Content-Type: ~A~AContent-Length: ~D~A~A~A" 
	    content-type +crlf+ (length content) +crlf+ +crlf+ content)
    (force-output stream)
    (list
     (response-read-code http-stream)
     (response-read-headers http-stream)
     http-stream)))

(defun http-resolve (url &key (http-method 'http-get)
		     (signal-error? t) (verbose? nil))
  "Similar to [http-get][], `http-resolve` returns a list of four 
elements: the HTTP response code, the headers, the stream 
and the resolved URL. HTTP-response resolves 301 and 302 
responses, and signals an error on responses greater 
than 400. If there is not an error, then the caller is responsible
for closing the HTTP stream."
  (block handler
    (handler-case 
	(destructuring-bind (response headers stream)
	    (block got
	      (loop
		 (destructuring-bind (response headers &optional stream)
		     (funcall http-method url)
		   (when verbose? 
		     (format *debug-io* "~% ~A -> ~A" url response))
		   (unless (member response '(301 302))	       
		     (return-from got (list response headers stream)))
		   (when stream 
		     (close stream))
		   (setf url (header-value :location headers)))))
	  (when (>= response 400)
	    (when stream (close stream))
	    (error 'download-error 
		   :url url :command http-method :response response))
	  (list response headers stream url))
      (error (c)
	(when signal-error?
	  (error c))
	(list nil nil nil nil)
	#+(or)
	(unless signal-error?
	  (return-from handler (list nil nil nil nil)))))))

(defun http-download (url destination &key (signal-error? t))
  "Resolves `url` using http-resolve and downloads the contents of the 
stream it to `destination`. Destination is assumed to be a file. 
Returns \(as multiple values\) the number of elements downloaded 
\(e.g., bytes\) and the actual URL."
  ;; mostly from ASDF-Install
  (destructuring-bind (response headers stream actual-url)
      (http-resolve url :signal-error? signal-error?)
    (declare (ignore response))
    (when stream
      (unwind-protect
	   (let ((length (parse-integer 
			  (or (header-value :content-length headers) "")
			  :junk-allowed t))
		 )
	     #+:clisp (setf (stream-element-type stream)
			    '(unsigned-byte 8))
	     (values
	      (download-stream stream destination :expected-length length)
	      actual-url)
	     #+(or)
	     (let ((ok? nil) (o nil))
	       (unwind-protect
		    (progn
		      (setf o (apply #'open destination
				     :direction :output :if-exists :supersede
				     (open-file-arguments)))
		      (setf total (copy-stream stream o))
		      (when length
			(unless (= length total)
			  (error 'mismatched-download-size-error
				 :length-claimed length
				 :length-downloaded total)))
		      (setf ok? t)
		      (values total actual-url))
		 (when o (close o :abort (null ok?))))))
	(close stream)))))

(defun download-stream (stream destination &key expected-length)
  (let ((ok? nil) (o nil) (retrieved 0))
    (unwind-protect
	 (progn
	   (setf o (apply #'open destination
			  :direction :output :if-exists :supersede
			  (open-file-arguments)))
	   (setf retrieved (copy-stream stream o))
	   (when expected-length
	     (unless (= expected-length retrieved)
	       (error 'mismatched-download-size-error
		      :length-claimed expected-length
		      :length-downloaded retrieved)))
	   (setf ok? t))
      (when o (close o :abort (null ok?))))
    retrieved))

;;; ---------------------------------------------------------------------------
;;; 'utilities'
;;; ---------------------------------------------------------------------------

(defun open-file-arguments ()
  (append 
   #+sbcl
   '(:external-format :latin1)
   #+(or :clisp :digitool (and :lispworks :win32))
   '(:element-type (unsigned-byte 8))))

(defun url-path (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
	 (host-end (min (or (position #\/ url :start 7) (length url))
			(or port-start (length url)))))
    (subseq url host-end)))

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
  (let ((path-start (position #\/ url :start 7)))
    (let ((port-start (position #\: url :start 7 :end path-start)))
      (if port-start
	  (parse-integer url :start (1+ port-start) :junk-allowed t)
	  80))))

(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
	 (host-end (min (or (position #\/ url :start 7) (length url))
			(or port-start (length url)))))
    (subseq url 7 host-end)))

(defun response-read-code (stream)
  (let* ((l (read-line stream))
            (space (position #\Space l)))
    (if space
      (parse-integer l :start (1+ space) :junk-allowed t)
      0)))

(defun response-read-headers (stream)
  (loop for line = (read-line stream nil nil)
     until (or (eql (length line) 0)
	       (eql (elt line 0) (code-char 13))
	       (eql (elt line 0) (code-char 10)))
     collect
       (let ((colon (position #\: line)))
         (if colon
	   (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
	         (string-trim (list #\Space (code-char 13) (code-char 10))
			      (subseq line (1+ colon))))
           nil))))

;;; SBCL has it's own but IMHO #+/#- gets crazy fast
(defvar *stream-buffer-size* 8192)

(defun copy-stream (from to)
  "Copy into TO from FROM until end of the input stream, in blocks of
*stream-buffer-size*.  The streams should have the same element type.
Returns the total number of 'elements' read and written."
  (unless (subtypep (stream-element-type to) (stream-element-type from))
    (error 'incompatible-stream-error
	   :from from :to to))
  (let ((buf (make-array *stream-buffer-size*
			 :element-type (stream-element-type from)))
        (total 0))
    (loop
      (let ((pos #-(or :clisp :cmu) (read-sequence buf from)
                 #+:clisp (ext:read-byte-sequence buf from :no-hang nil)
                 #+:cmu (sys:read-n-bytes from buf 0 *stream-buffer-size* nil)))
        (when (zerop pos) (return total))
        (incf total pos)
        (write-sequence buf to :end pos)))))

;; this next method stolen from Araneida
(defun url-reserved-character-p (c)
  (not (or (member c '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\) ))
           (alphanumericp c))))

(defun escape-url-query (query)
  "Escapes a query string in accordance with the HTTP specification."
  (apply #'concatenate 'string
   (loop for c across query
         if (url-reserved-character-p c)
         collect (format nil "%~2,'0X" (char-code c))
         else
         collect (string c))))

(defun header-value (name headers)
  "Searchers headers for name _without_ case sensitivity. Headers 
should be an alist mapping symbols to values; name a symbol. 
Returns the value if name is found or nil if it is not."
  (cdr (header-pair name headers)))

(defun header-pair (name headers)
  "Searches headers for name _without_ case sensitivity. Headers 
should be an alist mapping symbols to values; name a symbol. 
Returns the \(name value\) pair if name is found or nil if it is not."
  (assoc name headers 
         :test (lambda (a b) 
                 (string-equal (symbol-name a) (symbol-name b)))))
