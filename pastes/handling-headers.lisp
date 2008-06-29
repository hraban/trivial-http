;;; paste 12803
;;; handling headers

(defpackage :trivial-http
  (:use :cl :trivial-sockets)
  (:nicknames :thttp)
  (:export :http-get :http-post :escape-url-query))
(in-package :trivial-http)

(defparameter *user-agent* "Trivial HTTP for Common Lisp")

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
  (let ((path-start (position #\/ url :start 7)))
    (let ((port-start (position #\: url :start 7 :end path-start)))
      (if port-start (parse-integer url :start (1+ port-start) :junk-allowed t) 80))))

(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
         (host-end (min (or (position #\/ url :start 7) (length url))
                        (or port-start (length url)))))
    (subseq url 7 host-end)))

(defconstant +crlf+
  (if (boundp '+crlf+)
      (symbol-value '+crlf+)
      (concatenate 'string
                   (string (code-char 13))
                   (string (code-char 10)))))

(defun response-read-code (stream)
  (let* ((l (read-line stream))
            (space (position #\Space l)))
       (parse-integer l :start (1+ space) :junk-allowed t)))

(defun response-read-headers (stream)
  (loop for line = (read-line stream nil nil)
     until (or (eql (length line) 0)
               (eql (elt line 0) (code-char 13))
               (eql (elt line 0) (code-char 10)))
     collect
       (let ((colon (position #\: line)))
         (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
               (string-trim (list #\Space (code-char 13) (code-char 10))
                            (subseq line (1+ colon)))))))

(defun http-get (url &key headers connection)
  "headers is an alist"
  (let* ((host (url-host url))
         (port (url-port url))
         (http-headers (add-headers
                        `(("Host" . ,host) ("User-Agent" . ,*user-agent*))
                        headers :overwrite nil))
         (stream (if connection
                     connection
                     (open-stream host port))))

    ;; Send the request
    (format stream "GET ~A HTTP/1.0~A" url +crlf+)
    (write-headers http-headers stream)
    (format stream "~A" +crlf+)        ; write-headers writes a trailing +crlf+
    (force-output stream)

    ;; Read in the response
    (list
     (response-read-code stream)
     (response-read-headers stream)
     stream)))

(defun http-post (url content-type content &key headers connection)
  (let* ((host (url-host url))
         (port (url-port url))
         (http-headers (add-headers
                        `(("Host" . ,host) ("User-Agent" . ,*user-agent*)
                          ("Content-Type" . ,content-type)
                          ("Content-Length" . ,(length content)))
                        headers :overwrite nil))
         (stream (if connection
                     connection
                     (open-stream host port))))
    
    ;; Send the request
    (format stream "POST ~A HTTP/1.0~A" url +crlf+)
    (write-headers http-headers stream)
    (format stream "~A" +crlf+) ; write-headers writes a trailing +crlf+
    (format stream "~A" content)
    (force-output stream)

    ;; Read in the response
    (list
     (response-read-code stream)
     (response-read-headers stream)
     stream)))

;; this next method stolen from Araneida

(defun url-reserved-character-p (c)
  (not (or (member c '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\) ))
           (alphanumericp c))))

(defun escape-url-query (query)
  (apply #'concatenate 'string
   (loop for c across query
         if (url-reserved-character-p c)
         collect (format nil "%~2,'0X" (char-code c))
         else
         collect (string c))))

(defun write-headers (headers stream)
  (dolist (header headers)
    (format stream "~A: ~A~A" (car header) (cdr header) +crlf+)))

(defun add-headers (new-headers header-list &key (overwrite t))
  (dolist (header new-headers)
    (if (assoc (first header) header-list :test #'string=)
        (when overwrite
          (setf (cdr (assoc (car header) header-list :test #'string=))
                (cdr header)))
        (push header header-list)))
  header-list) 