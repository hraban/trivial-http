;;; http://paste.lisp.org/display/5418
;;; download images

(defun skip-headers (stream)
  "Skips bytes in the stream until it runs into two CR-LF sequences"
  (let* ((sequence '(13 10 13 10))
         (point sequence))
    (loop while point
       do (if (= (read-byte stream) (car point))
	      (setf point (cdr point))
	      (setf point sequence))
       finally (return stream))))
     

(defun call-server (url method &key (element-type 'character) content-type content)
  (let* ((host (url-host url))
         (port (url-port url))
         (stream (open-stream host port :element-type element-type)))
    (flet ((send (string)
             (if (eq element-type 'character)
                 (princ string stream)
                 (loop for char across string
		    do (write-byte (char-code char) stream)))))

      (send (format nil
                    "~A ~A HTTP/1.0~AHost: ~A~AUser-agent: Trivial HTTP for Common Lisp~A"
                    method url +crlf+ host +crlf+ +crlf+))
      (if (eq method :post)
          (send (format nil
                        "Content-type: ~A~AContent-Length: ~D~A~A~A"
                        content-type +crlf+ (length content) +crlf+ +crlf+ content))
          (send +crlf+))
      (force-output stream)
      (if (eq element-type 'character)
          (list
           (response-read-code stream)
           (response-read-headers stream)
           stream)
          (skip-headers stream)))))

(defun http-get (url)
  (destructuring-bind (code headers stream)
      (call-server url :head)
    (close stream)
    (let ((answer-type (assoc :content-type headers)))
      (if (and answer-type (not (string= (cdr answer-type) "text/" :end1 5)))
          (list code headers (call-server url :get :element-type '(unsigned-byte 8)))
          (call-server url :get)))))
    
(defun http-post (url content-type content)
  (call-server url :post :content-type content-type :content content))
