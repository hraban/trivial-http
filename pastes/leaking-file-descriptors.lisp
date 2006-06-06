;;; leaking file descriptors
;;;; http://lemonodor.com/archives/001145.html

;; This code is from 2002.
(defun connect-to-inet-socket (host port &optional (kind :stream))
  "The host may be an address string or an IP address in host order."
  (let ((socket (create-inet-socket kind))
        (hostent (or (lookup-host-entry host)
                     (error "Unknown host: ~S." host))))
    (with-alien ((sockaddr inet-sockaddr))
      (setf (slot sockaddr 'family) af-inet)
      (setf (slot sockaddr 'port) (htons port))
      (setf (slot sockaddr 'addr) (htonl (host-entry-addr hostent)))
      (when (minusp (unix:unix-connect socket
                                       (alien-sap sockaddr)
                                       (alien-size inet-sockaddr :bytes)))
        (unix:unix-close socket)
        (error "Error connecting socket to [~A:~A]: ~A"
               (host-entry-name hostent)
               port
               (unix:get-unix-error-msg)))
      socket)))
