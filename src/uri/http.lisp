(in-package :quri)

(defstruct (uri-http (:include uri (scheme "http") (port #.(scheme-default-port "http")))))

(defstruct (uri-https (:include uri-http (scheme "https") (port #.(scheme-default-port "https")))))

(defun uri-query-params (http)
  (when-let (query (uri-query http))
    (url-decode-params query)))

(defun (setf uri-query-params) (new http)
  (setf (uri-query http) (url-encode-params new)))
