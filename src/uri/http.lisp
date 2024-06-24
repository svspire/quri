(in-package :quri)

(defstruct (uri-http (:include uri (scheme "http") (port #.(scheme-default-port "http")))))

(defstruct (uri-https (:include uri-http (scheme "https") (port #.(scheme-default-port "https")))))

(defun uri-query-params (http &key (lenient t) (percent-decode t))
  (when-let (query (uri-query http))
    (url-decode-params query
                       :lenient lenient
                       :percent-decode percent-decode)))

(defun (setf uri-query-params) (new http &key lenient (percent-encode t))
  (declare (ignore lenient))
  (setf (uri-query http)
        (if new
            (url-encode-params
             new :percent-encode percent-encode)
            nil)))
