(in-package :quri)

(defstruct (uri (:constructor %make-uri)
                (:copier %copy-uri))
  scheme
  userinfo
  host
  port
  path
  query
  fragment)

(defun make-basic-uri (&key scheme userinfo host port path query fragment)
  (let ((uri (%make-uri :scheme scheme
                        :userinfo userinfo
                        :host host
                        :port port
                        :path path
                        :query query
                        :fragment fragment)))
    (unless (uri-port uri)
      (setf (uri-port uri)
            (scheme-default-port (uri-scheme uri))))
    uri))

(defun uri-authority (uri)
  (when (uri-host uri)
    (let ((default-port (scheme-default-port (uri-scheme uri))))
      (format nil "~:[~;~:*~A@~]~A~:[:~A~;~*~]"
              (uri-userinfo uri)
              (uri-host uri)
              (eql (uri-port uri) default-port)
              (uri-port uri)))))

(defstruct (urn (:include uri (scheme :urn))
                (:constructor %make-urn))
  nid
  nss)

(defun make-urn (&rest initargs)
  (let ((urn (apply #'%make-urn initargs)))
    (when (uri-path urn)
      (let ((colon-pos (position #\: (uri-path urn))))
        (if colon-pos
            (setf (urn-nid urn) (subseq (uri-path urn) 0 colon-pos)
                  (urn-nss urn) (subseq (uri-path urn) (1+ colon-pos)))
            (setf (urn-nid urn) (uri-path urn)))))
    urn))
