(in-package :quri)

(defstruct (uri (:constructor %make-uri)
                (:copier %copy-uri))
  (scheme nil :read-only t)
  userinfo
  host
  port
  path
  query
  fragment)

(defmethod make-load-form ((object uri) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defun make-basic-uri (&rest args &key scheme userinfo host port path query fragment)
  (declare (ignore scheme userinfo host port path query fragment))
  (let ((uri (apply #'%make-uri args)))
    (unless (uri-port uri)
      (setf (uri-port uri) (scheme-default-port (uri-scheme uri))))
    (when (pathnamep (uri-path uri))
      (setf (uri-path uri)
            (uiop:native-namestring (uri-path uri))))
    uri))

(defun uri-authority (uri)
  (when (uri-host uri)
    (let ((default-port (scheme-default-port (uri-scheme uri))))
      (with-standard-io-syntax
        (format nil "~:[~;~:*~A@~]~A~:[:~A~;~*~]"
                (uri-userinfo uri)
                (uri-host uri)
                (eql (uri-port uri) default-port)
                (uri-port uri))))))

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
