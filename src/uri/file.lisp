(in-package :quri)

(defstruct (uri-file (:include uri (scheme "file") (port nil))
                     (:constructor %make-uri-file)))

(defun make-uri-file (&rest initargs &key path &allow-other-keys)
  (when (pathnamep path)
    (setf (getf initargs :path)
          (uiop:native-namestring path)))
  (apply #'%make-uri-file initargs))

(declaim (ftype (function (uri-file) pathname) uri-file-pathname))
(defun uri-file-pathname (file)
  "Get a lisp pathname object from a file URI.
Assumes that the path of the file URI is correct path syntax for the environment."
  (parse-namestring (uri-path file)))
