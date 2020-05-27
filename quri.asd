#|
  This file is a part of quri project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem quri
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:babel
               :alexandria
               :split-sequence
               ;:cl-utilities
               #+sbcl :sb-cltl2)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "macros")
                 (:file "util")
                 (:file "error")
                 (:file "port")
                 (:file "uri")
                 (:file "encode")
                 (:file "decode")
                 (:file "etld")
                 (:file "parser")
                 (:file "domain")
                 (:module "uri-classes"
                  :serial t
                  :pathname "uri"
                  :components
                  ((:file "ftp")
                   (:file "http")
                   (:file "ldap")
                   (:file "file")))
                 (:file "quri")
                 )))
  :description "Yet another URI library for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input
                            :element-type #+lispworks :default #-lispworks 'character
                            :external-format #+clisp charset:utf-8 #-clisp :utf-8)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op quri-test))))
