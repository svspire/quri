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
               :cl-utilities
               #+sbcl :sb-cltl2)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "quri" :depends-on ("package" "uri" "uri-classes" "domain" "parser" "decode" "encode" "error"))
                 (:file "uri" :depends-on ("package" "port"))
                 (:module "uri-classes"
                  :pathname "uri"
                  :depends-on ("package" "uri" "port" "encode" "decode")
                  :components
                  ((:file "ftp")
                   (:file "http")
                   (:file "ldap")
                   (:file "file")))
                 (:file "domain" :depends-on ("uri" "etld"))
                 (:file "etld" :depends-on ("package"))
                 (:file "parser" :depends-on ("package" "error" "util"))
                 (:file "decode" :depends-on ("package" "error" "util"))
                 (:file "encode" :depends-on ("package"))
                 (:file "port" :depends-on ("package"))
                 (:file "util" :depends-on ("package"))
                 (:file "error" :depends-on ("package")))))
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
