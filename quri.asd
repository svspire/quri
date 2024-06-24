(defsystem "quri"
  :version "0.7.0"
  :author "Eitaro Fukamachi"
  :maintainer "Pierre Neidhardt"
  :license "BSD 3-Clause"
  :depends-on (:babel
               :alexandria
               :split-sequence
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
  :in-order-to ((test-op (test-op quri-test))))