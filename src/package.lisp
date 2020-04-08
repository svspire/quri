(defpackage quri
  (:use :cl)
  #+(or sbcl openmcl cmu allegro)
  (:import-from #+sbcl :sb-cltl2
                #+openmcl :ccl
                #+cmu :ext
                #+allegro :sys
                :variable-information)
  (:import-from :split-sequence :split-sequence)
  (:import-from :babel-encodings
                :*default-character-encoding*)
  (:import-from :babel
                :octets-to-string
                :string-to-octets)
  (:import-from :alexandria
                :plist-hash-table
                :starts-with-subseq
                :ends-with-subseq
                :define-constant
                :delete-from-plist
                :when-let
                :when-let*
                :plist-hash-table
                :xor
                :ends-with-subseq
                :length=
                :with-gensyms)
  (:import-from :cl-utilities
                :collecting
                :collect)

  (:export :parse-uri
           :parse-scheme
           :parse-authority
           :parse-path
           :parse-query
           :parse-fragment
           :parse-domain
           :make-uri
           :uri
           :uri=
           :uri-p
           :uri-scheme
           :uri-userinfo
           :uri-host
           :uri-port
           :uri-path
           :uri-query
           :uri-fragment
           :uri-authority

           :uri-tld
           :uri-domain
           :ipv4-addr-p
           :ipv6-addr-p
           :ip-addr-p
           :ip-addr=
           :cookie-domain-p

           :urn
           :urn-p
           :urn-nid
           :urn-nss

           :uri-ftp
           :uri-ftp-p
           :uri-ftp-typecode

           :uri-http
           :uri-http-p
           :uri-query-params

           :uri-ldap
           :uri-ldap-p
           :uri-ldap-dn
           :uri-ldap-attributes
           :uri-ldap-scope
           :uri-ldap-filter
           :uri-ldap-extensions

           :uri-file
           :uri-file-p
           :uri-file-pathname

           :copy-uri
           :render-uri
           :merge-uris

           :url-decode
           :url-decode-params
           :url-encode
           :url-encode-params

           :uri-error
           :uri-malformed-string
           :uri-invalid-port
           :url-decoding-error
           :uri-malformed-urlencoded-string))