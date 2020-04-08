(in-package :quri-test)

(defun run-benchmark ()
  (format t "~2&# QURI:URI~2%")
  (time
   (dotimes (i 100000)
     (quri:uri "http://www.ics.uci.edu/pub/ietf/uri/#Related")))
  (format t "~2&# QURI:URL-DECODE~2%")
  (time
   (dotimes (i 100000)
     (quri:url-decode "/foo%E3%81%82")))
  (format t "~2&# QURI:URL-ENCODE~2%")
  (time
   (dotimes (i 100000)
     (quri:url-encode "/fooあ"))))
