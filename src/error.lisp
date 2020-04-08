(in-package :quri)

(define-condition uri-error (error) ())

(define-condition uri-malformed-string (uri-error)
  ((data :initarg :data)
   (position :initarg :position))
  (:report (lambda (condition stream)
             (with-slots (data position) condition
               (format stream "URI ~S contains an illegal character ~S at position ~S."
                       data (aref data position) position)))))
(define-condition uri-invalid-port (uri-malformed-string)
  ()
  (:report (lambda (condition stream)
             (with-slots (data position) condition
               (format stream "URI ~S contains an illegal character ~S at position ~S."
                       data (aref data position) position)))))

(define-condition url-decoding-error (uri-error) ())

(define-condition uri-malformed-urlencoded-string (uri-error) ())
