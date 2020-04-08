(in-package :quri)

(defun standard-alpha-char-p (char)
  (declare (type character char)
           (optimize (speed 3) (safety 0)))
  (standard-alpha-byte-p (char-code char)))

(defun standard-alpha-byte-p (byte)
  (declare (type (unsigned-byte 8) byte)
           (optimize (speed 3) (safety 0)))
  (or (<= #.(char-code #\A) byte #.(char-code #\Z))
      (<= #.(char-code #\a) byte #.(char-code #\z))))

(defun standard-alphanumeric-p (char)
  (declare (type character char)
           (optimize (speed 3) (safety 0)))
  (or (digit-char-p char)
      (standard-alpha-char-p char)))

(defun standard-alphanumeric-byte-p (byte)
  (declare (type (unsigned-byte 8) byte)
           (optimize (speed 3) (safety 0)))
  (or (<= #.(char-code #\0) byte #.(char-code #\9))
      (standard-alpha-byte-p byte)))

(define-condition parsing-end-unexpectedly (simple-error)
  ((state :initarg :state
          :initform nil))
  (:report (lambda (condition stream)
             (format stream "Parsing ended unexpectedly~:[~;~:* at ~A~]"
                     (slot-value condition 'state)))))

(define-condition no-next-state (simple-error) ())



