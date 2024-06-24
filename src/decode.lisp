(in-package :quri)

(declaim (ftype (function (character) (unsigned-byte 4)) hexdigit-to-integer))

; from Tim Bradshaw, http://www.tfeb.org/lisp/hax.html#COLLECTING
(defmacro collecting (&body forms)
  ;; Collect some random stuff into a list by keeping a tail-pointer
  ;; to it, return the collected list.  No real point in using
  ;; gensyms, although one probably should on principle.
  "Collect things into a list forwards.  Within the body of this macro
  The form `(COLLECT THING)' will collect THING into the list returned by
  COLLECTING.  Uses a tail pointer -> efficient."
  (let (($resnam$ (gensym)) ($tail$ (gensym)) ($thing$ (gensym)))
    `(let
         (,$resnam$ ,$tail$)
       (macrolet
           ((collect
                (thing)
                ;; Collect returns the thing it's collecting
                `(let ((,',$thing$ ,thing))
                   (if ,',$resnam$
                       (setf (cdr ,',$tail$)
                             (setf ,',$tail$ (list ,',$thing$)))
                       (setf ,',$resnam$
                             (setf ,',$tail$ (list ,',$thing$))))
                   ,',$thing$)))
         ,@forms)
       ,$resnam$)))

(defun hexdigit-to-integer (char)
  (declare (type character char)
           (optimize (speed 3) (safety 0)))
  (let ((code (char-code char)))
    (declare (type fixnum code))
    (cond
      ((<= #.(char-code #\0) code #.(char-code #\9))
       (- code #.(char-code #\0)))
      ((<= #.(char-code #\A) code #.(char-code #\F))
       (- code #.(- (char-code #\A) 10)))
      ((<= #.(char-code #\a) code #.(char-code #\f))
       (- code #.(- (char-code #\a) 10)))
      (t (error 'url-decoding-error)))))

(defun url-decode (data &key
                          (encoding babel-encodings:*default-character-encoding*)
                          (start 0)
                          end
                          (lenient nil))
  (declare (type (or string simple-byte-vector) data)
           (type integer start)
           (optimize (speed 3) (safety 2)))
  (let* ((end (or end (length data)))
         (buffer (make-array (- end start)
                             :element-type '(unsigned-byte 8)))
         (i 0)
         parsing-encoded-part)
    (declare (type integer end i)
             (type simple-byte-vector buffer))
    (flet ((write-to-buffer (byte)
             (declare (optimize (speed 3) (safety 0)))
             (setf (aref buffer i) byte)
             (incf i)))
      (with-array-parsing (char p data start end (and (not (stringp data))
                                                      #'code-char))
        (parsing
         (cond
           ((char= char #\%)
            (gonext))
           ((char= char #\+)
            (write-to-buffer #.(char-code #\Space))
            (redo))
           (t
            (write-to-buffer (char-code char))
            (redo))))

        (parsing-encoded-part
         (setq parsing-encoded-part char)
         (gonext))

        (parsing-encoded-part-second
         (handler-bind ((url-decoding-error
                          (lambda (error)
                            (declare (ignore error))
                            (when lenient
                              (write-to-buffer #.(char-code #\%))
                              (write-to-buffer (char-code parsing-encoded-part))
                              (write-to-buffer (char-code char))
                              (setq parsing-encoded-part nil)
                              (goto parsing)))))
           (write-to-buffer
            (+ (* 16 (hexdigit-to-integer parsing-encoded-part))
               (hexdigit-to-integer char))))
         (setq parsing-encoded-part nil)
         (goto parsing))

        (:eof
         (when parsing-encoded-part
           (error 'url-decoding-error)))))
    (babel:octets-to-string buffer :end i :encoding encoding :errorp (not lenient))))

(defun url-decode-params (data &key
                                 (delimiter #\&)
                                 (encoding babel-encodings:*default-character-encoding*)
                                 (start 0)
                                 end
                                 (lenient nil)
                                 (percent-decode t))
  (declare (type (or string simple-byte-vector) data)
           (type integer start)
           (type character delimiter)
           (optimize (speed 3) (safety 2)))
  (let ((end (or end (length data)))
        (start-mark nil)
        (=-mark nil))
    (declare (type integer end))
    (collecting
      (labels ((maybe-decode (string encoding start end)
                 (if percent-decode
                     (url-decode string
                                 :encoding encoding
                                 :start start
                                 :end end
                                 :lenient lenient)
                     (subseq string start end)))
               (collect-pair (p)
                 (tagbody
                    (handler-bind ((url-decoding-error
                                    (lambda (error)
                                      (declare (ignore error))
                                      (when lenient
                                        (go continue)))))
                      (collect
                          (cons (maybe-decode data encoding start-mark =-mark)
                                (maybe-decode data encoding (1+ =-mark) p))))
                  continue)
                 (setq start-mark nil
                       =-mark nil))
               (collect-field (p)
                 (tagbody
                    (handler-bind ((url-decoding-error
                                    (lambda (error)
                                      (declare (ignore error))
                                      (when lenient
                                        (go continue)))))
                      (collect
                          (cons (maybe-decode data encoding start-mark p)
                                nil)))
                  continue)
                 (setq start-mark nil)))
        (with-array-parsing (char p data start end (and (not (stringp data))
                                                        #'code-char))
          (start
           (setq start-mark p)
           (if lenient
               (cond
                 ((char= char #\=)
                  (setq =-mark p)
                  (goto parsing-value))
                 ((char= char delimiter)
                  (redo)))
               (when (or (char= char #\=)
                         (char= char delimiter))
                 (error 'uri-malformed-urlencoded-string)))
           (gonext))

          (parsing-field
           (cond
             ((char= char #\=)
              (setq =-mark p)
              (gonext))
             ((char= char delimiter)
              ;; field only
              (collect-field p)
              (goto start)))
           (redo))

          (parsing-value
           (cond
             ((char= char #\=)
              (unless lenient
                (error 'uri-malformed-urlencoded-string)))
             ((char= char delimiter)
              (collect-pair p)
              (goto start)))
           (redo))

          (:eof
           (cond
             (=-mark (collect-pair p))
             (start-mark (collect-field p)))))))))
