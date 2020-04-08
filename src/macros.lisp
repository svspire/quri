(in-package :quri)

(defmacro with-string-parsing ((elem p seq &optional (start 0) end key) &body body)
  `(let ((,elem #\Nul))
     (declare (type character ,elem))
     (%with-array-parsing (,elem ,p ,seq ,start ,end ,key) ,@body)))

(defmacro with-byte-array-parsing ((elem p seq &optional (start 0) end key) &body body)
  `(let ((,elem 0))
     (declare (type (unsigned-byte 8) ,elem))
     (%with-array-parsing (,elem ,p ,seq ,start ,end ,key) ,@body)))

(defmacro with-array-parsing ((elem p seq &optional (start 0) end key) &body body)
  `(let (,elem)
     (%with-array-parsing (,elem ,p ,seq ,start ,end ,key) ,@body)))

(defmacro %with-array-parsing ((elem p seq &optional (start 0) end key) &body body)
  (with-gensyms (g-end no-next-state last key-fn)
    (let ((eof-exists nil))
      `(let (,@(and key `((,key-fn ,key)))
             (,p ,start)
             (,g-end (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
                       (or ,end (length ,seq)))))
         (declare (ignorable ,p ,g-end))
         ,@(loop for (exp . rest) on body
                 while (and (listp exp) (eq (car exp) 'declare))
                 collect exp
                 do (setq body rest))
         (macrolet ((goto (tag &optional (amount 1))
                      `(locally (declare (optimize (speed 3) (safety 0)))
                         (incf ,',p ,amount)
                         ,@(if (eql amount 0)
                               ()
                               `((when (= ,',p ,',g-end)
                                   (go :eof))
                                 (setq ,',elem
                                       ,',(if key
                                              `(if ,key-fn
                                                   (funcall ,key-fn (aref ,seq ,p))
                                                   (aref ,seq ,p))
                                              `(aref ,seq ,p)))))
                         (go ,tag))))
           (tagbody
              (when (= ,p ,g-end)
                (go :eof))
              (locally (declare (optimize (speed 3) (safety 0)))
                (setq ,elem ,@(if key
                                  `((if ,key-fn
                                        (funcall ,key-fn (aref ,seq ,p))
                                        (aref ,seq ,p)))
                                  `((aref ,seq ,p)))))
              ,@(loop for (tagpart . rest) on body
                      for (tag . part) = tagpart
                      if (eq tag :eof)
                        append (progn
                                 (setf eof-exists t)
                                 `(,@tagpart
                                   (go ,last)))
                      else
                        append
                        (list tag
                              `(macrolet ((redo (&optional (amount 1))
                                            `(goto ,',tag ,amount))
                                          (gonext (&optional (amount 1))
                                            `(goto ,',(or (caar rest) no-next-state)
                                                   ,amount)))
                                 ,@part
                                 (error 'parsing-end-unexpectedly :state ',tag))))

              ,no-next-state
              (error 'no-next-state)

              ,@(if eof-exists
                    ()
                    '(:eof))

              ,last))))))

(defmacro parse-until-string (delimiters data &key start end test)
  (with-gensyms (p char)
    `(block nil
       (progn
         (do ((,p ,start (1+ ,p)))
             ((= ,p ,end)
              (values ,data ,start ,end))
           (declare (type fixnum ,p))
           (let ((,char (aref ,data ,p)))
             (declare (type character ,char))
             (when (or ,@(loop for delim in delimiters
                               collect `(char= ,delim ,char)))
               (return (values ,data ,start ,p)))
             ,@(when test
                 `((unless (funcall ,test ,char)
                      (error 'uri-malformed-string
                             :data ,data :position ,p))))))))))
(defmacro defun-with-array-parsing (name (char p data start end &rest other-args) &body body)
  (with-gensyms (args type form env)
    (flet ((intern-proper-case (a b)
             (intern (format nil "~:@(~a-~a~)" a b))))
      (let ((fn-for-string (intern-proper-case name :string)) 
            (fn-for-byte-vector (intern-proper-case name :byte-vector)))
        `(progn
           (defun ,name (,data &rest ,args &key ,start ,end)
             (declare (ignore ,start ,end))
             (etypecase ,data
                 (simple-string (apply ',(intern-proper-case name :string) data ,args))
                 (simple-byte-vector (apply ',(intern-proper-case name :byte-vector) data ,args))))
  
           #+(or sbcl openmcl cmu allegro)
           (define-compiler-macro ,name (&whole ,form &environment ,env ,data &rest ,args)
             (declare (ignore ,args))
             (let ((,type (cond
                            ((constantp ,data) (type-of ,data))
                            ((symbolp ,data) (cdr (assoc 'type (nth-value 2 (variable-information ,data ,env))))))))
               (cond
                 ((null ,type) ,form)
                 ((subtypep ,type 'simple-string) `(,',fn-for-string ,@(cdr ,form)))
                 ((subtypep ,type 'simple-byte-vector) `(,',fn-for-byte-vector ,@(cdr ,form)))
                 (t ,form))))
  
           (defun ,fn-for-string (,data &key (,start 0) (,end (length ,data)) ,@other-args)
             (declare (type simple-string ,data)
                      (type fixnum ,start ,end)
                      (optimize (speed 3) (safety 2)))
             (macrolet ((char=* (char1 char2)
                          `(char= ,char1 ,char2))
                        (char-code* (char)
                          `(char-code ,char))
                        (scheme-char-p* (char)
                          `(scheme-char-p ,char))
                        (standard-alpha-char-p* (char)
                          `(standard-alpha-char-p ,char)))
               (block ,name
                 (with-string-parsing (,char ,p ,data ,start ,end)
                   (declare (type fixnum ,p))
                   ,@body))))
  
           (defun ,fn-for-byte-vector (,data &key (,start 0) (,end (length ,data)) ,@other-args)
             (declare (type simple-byte-vector ,data)
                      (type fixnum ,start ,end)
                      (optimize (speed 3) (safety 2)))
             (macrolet ((char=* (byte char)
                          `(= ,byte ,(char-code char)))
                        (char-code* (byte)
                          byte)
                        (scheme-char-p* (byte)
                          `(scheme-byte-p ,byte))
                        (standard-alpha-char-p* (byte)
                          `(standard-alpha-byte-p ,byte)))
               (block ,name
                 (with-byte-array-parsing (,char ,p ,data ,start ,end)
                   (declare (type fixnum ,p))
                     ,@body)))))))))

(defmacro parse-until-byte-vector (delimiters data &key start end test)
  (with-gensyms (p byte)
    `(block nil
       (progn
         (do ((,p ,start (1+ ,p)))
             ((= ,p ,end)
              (values ,data ,start ,end))
           (declare (type fixnum ,p))
           (let ((,byte (aref ,data ,p)))
             (declare (type (unsigned-byte 8) ,byte))
             (when (or ,@(loop for delim in delimiters
                               collect `(= ,(char-code delim) ,byte)))
               (return (values ,data ,start ,p)))
             ,@(when test
                 `((unless (funcall ,test ,byte)
                     (error 'uri-malformed-string
                            :data ,data :position ,p))))))))))