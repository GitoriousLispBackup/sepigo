(in-package gtp)

(defclass session ()
  ((in-stream
    :initarg :in-stream
    :accessor in-stream)
   (out-stream
    :initarg :out-stream
    :accessor out-stream)
   (key
    :initarg :key
    :accessor key)
   (id 
    :initarg :id
    :accessor id
    :initform 0))
  (:documentation
   "Handles the state of a gtp session: the input and output streams
   to the child process, a key that associates a gtp session with a
   web session and the current request/response id"))

(defun make-session ()
  (multiple-value-bind (in out)
      (open-gtp-stream)
    (let ((session (make-instance 'session
                                  :in-stream in
                                  :out-stream out
                                  :key (random 1024))))
      session)))

(defclass command ()
  ((id
    :initarg :id
    :accessor id)
   (command-name
    :initarg :command-name
    :accessor command-name)
   (arguments
    :initarg :arguments
    :accessor arguments)))

(defmethod make-command ((session session) command-name &optional arguments)
  (let ((command
         (make-instance 'command
                        :id (incf (id session))
                        :command-name command-name
                        :arguments arguments)))
    command))

(defmethod ->string ((command command))
  (concatenate 'string
               (write-to-string (id command))
               " "
               (command-name command)
               " "
               (arguments command)))

(defmethod command-valid-p ((command command))
  (if (command-name command) t))
  
(defclass response ()
  ((id
    :initarg :id
    :accessor id)
   (success
    :initarg :success
    :accessor success)
   (data
    :initarg :data
    :accessor data)))

(defmethod make-response ((session session) id success data)
  (let ((response
         (make-instance 'response
                        :id id
                        :success success
                        :data data)))
    response))

(defmethod make-response-from-string ((session session) string)
  (let* ((tokens (cl-utilities:split-sequence #\Space string :remove-empty-subseqs t))
         (token0 (first tokens))
         (success (cond ((eql (char token0 0) #\=)
                         t)
                        ((eql (char token0 0) #\?)
                         nil)
                        (t
                         (error "Malformed response: expected = or ?"))))
         (id (parse-integer (subseq token0 1)))
         (data (rest tokens)))
    (make-response session id success data)))
        
(defclass vertex ()
  ((row
    :initarg :row
    :accessor row)
   (col
    :initarg :col
    :accessor col)))

(defmethod ->string ((vertex vertex))
  (concatenate 'string
               (list (code-char (+ (1- (row vertex))
                                   (char-code #\a))))
               (write-to-string (col vertex))))

(defun make-vertex-from-string (string)
  (make-instance 'vertex
                 :row (1+ (- (char-code (aref string 0))
                             (char-code #\a)))
                 :col (parse-integer (subseq string 1))))

(defclass move ()
  ((color
    :initarg :color
    :accessor color)
   (vertex
    :initarg :vertex
    :accessor vertex
    :type vertex)))

(defmethod ->string ((move move))
  (concatenate 'string
               (case (color move)
                 (:w "w")
                 (:b "b"))
               " "
               (->string (vertex move))))

(defun make-move-from-string (string)
  (let ((tokens (cl-utilities:split-sequence #\Space string :remove-empty-subseqs t)))
    (make-instance 'move
                   :color (case (string-downcase (first tokens))
                            (("white" "w") :w)
                            (("black" "b") :b)
                            (t :b))
                   :vertex (make-vertex-from-string (format nil "~{~a~^ ~}" (rest tokens))))))

(defun open-gtp-stream ()
  #+sbcl
  (let* ((process (sb-ext:run-program "gnugo" '("--mode=gtp" "--level=0") :search t :input :stream :output :stream :wait nil))
         (in (sb-ext:process-input process))
         (out (sb-ext:process-output process)))
    (format t "~a ~a" in out)
    (values in out process)))

(defmethod issue-command ((session session) (command command))
  (write-line (->string command)
              (in-stream session))
  (finish-output (in-stream session))
  (let* ((returned-lines
          (loop for line = (read-line (out-stream session) nil 'eof)
             until (or (equal line "") (eql line 'eof))
             collecting line))
         (lines-in-one-str (format nil "~{~a~^~%~}" returned-lines))
         (response
          (make-response-from-string session
                                         lines-in-one-str)))
    ;; (unless (eql (id session) (id response))
    ;;   (error "Request and response ids not the same"))
    (format t "foo")
    response))
