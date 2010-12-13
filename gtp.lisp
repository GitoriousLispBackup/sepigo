(in-package gtp)

(defparameter *allowed-command-names*
  '("boardsize"
    "clear_board"
    "genmove"
    "list_stones"
    "captures"
    "play"
    "quit"))

(defclass gtp-session ()
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

(defun make-gtp-session ()
  (multiple-value-bind (in out)
      (open-gtp-stream)
    (let ((session (make-instance 'gtp-session
                                  :in-stream in
                                  :out-stream out
                                  :key (random 1024))))
      session)))

(defclass gtp-command ()
  ((id
    :initarg :id
    :accessor id)
   (command-name
    :initarg :command-name
    :accessor command-name)
   (arguments
    :initarg :arguments
    :accessor arguments)))

(defmethod make-gtp-command ((session gtp-session) command-name &optional arguments)
  (let ((command
         (make-instance 'gtp-command
                        :id (incf (id session))
                        :command-name command-name
                        :arguments arguments)))
    command))

(defmethod make-gtp-command-list ((session gtp-session) command-list)
  (mapcar #'(lambda (command)
              (make-gtp-command session
                                (cdr (assoc :command-name command))
                                (cdr (assoc :args command))))
          command-list))

(defmethod ->string ((command gtp-command))
  (concatenate 'string
               (write-to-string (id command))
               " "
               (command-name command)
               " "
               (arguments command)))
  
(defclass gtp-response ()
  ((id
    :initarg :id
    :accessor id)
   (success
    :initarg :success
    :accessor success)
   (data
    :initarg :data
    :accessor data)))

(defmethod make-gtp-response ((session gtp-session) id success data)
  (let ((response
         (make-instance 'gtp-response
                        :id id
                        :success success
                        :data data)))
    response))

(defmethod make-gtp-response-from-string ((session gtp-session) string)
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
    (make-gtp-response session id success data)))
        
(defclass gtp-vertex ()
  ((row
    :initarg :row
    :accessor row)
   (col
    :initarg :col
    :accessor col)))

(defmethod ->string ((vertex gtp-vertex))
  (concatenate 'string
               (list (code-char (+ (1- (row vertex))
                                   (char-code #\a))))
               (write-to-string (col vertex))))

(defun make-gtp-vertex-from-string (string)
  (make-instance 'gtp-vertex
                 :row (1+ (- (char-code (aref string 0))
                             (char-code #\a)))
                 :col (parse-integer (subseq string 1))))

(defclass gtp-move ()
  ((color
    :initarg :color
    :accessor color)
   (vertex
    :initarg :vertex
    :accessor vertex
    :type gtp-vertex)))

(defmethod ->string ((move gtp-move))
  (concatenate 'string
               (case (color move)
                 (:w "w")
                 (:b "b"))
               " "
               (->string (vertex move))))

(defun make-gtp-move-from-string (string)
  (let ((tokens (cl-utilities:split-sequence #\Space string :remove-empty-subseqs t)))
    (make-instance 'gtp-move
                   :color (case (string-downcase (first tokens))
                            (("white" "w") :w)
                            (("black" "b") :b)
                            (t :b))
                   :vertex (make-gtp-vertex-from-string (format nil "~{~a~^ ~}" (rest tokens))))))

(defun open-gtp-stream ()
  #+sbcl
  (let* ((process (sb-ext:run-program "gnugo" '("--mode=gtp" "--level=0") :search t :input :stream :output :stream :wait nil))
         (in (sb-ext:process-input process))
         (out (sb-ext:process-output process)))
    (format t "~a ~a" in out)
    (values in out))
  #+ccl
  (let* ((process (ccl:run-program "gnugo" '("--mode=gtp" "--level=0") :input :stream :output :stream :wait nil))
         (in (ccl:external-process-input-stream process))
         (out (ccl:external-process-output-stream process)))
    (format t "~a ~a" in out)
    (values in out)))

(defmethod close-session ((session gtp-session))
  "Close the gtp-session and release all associated resources like the
inferior process"
  (issue-command session (make-gtp-command session "quit")))

(defmethod issue-command ((session gtp-session) (command gtp-command))
  (unless (find (command-name command) *allowed-command-names* :test #'equal)
    (error "Command ~A not allowed" (command-name command)))

  (write-line (->string command)
              (in-stream session))
  (finish-output (in-stream session))
  (let* ((returned-lines
          (loop for line = (read-line (out-stream session) nil 'eof)
             until (or (equal line "") (eql line 'eof))
             collecting line))
         (lines-in-one-str (format nil "~{~a~^~%~}" returned-lines))
         (response
          (make-gtp-response-from-string session
                                         lines-in-one-str)))
    ;; (unless (eql (id session) (id response))
    ;;   (error "Request and response ids not the same"))
    response))

(defmethod issue-command ((session gtp-session) (command-list list))
  (mapcar #'(lambda (command)
              (issue-command session
                                 command))
          command-list))