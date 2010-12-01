(in-package gtp)

(hunchentoot:define-easy-handler (handle-command :uri "/go") (command-name args)
  (setf (hunchentoot:content-type*) "text/plain")

  (unless (gethash (hunchentoot:session-value :key) *game-hash*)
    (let ((gtp-session (make-gtp-session))
          (gtp-session-key (random 100000000)))
      (setf (hunchentoot:session-value :key)
            gtp-session-key)
      (setf (key gtp-session)
            gtp-session-key)
      ;; Create a new gtp session (spawns a process) and save it to the
      ;; game hash
      (setf (gethash (hunchentoot:session-value :key) *game-hash*)
            gtp-session)))

  (let* ((session (gethash (hunchentoot::session-value :key) *game-hash*))
         (command (make-gtp-command session command-name args))
         (response (issue-gtp-command session command)))
    (json:encode-json-to-string response)))

(hunchentoot:define-easy-handler (reset-session :uri "/reset") ()
  (reset))

(defun reset ()
  (maphash #'(lambda (key session) (issue-gtp-command session (make-gtp-command session "quit"))) *game-hash*)
  (clrhash *game-hash*)
  (hunchentoot:reset-sessions))  

(defun start (port)
  (setf *acceptor* (make-instance 'hunchentoot:acceptor :port port))
  (hunchentoot:start *acceptor*))

(defun stop ()
  (reset)
  (hunchentoot:stop *acceptor*))

(defun toplevel ()
  (defparameter *game-hash*
    (make-hash-table))
  (defparameter *acceptor* nil)
  (setf hunchentoot:*show-lisp-errors-p* t)
  (push (hunchentoot:create-folder-dispatcher-and-handler "/sepigo/" #p"web/")
        hunchentoot:*dispatch-table*)
  (start 8080)
  (sb-thread:join-thread (first (sb-thread:list-all-threads))))
