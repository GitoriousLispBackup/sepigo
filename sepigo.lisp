(in-package sepigo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package 'hunchentoot 'hunchentoot '(ht)))

(defparameter *sepigo-acceptor* nil)

(defclass sepigo-acceptor (ht:easy-acceptor)
  ((session-removal-hook
    :initform nil
    :initarg :session-removal-hook
    :accessor session-removal-hook)))

;; Remove
(defmethod acceptor-remove-session ((acceptor sepigo-acceptor) (session ht:session))
  (when (session-removal-hook acceptor)
    (funcall (session-removal-hook acceptor) session)))

(ht:define-easy-handler (handle-ajax :uri "/go") ()
  (setf (ht:content-type*) "text/plain")

  ;; Create a new session if necessary
  (unless (ht:session-value :gtp-session)
    (setf (ht:session-value :gtp-session)
          (gtp:make-session)))

  (let* ((command-name (ht:parameter "command-name"))
	 (command-args (ht:parameter "args"))
	 (session (ht:session-value :gtp-session))
	 (command
	  (gtp:make-command session
			    command-name
			    command-args)))
    (if (gtp:command-valid-p command)
	(json:encode-json-to-string
	 (gtp:issue-command session command))
	"Error: Invalid command")))

(ht:define-easy-handler (reset-session :uri "/reset") ()
  (reset))

(ht:define-easy-handler (stats :uri "/stats") ()
  (setf (ht:content-type*) "text/html")
  (cl-who:with-html-output-to-string (stream nil :prologue t :indent t)
    (:html
     (:head (:title "stats"))
     (:body
      (:table
       (:thead
        (:tr (:td "Useragent") (:td "IP-Address") (:td "ID")))
       (:tbody
        (mapc #'(lambda (l)
                  (cl-who:htm
                   (:tr
                    (:td (cl-who:esc (ht:session-user-agent (cdr l))))
                    (:td (cl-who:esc (ht:session-remote-addr (cdr l))))
                    (:td (cl-who:esc (write-to-string (gtp:id (ht:session-value :gtp-session (cdr l)))))))))
              (ht:session-db *sepigo-acceptor*))))))))

;; Can only be called in the context of a request
(defun reset ()
  (log-message :sepigo "Seession reset!")
  (ht:remove-session ht:*session*)
  (ht:redirect "/"))

(defun start (&optional port address)
  (setf (log-manager)
        (make-instance 'log-manager
                       :message-class 'formatted-message))
  (start-messenger 'text-file-messenger
                   :filename "/tmp/sepigo.log")
  (let ((a (or address "127.0.0.1"))
        (p (or port 8080)))
    (setf *sepigo-acceptor*
          (ht:start
           (make-instance 'sepigo-acceptor :address a :port p
                          :access-log-destination nil
                          :message-log-destination nil)))
    (log-message :sepigo "Server started. Listening on ~a:~a" a p))
  ;; (mapcar #'(lambda (th) (sb-thread:join-thread th))
  ;;         (sb-thread:list-all-threads))
  )

(defun stop ()
  (ht:stop *sepigo-acceptor*)
  (log-message :sepigo "Server stopped (~a:~a)."
               (ht:acceptor-address *sepigo-acceptor*)
               (ht:acceptor-port *sepigo-acceptor*)))

(defun configure ()
  ;; (setf ht:*show-lisp-errors-p* t
  ;;       ht:*catch-errors-p* nil)
  (push (ht:create-static-file-dispatcher-and-handler "/" #p"/home/enigma/sync/src/sepigo/web/sepigo.html")
	ht:*dispatch-table*)
  (push (ht:create-folder-dispatcher-and-handler "/web/" #p"/home/enigma/sync/src/sepigo/web/")
	ht:*dispatch-table*))

(configure)
