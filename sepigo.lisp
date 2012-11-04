(in-package sepigo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package 'hunchentoot 'hunchentoot '(ht)))

(defparameter *sepigo-acceptor* nil)

(defclass sepigo-acceptor (ht:easy-acceptor)
  ((session-removal-hook
    :initform nil
    :initarg :session-removal-hook
    :accessor session-removal-hook)))

;; Session removal by timeout or explicite call of ht:remove-session
;; triggers the call of (session-removal-hook acceptor).
(defmethod acceptor-remove-session ((acceptor sepigo-acceptor) session)
  (log-message :ALARM "ALARM")
  (let ((hook (session-removal-hook acceptor)))
    (when hook
      (funcall hook session))))

;; Main resource for the js sepigo client
(ht:define-easy-handler (handle-ajax :uri "/go") ()
  (setf (ht:content-type*) "text/plain")

  ;; Create a new HTTP session if necessary
  (ht:start-session)
  (setf (ht:session-max-time ht:*session*) ; Session expires after 15min
        ;; (* 60 15)
        50
        )

  ;; Create a new GTP session if necessary
  (unless (current-gtp-session)
    (setf (ht:session-value :gtp-session)
          (gtp:make-session)))

  (let* ((command-name (ht:parameter "command-name"))
	 (command-args (ht:parameter "args"))
	 (session (current-gtp-session))
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

;; Manually reset the HTTP session
(defun reset ()
  (log-message :sepigo "Session reset!")
  (gtp:destroy-session (current-gtp-session))
  (ht:remove-session ht:*session*)
  (ht:redirect "/"))

(defun current-gtp-session ()
  "Return the current GTP session in the context of a query."
  (ht:session-value :gtp-session))

(defun start (&optional port address)
  ;; Configure sepigo logging
  (setf (log-manager)
        (make-instance 'log-manager
                       :message-class 'formatted-message))
  (start-messenger 'text-file-messenger
                   :filename "sepigo.log")

  ;; Open server
  (let ((a (or address "127.0.0.1"))
        (p (or port 8080)))
    (setf *sepigo-acceptor*
          (ht:start
           (make-instance 'sepigo-acceptor
                          :address a
                          :port p
                          :access-log-destination nil
                          :message-log-destination nil)))
    (log-message :sepigo "Server started. Listening on ~a:~a" a p))

  ;; Configure session removal hook
  (setf (session-removal-hook *sepigo-acceptor*)
        (lambda (session)
          (log-message :sepigo "Session removed")
          (when session
            (gtp:issue-command
             (current-gtp-session)
             (gtp:make-command (current-gtp-session) "quit"))
            (gtp:destroy-session (current-gtp-session)))))

  ;; (mapcar #'(lambda (th) (sb-thread:join-thread th))
  ;;         (sb-thread:list-all-threads))
  )

(defun stop ()
  (ht:stop *sepigo-acceptor*)
  (log-message :sepigo "Server stopped (~a:~a)."
               (ht:acceptor-address *sepigo-acceptor*)
               (ht:acceptor-port *sepigo-acceptor*)))

(defun rstart ()
  (stop)
  (start))

(defun configure ()
  ;; (setf ht:*show-lisp-errors-p* t
  ;;       ht:*catch-errors-p* nil)
  (push (ht:create-static-file-dispatcher-and-handler
         "/"
         #p"/home/enigma/sync/src/sepigo/web/sepigo.html")
	ht:*dispatch-table*)
  (push (ht:create-folder-dispatcher-and-handler
         "/web/"
         #p"/home/enigma/sync/src/sepigo/web/")
	ht:*dispatch-table*))

(configure)
