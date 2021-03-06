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
(defmethod ht:acceptor-remove-session ((acceptor sepigo-acceptor)
                                       (session t))
  (let ((hook (session-removal-hook acceptor)))
    (when (and hook session)
      (funcall hook session))))

(defun session-remove (ht-session)
  "Remove a gtp session based on a ht session."
  (log:info "Ht and GTP sessions removed")
  (let ((gtp-session (ht:session-value :gtp-session
                                       ht-session)))
    (when (and (current-gtp-session)
               (gtp:process-valid-p (current-gtp-session)))
      (gtp:issue-command gtp-session
                         (gtp:make-command gtp-session "quit"))
      (gtp:destroy-session gtp-session)
      (ht:remove-session ht-session))))

;; Main resource for the js sepigo client
(ht:define-easy-handler (handle-ajax :uri "/go") ()
  (setf (ht:content-type*) "text/plain")

  ;; Create a new HTTP session if necessary
  (ht:start-session)

  ;; Create a new GTP session (with its associated gnugo process) if
  ;; necessary
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
    (if (and (gtp:command-valid-p command)
             (gtp:process-valid-p (current-gtp-session)))
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
                    (:td (cl-who:esc (ht:session-user-agent
                                      (cdr l))))
                    (:td (cl-who:esc (ht:session-remote-addr
                                      (cdr l))))
                    (:td (cl-who:esc (write-to-string
                                      (gtp:id
                                       (ht:session-value
                                        :gtp-session (cdr l)))))))))
              (ht:session-db *sepigo-acceptor*))))))))

;; Manually reset the HTTP and GTP sessions
(defun reset ()
  (log:info "Session reset!")
  (when (current-gtp-session)
    (gtp:destroy-session (current-gtp-session)))
  (ht:remove-session ht:*session*)
  (ht:redirect "/"))

(defun current-gtp-session ()
  "Return the current GTP session in the context of a query."
  (ht:session-value :gtp-session))

(defun start (&optional port address)
  ;; Configure sepigo logging
  (log:config :sane)
  
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
    (log:info "Server started. Listening on ~a:~a" a p))

  ;; Session expires after 15min
  (setf ht:*session-max-time*
        (* 60 15))

  ;; Configure session removal hook
  (setf (session-removal-hook *sepigo-acceptor*)
        #'session-remove))

(defun stop ()
  (log:info "Server stopped (~a:~a)."
            (ht:acceptor-address *sepigo-acceptor*)
            (ht:acceptor-port *sepigo-acceptor*))
  (ht:stop *sepigo-acceptor*))

(defun rstart ()
  (stop)
  (start))

(defun configure ()
  (setf ht:*show-lisp-errors-p* t
        ht:*catch-errors-p* nil)
  (setf ht:*dispatch-table*
        (list (ht:create-folder-dispatcher-and-handler "/web/"
               #p"/home/enigma/sync/src/sepigo/web/")
              (ht:create-static-file-dispatcher-and-handler "/"
               #p"/home/enigma/sync/src/sepigo/web/index.html")
              #'ht:dispatch-easy-handlers)))

(configure)
