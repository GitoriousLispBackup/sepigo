(in-package gtp-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package 'hunchentoot 'hunchentoot '(ht)))

(defparameter *acceptor* nil)
(defparameter *max-sessions* 100)       ; Not used yet

(defun handle-ajax ()
  (setf (ht:content-type*) "text/plain")

  ;; Create a new gtp session if necessary
  (unless (ht:session-value :gtp-session)
    (setf (ht:session-value :gtp-session)
          (gtp:make-gtp-session)))

  (if (ht:parameter "command-list")
      (let* ((command-list
              (json:decode-json-from-string (ht:parameter "command-list")))
             (gtp-session (ht:session-value :gtp-session))
             (gtp-command-list
              (gtp:make-gtp-command-list gtp-session command-list))
             (response-list
              (gtp:issue-command gtp-session gtp-command-list)))
        (json:encode-json-to-string response-list))
      (let* ((command-name (ht:parameter "command-name"))
             (gtp-session (ht:session-value :gtp-session))
             (args (ht:parameter "args"))
             (gtp-command
              (gtp:make-gtp-command gtp-session command-name args))
             (response
              (gtp:issue-command gtp-session gtp-command))
             )
        (json:encode-json-to-string response)
        )
      ))
  

(ht:define-easy-handler (reset-session :uri "/reset") ()
  (reset))

(ht:define-easy-handler (root :uri "/") ()
  (ht:redirect "/sepigo/sepigo.html"))

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
              (ht:session-db *acceptor*))))))))

(defun reset ()
"Reset a web session"
  (ht:remove-session ht:*session*)
  (ht:redirect "/sepigo/sepigo.html"))

(defun start (&optional (port 8080))
  (setf *acceptor* (make-instance 'ht:acceptor :port port))
  (ht:start *acceptor*))

(defun stop ()
  (ht:reset-sessions *acceptor*)
  (ht:stop *acceptor*))

(defun restart* ()
  (cond
    (*acceptor*
     (stop)
     (start (ht:acceptor-port *acceptor*)))
    (t (start))))

(defun init ()
  (setf ht:*show-lisp-errors-p* t
        ht:*catch-errors-p* nil)

  ;; Quit 
  (setf ht:*session-removal-hook*
        #'(lambda (session)
            (gtp:close-session (ht:session-value :gtp-session session))))

  ;; Setup *dispatch-table*
  (setf ht:*dispatch-table*
        (list
         (ht:create-prefix-dispatcher "/go" #'handle-ajax)
         (ht:create-folder-dispatcher-and-handler "/sepigo/" #p"/home/enigma/sync/src/sepigo/web/")
         'ht:dispatch-easy-handlers
         'ht:default-dispatcher)))

(defun toplevel ()
  (init)
  (start 8080)
#+sbcl
  (sb-thread:join-thread (first (sb-thread:list-all-threads)))
#+ccl
  (mapc #'(lambda (p) (ccl:join-process p)) (ccl:all-processes)))
