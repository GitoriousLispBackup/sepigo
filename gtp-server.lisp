(in-package gtp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package 'hunchentoot 'hunchentoot '(ht)))

(defparameter *acceptor* nil)

(defun handle-ajax ()
  (setf (ht:content-type*) "text/plain")

  (unless (ht:session-value :gtp-session)
    (setf (ht:session-value :gtp-session)
          (make-gtp-session)))

  (if (ht:parameter "command-list")
      (let* ((command-list
              (json:decode-json-from-string (ht:parameter "command-list")))
             (gtp-session (ht:session-value :gtp-session))
             (gtp-command-list
              (make-gtp-command-list gtp-session command-list))
             (response-list
              (issue-gtp-command gtp-session gtp-command-list)))
        (json:encode-json-to-string response-list))
      (let* ((command-name (ht:parameter "command-name"))
             (gtp-session (ht:session-value :gtp-session))
             (args (ht:parameter "args"))
             (gtp-command
              (make-gtp-command gtp-session command-name args))
             (response
              (issue-gtp-command gtp-session gtp-command))
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
                    (:td (cl-who:esc (write-to-string (id (ht:session-value :gtp-session (cdr l)))))))))
              (ht:session-db *acceptor*))))))))

(defun reset ()
  (ht:remove-session ht:*session*)
  (ht:redirect "/sepigo/sepigo.html"))

(defun start (port)
  (setf *acceptor* (make-instance 'ht:acceptor :port port))
  (ht:start *acceptor*))

(defun stop ()
  (reset)
  (ht:stop *acceptor*))

(defun init ()
  (setf ht:*show-lisp-errors-p* t
        ht:*catch-errors-p* nil)

  (setf ht:*session-removal-hook*
        (lambda (session)
          (if session
              (issue-gtp-command
               (ht:session-value :gtp-session session)
               (make-gtp-command (ht:session-value :gtp-session session) "quit")))))

  ;; Setup *dispatch-table*
  (setf ht:*dispatch-table*
        (list
         (ht:create-prefix-dispatcher "/go" #'handle-ajax)
         (ht:create-folder-dispatcher-and-handler "/sepigo/" #p"/home/enigma/sync/src/sepigo/web/")
         'ht:dispatch-easy-handlers
         'ht:default-dispatcher)))

(defun toplevel ()
  (start 8080)
  (sb-thread:join-thread (first (sb-thread:list-all-threads))))
