(in-package gtp)

(defparameter *acceptor* nil)

(hunchentoot:define-easy-handler (handle-command :uri "/go") (command-name args)
  (setf (hunchentoot:content-type*) "text/plain")

  (unless (hunchentoot:session-value :gtp-session)
    (setf (hunchentoot:session-value :gtp-session)
          (make-gtp-session)))

  (let* ((session (hunchentoot::session-value :gtp-session))
         (command (make-gtp-command session command-name args))
         (response (issue-gtp-command session command)))
    (json:encode-json-to-string response)))

(hunchentoot:define-easy-handler (reset-session :uri "/reset") ()
  (reset))

(hunchentoot:define-easy-handler (stats :uri "/stats") ()
  (setf (hunchentoot:content-type*) "text/html")
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
                    (:td (cl-who:esc (hunchentoot:session-user-agent (cdr l))))
                    (:td (cl-who:esc (hunchentoot:session-remote-addr (cdr l))))
                    (:td (cl-who:esc (write-to-string (id (hunchentoot:session-value :gtp-session (cdr l)))))))))
              (hunchentoot:session-db *acceptor*))))))))

(defun reset ()
  (if hunchentoot:*session*
      (hunchentoot:remove-session hunchentoot:*session*))
  (hunchentoot:redirect "/sepigo/sepigo.html"))

(defun start (port)
  (setf *acceptor* (make-instance 'hunchentoot:acceptor :port port))
  (hunchentoot:start *acceptor*))

(defun stop ()
  (reset)
  (hunchentoot:stop *acceptor*))

(defun init ()
  (setf hunchentoot:*show-lisp-errors-p* t
        hunchentoot:*catch-errors-p* nil)
  (setf hunchentoot:*session-removal-hook*
        (lambda (session)
          (if session
              (issue-gtp-command
               (hunchentoot:session-value :gtp-session session)
               (make-gtp-command (hunchentoot:session-value :gtp-session session) "quit")))))
  (push (hunchentoot:create-folder-dispatcher-and-handler "/sepigo/" #p"web/")
        hunchentoot:*dispatch-table*))


(defun toplevel ()
  (start 8080)
  (sb-thread:join-thread (first (sb-thread:list-all-threads))))
