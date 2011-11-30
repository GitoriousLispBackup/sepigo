(in-package sepigo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package 'hunchentoot 'hunchentoot '(ht)))

(defparameter *sepigo-acceptor* nil)

(ht:define-easy-handler (handle-ajax :uri "/go") ()
  (setf (ht:content-type*) "text/plain")

  (unless (ht:session-value :gtp-session)
    (setf (ht:session-value :gtp-session)
          (gtp:make-gtp-session)))
  (let* ((command-name (ht:parameter "command-name"))
	 (gtp-session (ht:session-value :gtp-session))
	 (args (ht:parameter "args"))
	 (gtp-command
	  (gtp:make-gtp-command gtp-session command-name args))
	 (response
	  (gtp:issue-gtp-command gtp-session gtp-command)))
    (json:encode-json-to-string response)))

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
  (ht:remove-session ht:*session*)
  (ht:redirect "/"))

(defun start (port)
  (setf *sepigo-acceptor*
	(ht:start
	 (make-instance 'ht:easy-acceptor :port port))))

(defun stop ()
  (ht:stop *sepigo-acceptor*))

(defun init ()
  ;; (setf ht:*show-lisp-errors-p* t
  ;;       ht:*catch-errors-p* nil)

  (setf ht:*session-removal-hook*
        (lambda (session)
          (if session
              (gtp:issue-gtp-command
               (ht:session-value :gtp-session session)
               (gtp:make-gtp-command (ht:session-value :gtp-session session) "quit")))))

  (push (ht:create-static-file-dispatcher-and-handler "/" #p"/home/enigma/sync/src/sepigo/web/sepigo.html")
	ht:*dispatch-table*)
  (push (ht:create-folder-dispatcher-and-handler "/web/" #p"/home/enigma/sync/src/sepigo/web/")
	ht:*dispatch-table*))
