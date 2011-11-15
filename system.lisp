(defpackage gtp
  (:use :common-lisp)
  (:export #:gtp-session
           #:make-gtp-session
           #:gtp-command
           #:make-gtp-command
	   #:issue-gtp-command
	   #:make-gtp-command-list
	   #:id))

(defpackage sepigo
  (:use :common-lisp)
  (:export #:start
	   #:stop))
