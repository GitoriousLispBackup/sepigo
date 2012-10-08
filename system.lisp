(defpackage gtp
  (:use :common-lisp)
  (:export #:session
           #:make-session
           #:command
           #:make-command
	   #:issue-command
	   #:make-command-list
	   #:id
	   #:command-valid-p))

(defpackage sepigo
  (:use :common-lisp)
  (:export #:start
	   #:stop
           #:configure))
