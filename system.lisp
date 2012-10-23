(defpackage gtp
  (:use :common-lisp :cl-log)
  (:export #:session
           #:make-session
           #:command
           #:make-command
	   #:issue-command
	   #:make-command-list
	   #:id
	   #:command-valid-p))

(defpackage sepigo
  (:use :common-lisp :cl-log)
  (:export #:start
	   #:stop
           #:rstart
           #:configure))
