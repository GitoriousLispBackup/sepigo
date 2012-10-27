(defpackage gtp
  (:use :common-lisp :cl-log)
  (:export #:session
           #:make-session
           #:destroy-session
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
           #:configure
           #:acceptor-remove-session))
