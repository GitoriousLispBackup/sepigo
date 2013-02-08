(defpackage gtp
  (:use :common-lisp)
  (:export #:session
           #:make-session
           #:destroy-session
           #:command
           #:make-command
	   #:issue-command
	   #:make-command-list
	   #:id
	   #:command-valid-p
           #:process-valid-p))

(defpackage sepigo
  (:use :common-lisp)
  (:export #:start
	   #:stop
           #:rstart
           #:configure
           #:acceptor-remove-session))
