(defpackage gtp
  (:use :common-lisp)
  (:export #:gtp-session
           #:make-gtp-session
           #:gtp-command
           #:make-gtp-command
           #:make-gtp-command-list
           #:issue-command
           #:close-session
           #:id))

(defpackage gtp-server
  (:use :common-lisp)
  (:export #:toplevel
           #:init
           #:start
           #:stop
           #:restart*))
