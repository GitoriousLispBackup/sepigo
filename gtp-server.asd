(defsystem :gtp-server
  :version "0.1"
  :depends-on (:cl-utilities :hunchentoot :cl-json :cl-who)
  :components
  ((:file "system")
   (:file "gtp" :depends-on ("system"))
   (:file "gtp-server" :depends-on ("system" "gtp"))))
