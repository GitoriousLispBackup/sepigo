(defsystem :sepigo
  :version "0.1"
  :depends-on (:cl-utilities :hunchentoot :cl-json)
  :components
  ((:file "system")
   (:file "gtp" :depends-on ("system"))
   (:file "gtp-server" :depends-on ("system" "gtp"))))