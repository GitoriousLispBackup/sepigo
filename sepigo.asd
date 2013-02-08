(defsystem :sepigo
  :version "0.1"
  :depends-on (:cl-utilities
               :hunchentoot
               :cl-json
               :cl-who
               :log4cl)
  :components
  ((:file "system")
   (:file "gtp" :depends-on ("system"))
   (:file "sepigo" :depends-on ("system" "gtp"))))
