(defsystem "sbcl-xrepl"
  :description "A readline-powered REPL for SBCL."
  :version "0.0.1"
  :author "Jan Felix Langenbach"
  :licence "GNU GPL v3"
  :depends-on ("cl-readline")
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "user" :depends-on ("utils"))
               (:file "readline" :depends-on ("utils" "user"))
               (:file "repl" :depends-on ("utils" "readline" "user"))))
