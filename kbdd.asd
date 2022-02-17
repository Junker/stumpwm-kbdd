;;;; kbdd.asd

(asdf:defsystem #:kbdd
  :description "Keyboard per window layout switcher and modeline module for StumpWM"
  :author "Dmitrii Kosenkov"
  :license  "GPLv3"
  :version "0.1.0"
  :serial t
  :depends-on (#:stumpwm #:dbus)
  :components ((:file "package")
               (:file "kbdd")))
