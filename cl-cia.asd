;;;; cl-cia.asd

(asdf:defsystem #:cl-cia
  :serial t
  :description "Describe cl-cia here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-irc)
  :components ((:file "package")
               (:file "cl-cia")))

