;;;; cl-cia.asd

(asdf:defsystem #:cl-cia
    :serial t
    :description "cl-cia is an IRC bot to report commits to a subversion repository"
    :author "Erik Greenwald <erik@elfga.com>"
    :license "LLGPL"
    :depends-on (#:cl-irc
		 :local-time
		 :cl-fad
		 :alexandria
		 :bordeaux-threads
		 :cl-ppcre
		 :s-xml
		 :cl-store
		 )
    :components ((:file "package")
		 (:file "cl-cia")
		 (:file "irc")
		 (:file "xmlrpc")
		 (:file "maildir")))

