
(in-package :cl-cia)

#|

<message> 
  <generator>
    <name>CIA Trac plugin</name>
    <version>0.1</version>
  </generator>
  <source>
    <project>%s</project>
    <module>%s</module>
  </source>
  <body>
    <commit>
      <revision>%s</revision>
      <author>%s</author>
      <log>%s</log>
    </commit>
  </body>
</message>

|#

(defvar poop '())
(defun xmlrpc (str)
  (setf poop (s-xml:parse-xml (make-string-input-stream str))))
