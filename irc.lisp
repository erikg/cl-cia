
(in-package :cl-cia)

(defvar *bot-thread* '())
(defvar *connection* '())

(defun filestr (files)
  (if (> (length files) 1)
      (format nil "(~{~a~^ ~})" files)
      (car files)))

(defun ascii-ize (str col)
  (format nil "~C~2,'0d~a~C" #\etx col str #\etx))

(defun format-commit (message)
  (format nil "~a ~a: ~a"
	  (ascii-ize (format nil "~a * ~a" (user message) (revision message)) 3)
	  (filestr (files message))
	  (message message)))

(defun report-commit (project message)
  (cl-irc:privmsg *connection* (channel project) (format-commit message)))

(defun msg-hook (msg)
  (declare (ignore msg))
  '())

(defun bot (&key (nick "CIA") (server "irc.freenode.net") (channel "#brlcad"))
  (setf *connection* (cl-irc:connect :username nick :realname "BRL Bot" :server server :nickname nick))
  (cl-irc:add-hook *connection* :privmsg 'msg-hook)
  (cl-irc:join *connection* channel)
  (setf *bot-thread* (bordeaux-threads:make-thread (lambda ()
						     (cl-irc:read-message-loop *connection*))
						   :name "brlbot")))

(defun stop-bot ()
  (cl-irc:quit *connection* "EVACUATE! EVACUATE!")
;  (bordeaux-threads:join-thread *bot-thread*)
  (setf *bot-thread* '())
)
