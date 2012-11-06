
(in-package :cl-cia)

(defvar *bot-thread* '())
(defvar *connection* '())

(defun report-commit (message)
  (cl-irc:privmsg *connection* "#brlcad" (format nil "~a:~a ~a" (user message) (revision message) (message message))))

(defun bot (&key (nick "brlbot") (server "irc.freenode.net") (channel "#brlcad"))
  (setf *connection* (cl-irc:connect :username nick :realname "BRL Bot" :server server :nickname nick))
  (cl-irc:join *connection* channel)
  (setf *bot-thread* (bordeaux-threads:make-thread (lambda ()
						     (cl-irc:read-message-loop *connection*))
						   :name "brlbot")))

(defun stop-bot ()
  (cl-irc:quit *connection*)
  (bordeaux-threads:join-thread *bot-thread*))
