

(in-package :cl-cia)

(defvar *bot-thread* '())
(defvar *connection* '())
(defvar *notice-wrangler* '())
(defvar *notice-wrangler-running* '())
(defvar *notice-lock* (bordeaux-threads:make-lock "ircbot-notice-lock"))
(defvar *notices* '())

(defun filestr (files)
  (cond
    ((> (length files) 3) (format nil "(~a ~a and ~a others)" (car files) (cadr files) (- (length files) 2)))
    ((> (length files) 1) (format nil "(~{~a~^ ~})" files))
    (t (car files))))

(defun ascii-ize (str col)
  (format nil "~C~2,'0d~a~C" #\etx col str #\etx))

(defun format-commit (project message)
  (format nil "~a ~a: ~a"
	  (ascii-ize (format nil "~a:~a * ~a" (name project) (user message) (revision message)) 3)
	  (filestr (files message))
	  (message message)))

(defun report-commit (project message)
  (let ((msg (format-commit project message)))
    (bordeaux-threads:with-lock-held (*notice-lock*)
      (setf *notices*
	    (append *notices* (list (list *connection* (channel project) msg)
				    (list *connection* "#notify" msg)
				    (list *connection* "##notify" msg)))))))

(defun notice-wrangler ()
  (sleep 1)
  (when *notices*
    (bordeaux-threads:with-lock-held (*notice-lock*)
      (when *notices*
	(let ((n (pop *notices*)))
;	  (handler-case
	      (apply #'cl-irc:privmsg n)
;	    (t '()))
)))))

(defun start-notice-wrangler ()
  (bordeaux-threads:with-lock-held (*notice-lock*)
    (setf *notice-wrangler-running* t)
    (setf *notice-wrangler* (bordeaux-threads:make-thread
			     (lambda ()
			       (loop while *notice-wrangler-running* do (notice-wrangler)))
			     :name "notice wrangler"))))

(defun stop-notice-wrangler ()
  (setf *notice-wrangler-running* '())
  (bordeaux-threads:join-thread *notice-wrangler*))

(defun respond (msg str)
  (bordeaux-threads:with-lock-held (*notice-lock*)
    (push (list (irc::connection msg) (car (irc::arguments msg)) str) *notices*))) 

(defun msg-hook (msg)
  (when (> (length (cadr (irc::arguments msg))) 9)
    (when (string= (subseq (cadr (irc::arguments msg)) 0 7) "!notify")
      (let ((cmd (subseq (cadr (irc::arguments msg)) 8)))
	(alexandria:if-let ((len (find cmd '(("Day" 1 :day) ("Week" 7 :day) ("Month" 1 :month) ("year" 1 :year) ("All" 99 :year)) :test (lambda (cmd def) (when (string-equal (car def) cmd) def)))))
	  (respond msg (format nil "~{~{~a:~a~}~^, ~}" (count-commits-by-user-since (commits (find-project "brl-cad")) (local-time:timestamp- (local-time:now) (cadr len) (caddr len)))))
	  (respond msg "es schlummert")))))
  '())

(defun notice-hook (msg)
  (declare (ignore msg))
  '())

(defvar *excuses* '())
(defun get-excuse ()
  (unless *excuses*
    (setf *excuses* (sort (list "No thanks"
				"I need an adult! I need an adult!"
				"Ugh, you're not my type"
				"get offa me!")
			  #'<
			  :key (lambda (x)
				 (declare (ignore x))
				 (random 1.0)))))
  (pop *excuses*))
(defun action-hook (msg)
  (let ((content (cadr (irc::arguments msg))))
    (when (and (eq (irc::ctcp-message-type content) :action)
	       (string-equal (subseq content 1 (- (length content) 1)) "ACTION hugs notify"))
      (respond msg (get-excuse))))
  '())

(defun bot (&key (nick +bot-nick+) (ident +bot-ident+) (server +bot-server+) (channels +bot-channels+) (realname +bot-realname+) (nickserv-passwd +bot-nickserv-passwd+))
  (setf *connection* (cl-irc:connect :username ident :realname realname :server server :nickname nick))
  (cl-irc:add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (cl-irc:add-hook *connection* 'irc::irc-notice-message 'notice-hook)
  (cl-irc:add-hook *connection* 'irc::ctcp-action-message 'action-hook)
  (when nickserv-passwd
    (cl-irc:privmsg *connection* "nickserv" (format nil "IDENTIFY ~A" nickserv-passwd)))
  (dolist (c channels)
    (cl-irc:join *connection* c))
  (setf *bot-thread* (bordeaux-threads:make-thread (lambda () (cl-irc:read-message-loop *connection*)) :name "ircbot")))

(defun stop-bot ()
  (cl-irc:quit *connection* "EVACUATE! EVACUATE!")
;  (bordeaux-threads:join-thread *bot-thread*)
  (setf *bot-thread* '())
)
