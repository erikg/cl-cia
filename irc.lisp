

(in-package :cl-cia)

(defvar *bot-thread* '())
(defvar *connection* '())
(defvar *notice-wrangler* '())
(defvar *notice-wrangler-running* '())
(defvar *notice-lock* (bordeaux-threads:make-lock "ircbot-notice-lock"))

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
(defmacro post (obj place)
  `(setf ,place (append ,place (list ,obj))))

(defun report-msg (project msg)
  (bordeaux-threads:with-lock-held (*notice-lock*)
    (post (list *connection* "#notify" msg) (notices *state*))
    (post (list *connection* "##notify" msg) (notices *state*))
    (when project
      (dolist (c (channels project))
	(post (list *connection* c msg) (notices *state*))))))
(defun report-commit (project message)
  (report-msg project (format-commit project message)))

(defun notice-wrangler ()
  (sleep 1)
  (when (notices *state*)
    (bordeaux-threads:with-lock-held (*notice-lock*)
      (when (notices *state*)
	(let ((n (pop (notices *state*))))
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
			     :name "cl-cia notice wrangler"))))

(defun stop-notice-wrangler ()
  (setf *notice-wrangler-running* '())
  (bordeaux-threads:join-thread *notice-wrangler*))

(defun respond (msg str)
  (bordeaux-threads:with-lock-held (*notice-lock*)
    (push (list (irc::connection msg) (car (irc::arguments msg)) str) (notices *state*))))

(defun docmd (msg cmdstr)
  (let* ((cmds (split-sequence:split-sequence #\Space cmdstr))
	 (cmd (string-upcase (car cmds)))
	 (proj '()))
    (setf proj (find-project (car (irc::arguments msg))))
    (unless proj (setf proj (find-project (car (last cmds)))))
    (case (intern cmd 'cl-cia)
      (day (respond msg (format nil "~{~{~a:~a~}~^, ~}" (count-commits-by-user-since (commits proj) (local-time:timestamp- (local-time:now) 1 :day)))))
      (week (respond msg (format nil "~{~{~a:~a~}~^, ~}" (count-commits-by-user-since (commits proj) (local-time:timestamp- (local-time:now) 7 :day)))))
      (month (format t "Bingk~%") (respond msg (format nil "~{~{~a:~a~}~^, ~}" (count-commits-by-user-since (commits proj) (local-time:timestamp- (local-time:now) 1 :month)))))
      (year (respond msg (format nil "~{~{~a:~a~}~^, ~}" (count-commits-by-user-since (commits proj) (local-time:timestamp- (local-time:now) 1 :year)))))
      (all (respond msg (format nil "~{~{~a:~a~}~^, ~}" (count-commits-by-user-since (commits proj) (local-time:universal-to-timestamp 0)))))
      (todo (Push (list (irc::user msg) (car (irc::arguments msg)) (string-trim " " (subseq cmdstr (length cmd)))) (todo *state*)) (respond msg "OK")))))

(defun msg-hook (msg)
  (when (> (length (cadr (irc::arguments msg))) 9)
    (when (string= (subseq (cadr (irc::arguments msg)) 0 7) "!notify")
      (docmd msg (string-trim " " (subseq (cadr (irc::arguments msg)) 8))))))

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
  (setf *bot-thread* (bordeaux-threads:make-thread (lambda () (cl-irc:read-message-loop *connection*)) :name "cl-cia ircbot")))

(defun stop-bot ()
  (cl-irc:quit *connection* "EVACUATE! EVACUATE!")
;  (bordeaux-threads:join-thread *bot-thread*)
  (setf *bot-thread* '())
)
