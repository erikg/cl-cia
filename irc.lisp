

(in-package :cl-cia)

(defvar +irc-line-length+ 420)

(defvar *bot-thread* '())
(defvar *connection* '())
(defvar *notice-wrangler* '())
(defvar *notice-wrangler-running* '())
(defvar *notice-lock* (bordeaux-threads:make-lock "ircbot-notice-lock"))
(defparameter +always-channels+ '("#notify"))
(defvar *add-todo-hook* (lambda (x) x))

(defun find-connection-by-name (name)
  (declare (ignore name))
  *connection*)
(defun find-network-by-connection (conn)
  (declare (ignore conn))
  "freenode")

(defun filestr (files)
  (cond
    ((> (length files) 3) (format nil "(~a ~a and ~a others)" (car files) (cadr files) (- (length files) 2)))
    ((> (length files) 1) (format nil "(~{~a~^ ~})" files))
    (t (car files))))

(defun ascii-ize (str col)
  (format nil "~C~2,'0d~a~C" #\etx col str #\etx))

(defun tidy-for-irc (msg)
  (string-trim " " (cl-ppcre:regex-replace-all "[ \\t\\n]+" (if (listp msg) (format nil "~{~a~^ ~}" msg) msg) " ")))
(defun format-commit (project message)
  (format nil "~a ~a: ~a"
	  (ascii-ize (format nil "~a:~a * ~a" (name project) (user message) (revision message)) 3)
	  (filestr (files message))
	  (tidy-for-irc (message message))))
(defmacro post (obj place)
  `(setf ,place (append ,place (list ,obj))))

(defun truncate-for-irc (msg &optional (len +irc-line-length+))
  (let ((m (substitute #\Space #\Newline msg)))
    (if (<= (length m) len)
	m
	(loop for i from len downto 0
	   until (eq (char m i) #\Space)
	   finally (return (concatenate 'string (subseq m 0 i) "..."))))))
(defun split-for-irc (msg &optional (len +irc-line-length+))
  (let ((m (substitute #\Space #\Newline msg)))
    (if (<= (length m) len)
	(list m)
	(loop for i from len downto 0
	   until (eq (char m i) #\Space)
	   finally (return (cons (subseq m 0 i) (split-for-irc (subseq m (+ i 1)))))))))
(defun split-to-3-for-irc (msg &optional (len +irc-line-length+))
  (let ((bits (split-for-irc msg len)))
    (if (<= (length bits) 3)
	bits
	(list (car bits) (cadr bits) (concatenate 'string (caddr bits) "...")))))

(defun post-message (channel msg &optional (network "freenode"))
  (bordeaux-threads:with-lock-held (*notice-lock*)
    (dolist (m (split-for-irc msg))
      (post (list network channel m) (notices *state*))))
  t)

(defun report-msg (project msg)
  (bordeaux-threads:with-lock-held (*notice-lock*)
    (dolist (m (split-for-irc msg))
      (dolist (target (append +always-channels+ (when project (channels project))))
	(post (list "freenode" target m) (notices *state*))))))

(defun report-commit (project message)
  (when (and project message)
    (dolist (b (split-to-3-for-irc (format-commit project message)))
      (report-msg project b))))

(defun notice-wrangler ()
  (sleep 1)
  (when (notices *state*)
    (bordeaux-threads:with-lock-held (*notice-lock*)
      (when (notices *state*)
	(let ((n (pop (notices *state*))))
;	  (handler-case
	      (cl-irc:privmsg (find-connection-by-name (car n)) (cadr n) (caddr n))
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

(defun respond (msg strl)
  (bordeaux-threads:with-lock-held (*notice-lock*)
    (dolist (str (if (listp strl) strl (list strl)))
      (push (list (find-network-by-connection (irc::connection msg)) (car (irc::arguments msg)) str) (notices *state*)))))

(defun report-commit-frequency-for-irc (proj timeval timetype)
  (mapcar (lambda (p)
	    (format nil "~a: ~{~{~a:~a~}~^, ~}"
		    (name p)
		    (count-commits-by-user-since (commits p) (local-time:timestamp- (local-time:now) timeval timetype))))
	  (if (listp proj) proj (list proj))))

(defun docmd (msg cmdstr)
  (let* ((cmds (split-sequence:split-sequence #\Space cmdstr))
	 (cmd (string-upcase (car cmds)))
	 (proj '()))
    (when (cdr cmds)
	(setf proj (find-project (format nil "~{~a~^ ~}" (cdr cmds)))))
    (unless proj (setf proj (find-project (car (irc::arguments msg)))))
    (case (intern cmd 'cl-cia)
      (day (respond msg (report-commit-frequency-for-irc proj 1 :day)))
      (week (respond msg (report-commit-frequency-for-irc proj 7 :day)))
      (month (respond msg (report-commit-frequency-for-irc proj 1 :month)))
      (year (respond msg (report-commit-frequency-for-irc proj 1 :year)))
      (all (respond msg (format nil "~{~{~a:~a~}~^, ~}" (count-commits-by-user-since (commits proj) (local-time:universal-to-timestamp 0)))))
      (ask (respond msg "Questions in the channel should be specific, informative, complete, concise, and on-topic.  Don't ask if you can ask a question first.  Don't ask if a person is there; just ask what you intended to ask them.  Better questions more frequently yield better answers.  We are all here voluntarily or against our will."))
      (todo (let ((todo `((who ,(irc::source msg))
			  (whence ,(car (irc::arguments msg)))
			  (body ,(string-trim " " (subseq cmdstr (length cmd))))
			  (created ,(local-time:now)))))
	      (funcall *add-todo-hook* todo)
	      (respond msg "OK, added to https://elfga.com/notify/todo"))))))

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
  (cl-irc:add-hook (find-connection-by-name "freenode") 'irc::irc-privmsg-message 'msg-hook)
  (cl-irc:add-hook (find-connection-by-name "freenode") 'irc::irc-notice-message 'notice-hook)
  (cl-irc:add-hook (find-connection-by-name "freenode") 'irc::ctcp-action-message 'action-hook)
  (when nickserv-passwd
    (cl-irc:privmsg (find-connection-by-name "freenode") "nickserv" (format nil "IDENTIFY ~A" nickserv-passwd)))
  (dolist (c channels)
    (cl-irc:join (find-connection-by-name "freenode") c))
  (setf *bot-thread* (bordeaux-threads:make-thread (lambda () (cl-irc:read-message-loop (find-connection-by-name "freenode"))) :name "cl-cia ircbot")))

(defun stop-bot ()
  (cl-irc:quit (find-connection-by-name "freenode") "EVACUATE! EVACUATE!")
;  (bordeaux-threads:join-thread *bot-thread*)
  (setf *bot-thread* '())
)
