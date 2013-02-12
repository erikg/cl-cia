;;;; cl-cia.lisp

(in-package #:cl-cia)

(defparameter +rcfile+ (merge-pathnames ".cia.lisp" (user-homedir-pathname)))
(when (probe-file +rcfile+) (load +rcfile+))

(defparameter +proplist+ '())
(defmacro prop (name form)
  `(progn
     (unless (boundp ',name) (defparameter ,name ,form))
     (push ',name +proplist+)))

(defun write-propfile (&optional (propfile +rcfile+))
  (labels ((prel (el)
		   (typecase el
		     (string (format nil "\"~a\"" el))
		     (list (format nil "(list ~{~a~^ ~})" (mapcar #'prel el)))
		     (pathname (format nil "#P\"~a\"" el))
		     (t el))))
    (with-open-file (out propfile :direction :output)
      (dolist (p +proplist+)
	(format out "(defparameter ~(~a~) ~a)~%" p
		(prel (symbol-value p)))))))

(prop +db-dir+ (merge-pathnames "db/cia/" (user-homedir-pathname)))
(prop +db-state+ (merge-pathnames "cia.state" +db-dir+))

; mail form assumes the procmail matching rule puts it in $HOME/db/cia/mail/
; with the trailing slash inferring mailbox dir instead of mbox file.

(prop +db-mail-dir+ (merge-pathnames "mail/" +db-dir+))
(prop +db-unprocessed-mail-dir+ (merge-pathnames "new/" +db-mail-dir+))
(prop +db-processed-mail-dir+ (merge-pathnames "cur/" +db-mail-dir+))

(prop +db-xmlmail-dir+ (merge-pathnames "xmlmail/" +db-dir+))
(prop +db-unprocessed-xmlmail-dir+ (merge-pathnames "new/" +db-xmlmail-dir+))
(prop +db-processed-xmlmail-dir+ (merge-pathnames "cur/" +db-xmlmail-dir+))

(prop +bot-nick+ "Notify")
(prop +bot-nickserv-passwd+ '())
(prop +bot-server+ "irc.freenode.net")
(prop +bot-realname+ "Commit Notification Bot - http://elfga.com/notify")
(prop +bot-ident+ "notify")
(prop +bot-channels+ '("#notify" "##notify"))

(defvar *biglock* (bordeaux-threads:make-lock "cl-cia"))

(defun load-state (&optional (file +db-state+))
  (if (probe-file file)
      (cl-store:restore file)
      (make-instance 'state :projects (list (make-instance 'project :name "BRL-CAD" :hooks (list #'report-commit))))))
(defun save-state (&key (place *state*) (file +db-state+) (force '()))
  (bordeaux-threads:with-lock-held (*biglock*)
    (when (or force (dirty place))
      (setf (dirty place) '())
      (cl-store:store place file))))

(defclass project-association ()
  ((project :accessor project :initarg :project :initform '())
   (alias :accessor alias :initarg :alias :initform '())
   (flags :accessor flags :initarg :flags :initform '())))
(defmethod print-object ((a project-association) stream)
  (format stream "[~a:~a~@[:~a~]]" (name (project a)) (alias a) (flags a)))
(defclass user-mixin ()
  ((aliases :accessor aliases :initarg :aliases :initform '())))
(defmethod print-object ((o user-mixin) stream)
  (format stream "<~a ~a>" (type-of o) (aliases o)))

(defclass project ()
  ((oid :accessor oid :initform '())
   (name :accessor name :initarg :name)
   (created :accessor created :initform (local-time:now))
   (users :accessor users :initarg :users :initform '())
   (channels :accessor channels :initarg :channels :initform '())
   (list-id-regex :accessor list-id-regex :initarg :list-id-regex :initform '())
   (commits :accessor commits :initform '() :initarg :commits)
   (hooks :accessor hooks :initform '() :initarg :hooks)))
(defmethod print-object ((p project) stream)
  (format stream "<Project ~a ~a (~d commits)>" (name p) (channels p) (length (commits p))))

(defmethod find-user ((p project) name)
  (find-if (lambda (user)
	     (find-if (lambda (alias)
			(and
			 (eq p (project alias))
			 (find name (alias alias) :test #'string-equal)))
		      (aliases user)))
	   (users p)))
(defun add-user-to-project (p u)
  (declare (type project p) (type user-mixin u))
  (unless (find-association p u)
    (push (make-instance 'project-association :project p) (aliases u))))
(defmethod find-association ((p project) (u user-mixin))
  (find-if (lambda (a) (eq (project a) p)) (aliases u)))
(defmethod flagp ((p project) (u user-mixin) flag)
  (alexandria:when-let ((association (find-association p u)))
    (find flag (flags association))))
(defmethod set-flag ((p project) (u user-mixin) flag)
  (alexandria:when-let ((association (find-association p u)))
    (unless (find flag (flags association))
      (push flag (flags association)))))
(defmethod unset-flag ((p project) (u user-mixin) flag)
  (alexandria:when-let ((association (find-association p u)))
    (when (find flag (flags association))
      (setf (flags association) (remove flag (flags association))))))

(defmethod adminp ((p project) (u user-mixin)) (flagp p u 'admin))
(defmethod set-admin ((p project) (u user-mixin)) (set-flag p u 'admin))
(defmethod unset-admin ((p project) (u user-mixin)) (unset-flag p u 'admin))

(defclass state ()
  ((projects :accessor projects :initarg :projects  :initform '())
   (notices :accessor notices :initarg :notices :initform '())
   (users :accessor users :initarg :users :initform '())
   (dirty :accessor dirty :initarg :dirty :initform '())))
(defvar *state* '())
(defun add-project (project)
  (push project (projects *state*)))
(defmethod find-project ((name t) &optional (state *state*))
  (remove nil (mapcar (lambda (x)
			(when
			    (or
			     (string-equal (name x) name)
			     (find name (channels x) :test #'string-equal))
			  x))
	  (projects state))))
(defun find-project-by-list-id (list-id)
  (find-if (lambda (x)
	     (when (list-id-regex x)
	       (cl-ppcre:scan (list-id-regex x) list-id)))
	   (projects *state*)))
(defun all-channels (&optional (state *state*))
  (remove-duplicates (alexandria:flatten (cons '("#notify" "##notify") (mapcar #'channels (projects state)))) :test #'string-equal))
(defclass commit ()
  ((oid :accessor oid :initform '())
   (timestamp :accessor timestamp :initform (local-time:now))
   (date :accessor date :initarg :date :initform (local-time:now))
   (user :accessor user :initarg :user)
   (revision :accessor revision :initarg :revision)
   (files :accessor files :initarg :files :initform '())
   (url :accessor url :initarg :url :initform '())
   (message :accessor message :initarg :message :initform '())))

(defmethod print-object ((c commit) stream)
  (format stream "#<Commit: ~a@~a: ~a (at ~a)>" (user c) (revision c) (message c) (date c)))
(defmethod equals ((c1 commit) (c2 commit))
  (and (string-equal (user c1) (user c2))
       (string-equal (revision c1) (revision c2))
       (or
	(and (stringp c1) (stringp c2) (string-equal (message c1) (message c2)))
	(eq c1 c2))))

(defmethod find-commit ((p project) rev)
  (find-if (lambda (x) (string-equal (revision x) rev)) (commits p)))

(defun commit-has-file (commit filename)
  (when (find filename (files commit) :test #'string=)
    t))

(defun resort-commits (project)
  (bordeaux-threads:with-lock-held (*biglock*)
    (setf (commits project) (sort (copy-list (commits project)) (lambda (x y) (local-time:timestamp> (date x) (date y))))))
  t)

(defun remove-commit (project commit &key (test #'equals))
  (bordeaux-threads:with-recursive-lock-held (*biglock*)
    (setf (commits project) (remove commit (commits project) :test test))))

(defvar *message-hooks* '())
(defvar *global-message-hooks* '())
(defun message-seen (project message)
  (when (and project message)
    (find message (commits project) :test #'equals)))

(defun add-messages (messages project)
  (when (and project messages)
    (not
     (position
      '()
      (mapcar
       (lambda (message)
	 (when (listp (message message))
	   (setf (message message) (format nil "~{~a~}" (message message))))
	 (unless (message-seen project message)
	   (dolist (hook (hooks project)) (funcall hook project message))
	   (dolist (hook *global-message-hooks*) (funcall hook project message))
	   (setf (dirty *state*) t)
	   (push message (commits project))
	   t))
       (if (listp messages) messages (list messages)))))))
(defun add-message (message project) (add-messages (list message) project))

(defun in-the-last (commits &optional start end)
  (unless start (setf start (local-time:timestamp- (local-time:now) 24 :hour)))
  (unless end (setf end (local-time:now)))
  (remove-if (lambda (x)
               (or
                (local-time:timestamp< (cl-cia::date x) start)
                (local-time:timestamp> (cl-cia::date x) end)))
             commits))

(defun count-commits-by-user-since (commits &optional start end)
  (let ((last24hr (in-the-last commits start end))
        (bucket '()))
    (dolist (c last24hr)
      (let ((suck (assoc (user c) bucket :test #'string-equal)))
        (if suck
            (incf (cadr suck))
            (push (list (user c) 1) bucket))))
    (sort bucket (lambda (x y) (> (cadr x) (cadr y))))))

(defun start ()
  (unless *state*
    (setf *state* (load-state)))
  (setf *message-hooks* (list #'report-commit))
  (start-pump)
  (bot)
  (sleep 2)
  (start-notice-wrangler))
(defun stop ()
  (stop-bot)
  (stop-pump)
  (save-state))
