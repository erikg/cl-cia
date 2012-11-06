;;;; cl-cia.lisp

(in-package #:cl-cia)

(defparameter +db-dir+ (merge-pathnames "db/cia/" (user-homedir-pathname)))

; mail form assumes the procmail matching rule puts it in $HOME/db/cia/mail/
; with the trailing slash inferring mailbox dir instead of mbox file.
(defparameter +db-mail-dir+ (merge-pathnames "mail/" +db-dir+))
(defparameter +db-unprocessed-mail-dir+ (merge-pathnames "unprocessed/" +db-mail-dir+))

(defvar *biglock* (bordeaux-threads:make-lock "cl-cia"))

(defclass commit ()
  ((timestamp :accessor timestamp :initarg :timestamp :initform (local-time:now))
   (user :accessor user :initarg :user)
   (revision :accessor revision :initarg :revision)
   (message :accessor message :initarg :message)))

(defmethod print-object ((c commit) stream)
  (format stream "#<Commit: ~a@~a: ~a (at ~a)>" (user c) (revision c) (message c) (timestamp c)))
(defmethod equals ((c1 commit) (c2 commit))
  (and (string-equal (user c1) (user c2)) (= (revision c1) (revision c2))))

(defvar *messages* '())
(defun message-seen (message &optional (messages *messages*))
  (find message messages :test #'equals))

(defun pump ()
  (process-mail-dir :hook
		    (list
		     #'report-commit)))

(defvar *pump* '())
(defvar *pump-running* '())
(defun start-pump ()
  (unless *pump*
    (setf *pumper* (bordeaux-threads:make-thread
		    (lambda ()
		      (loop while *pump-running* do (progn
						      (pump) (sleep 5))))))))
(defun stop-pump ()
  (when (and *pump* *pump-running*)
    (setf *pump-running* '())
    (bordeaux-threads:join-thread *pump*)
    (setf *pump* '())))

(defun start ()
  (bot)
  (sleep 5)
  (start-pump))
(defun stop ()
  (stop-bot)
  (stop-pump))
