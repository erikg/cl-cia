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

(defun read-file-to-list (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
	 while line collect line)))

(defun snarf (regex lines)
  "Return contents of a : type line matching regex"
  (let ((start '()) (end '()) (r1 '()) (r2 '()) (l '()))
    (find-if (lambda (x)
	       (setf l x)
	       (multiple-value-setq (start end r1 r2)
		 (cl-ppcre:scan (concatenate 'string regex "[ \t]*(.*)") x))
	       start)
	     lines)
    (subseq l (aref r1 0) (aref r2 0))))

(defun datify (datestr)
  "Attempt to convert a string to a local-time:timestamp object, using local-time:now if unable"
  ; and it always seems unable.
  (if datestr
      (handler-case
	  (local-time:parse-timestring datestr)
	(local-time::invalid-timestring () (local-time:now)))
      (local-time:now)))

(defun load-commit-from-mail-message (mailfile)
  (let* ((f (read-file-to-list mailfile))
;	 (subj (snarf "^Subject:" f))
	 (date (datify (snarf "^Date:" f)))
	 (revision (parse-integer (snarf "^Revision:" f) :junk-allowed t))
	 (author (snarf "^Author:" f))
	 (body '()))
    (let ((start (+ (position-if (lambda (x) (cl-ppcre:scan "^Log Message:$" x)) f) 2))
	  (end (position-if (lambda (x) (cl-ppcre:scan "^Modified Paths:$" x)) f)))
      (when (and start end)
	(setf body (format nil "~{~a ~}" (subseq f start end)))))
    (make-instance 'commit :revision revision :timestamp date :user author :message body)))

(defun process-mail-dir (&key (maildir +db-unprocessed-mail-dir+) (hooks '()))
  "Parse all messages in a mail dir, adding parsed commit messages to the list and applying the hooks"
  (bordeaux-threads:with-lock-held (*biglock*)
    (dolist (file (cl-fad:list-directory maildir))
      (alexandria:when-let ((message (load-commit-from-mail-message file)))
	(unless (message-seen message)
	  (push message *messages*)
	  (dolist (h hooks) (funcall h message)))))))
