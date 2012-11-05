;;;; cl-cia.lisp

(in-package #:cl-cia)

(defparameter +dbdir+ (merge-pathnames "db/cia/" (user-homedir-pathname)))

; mail form assumes the procmail matching rule puts it in $HOME/db/cia/mail/
; with the trailing slash inferring mailbox dir instead of mbox file.
(defparameter +dbmaildir+ (merge-pathnames "mail/" +dbdir+))

(defclass commit ()
  ((timestamp :accessor timestamp :initarg :timestamp :initform (local-time:now))
   (user :accessor user :initarg :user)
   (revision :accessor revision :initarg :revision)
   (message :accessor message :initarg :message)))

(defmethod print-object ((c commit) stream)
  (format stream "#<Commit: ~a@~a: ~a (at ~a)>" (user c) (revision c) (message c) (timestamp c)))
(defmethod equals ((c1 commit) (c2 commit))
  (and (string-equal (user c1) (user c2)) (= (revision c1) (revision c2))))

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
	 (revision (snarf "^Revision:" f))
	 (author (snarf "^Author:" f))
	 (body '()))
    (let ((start (+ (position-if (lambda (x) (cl-ppcre:scan "^Log Message:$" x)) f) 2))
	  (end (position-if (lambda (x) (cl-ppcre:scan "^Modified Paths:$" x)) f)))
      (when (and start end)
	(setf body (format nil "~{~a ~}" (subseq f start end)))))
    (make-instance 'commit :revision revision :timestamp date :user author :message body)))
