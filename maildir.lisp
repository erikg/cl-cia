;;;; cl-cia.lisp

(in-package #:cl-cia)

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
    (when (and r1 r2)
      (subseq l (aref r1 0) (aref r2 0)))))

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
    (when (and revision author)
      (let ((start (+ (position-if (lambda (x) (cl-ppcre:scan "^Log Message:$" x)) f) 2))
	    (end (position-if (lambda (x) (cl-ppcre:scan "^Modified Paths:$" x)) f)))
	(when (and start end)
	  (setf body (format nil "~{~a ~}" (subseq f start end)))))
      (setf revision (parse-integer revision :junk-allowed t))
      (make-instance 'commit :revision revision :timestamp date :user author :message body))))

(defun process-mail-dir (&key (maildir +db-unprocessed-mail-dir+) (hooks '()))
  "Parse all messages in a mail dir, adding parsed commit messages to the list and applying the hooks"
  (bordeaux-threads:with-lock-held (*biglock*)
    (dolist (file (cl-fad:list-directory maildir))
      (alexandria:when-let ((message (load-commit-from-mail-message file)))
	(unless (message-seen message)
	  (push message *messages*)
	  (if hooks
	      (mapcar (lambda (x) (funcall x message)) hooks)
	      t))))))
