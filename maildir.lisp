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
		 (cl-ppcre:scan (concatenate 'string regex "[ \t]*(.*)[ \t]*$") x))
	       start)
	     lines)
    (when (and r1 r2)
      (subseq l (aref r1 0) (aref r2 0)))))

(defun snarf-all (regex lines)
  (remove nil
	  (mapcar
	   (lambda (x)
	     (multiple-value-bind (start end r1 r2)
		 (cl-ppcre:scan (concatenate 'string regex "[ \t\]*(.*)[ \t]*$") x)
	       (when (and start end r1 r2)
		 (subseq x (aref r1 0) (aref r2 0)))))
	   lines)))

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
	 (list-id (snarf "^List-Id:" f))
	 (date (datify (snarf "^Date:" f)))
	 (revision (snarf "^Revision:" f))
	 (author (snarf "^Author:" f))
	 (files (snarf-all "^Modified:" f))
	 (body '()))
    (when (and revision author)
      (let ((start (+ (position-if (lambda (x) (cl-ppcre:scan "^Log Message:$" x)) f) 2))
	    (end (position-if (lambda (x) (cl-ppcre:scan "^Modified Paths:$" x)) f)))
	(when (and start end)
	  (setf body (format nil "~{~a ~}" (subseq f start end)))))
      (values
       (make-instance 'commit :files files :revision revision :date date :user author :message body)
       (subseq list-id 0 (position #\Space list-id))))))

(defun process-mail-dir (&key (maildir +db-unprocessed-mail-dir+) (hooks '()))
  "Parse all messages in a mail dir, adding parsed commit messages to the list and applying the hooks"
  (bordeaux-threads:with-lock-held (*biglock*)
    (dolist (file (cl-fad:list-directory maildir))
      (multiple-value-bind (message project-name) (load-commit-from-mail-message file)
	(alexandria:when-let (project (find-project project-name))
	  (when (add-message project message)
	    (let ((res (if hooks (mapcar (lambda (x) (funcall x message)) hooks) '(t))))
	      (unless (find nil res)
		(rename-file file (merge-pathnames +db-processed-mail-dir+ (file-namestring file))))))))))
  (save-state))

(defun pump ()  (process-mail-dir))
(defvar *pump* '())
(defvar *pump-running* '())
(defun start-pump ()
  (unless (and *pump* (bordeaux-threads:threadp *pump*) (bordeaux-threads:thread-alive-p *pump*))
    (setf *pump-running* t)
    (setf *pump* (bordeaux-threads:make-thread
		  (lambda () (loop while *pump-running* do (progn (pump) (sleep 5))))
		  :name "cl-cia pumper"))))
(defun stop-pump ()
  (when (and *pump* *pump-running*)
    (setf *pump-running* '())
    (bordeaux-threads:join-thread *pump*)
    (setf *pump* '())))
