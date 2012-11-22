;;;; cl-cia.lisp

(in-package #:cl-cia)

(defun read-file-to-list (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
       while line collect line)))

(defun datify (datestr)
  "Attempt to convert a string to a local-time:timestamp object, using local-time:now if unable"
  ; and it always seems unable.
  (if datestr
      (handler-case
	  (local-time:parse-timestring datestr)
	(local-time::invalid-timestring () (local-time:now)))
      (local-time:now)))

(defun isarg (line)
  (when line (cl-ppcre:scan "^[A-Z][^:]*:" line)))

(defun trimmulti (lines offset)
  ""
  (if (<= (length (car lines)) offset)
      (cdr lines)
      (cons (string-trim " " (subseq (car lines) offset)) (cdr lines))))

(defun fieldinate (lines)
  "Given a set of lines following the email field format, build an association list of fields"
  (when lines
    (multiple-value-bind (start end) (isarg (car lines))
      (unless start (return-from fieldinate '()))
      (let ((nextargline (position-if #'isarg (cdr lines))))
	(let ((argname (subseq (car lines) start (- end 1))))
	  (cons
	   (cons argname
		 (if (or (not nextargline) (> nextargline 1))
		     (trimmulti (subseq lines 0 (when nextargline (+ nextargline 1))) end)
		     (list (string-trim " " (subseq (car lines) end)))))
	   (fieldinate (if nextargline
			   (nthcdr (+ nextargline 1) lines)
			   (cdr lines)))))))))

(defun flob (&optional (mailfile #P"/home/erik/db/cia/mail2/new/1353538242.48694_2.crit.brlcad.org"))
  (alexandria:when-let ((lines (read-file-to-list mailfile)))
    (alexandria:when-let ((p (position-if (lambda (x) (string= "" x)) lines)))
      (alexandria:when-let ((header (fieldinate (subseq lines 0 p)))
			    (body (fieldinate (subseq lines (+ p 1)))))
	(values header body)))))

(defun mail-element (name set)
  (let ((l (remove "" (mapcar (lambda (x) (string-trim " " x)) (cdr (assoc name set :test #'string=))) :test #'string=)))
    (if (cdr l) l (car l))))
(defun load-commit-from-mail-message (mailfile)
  (multiple-value-bind (header body)
      (flob mailfile)
    (alexandria:when-let ((list-id (mail-element "List-Id" header))
			  ;; favor the body Date node, but fall back to header if needed
			  (date (datify (or (mail-element "Date" body) (mail-element "Date" header))))
			  (revision (or (mail-element "Revision" body) (mail-element "New Revision" body)))
			  (author (mail-element "Author" body))
			  ;; this probably needs more work
			  (files (or (mail-element "Modified Paths" body) (mail-element "Modified" body)))
			  ;; the actual commit log message
			  (log (or (mail-element "Log" body) (mail-element "Log Message" body))))
      ;; SVN::Notify likes to shove "-----" lines in, so try to eat those
      (when (and (listp log) (cl-ppcre:scan "^-*$" (car log)) (pop log)))
      (when (and (listp files) (cl-ppcre:scan "^-*$" (car files))) (pop files))
      (unless (eq (type-of files) 'list) (setf files (list files)))
      (values
       (make-instance 'commit :files files :revision revision :date date :user author :message log)
       list-id))))

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
