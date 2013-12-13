;;;; cl-cia.lisp

(in-package #:cl-cia)

(defun read-file-to-list (file)
  (with-open-file (stream file :external-format :utf-8)
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

(defparameter +funny-field-names+ '("Added Paths"
				    "Directory Properties"
				    "Log Message"
				    "Modified Paths"
				    "New Revision"
				    "Property changes on"
				    "Removed Paths"
				    "Revision Links"))

(defun isarg (line)
  (when line
    (multiple-value-bind (start end) (cl-ppcre:scan "^[A-Z][A-Za-z0-9-_]*:" line)
      (if start (values start end)
	  (let ((match (find (subseq line 0 (position #\: line)) +funny-field-names+ :test #'string=)))
	    (when match
	      (values 0 (+ (length match) 1))))))))

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
		 (if (or (not nextargline) (>= nextargline 1))
		     (trimmulti (subseq lines 0 (when nextargline (+ nextargline 1))) end)
		     (list (string-trim " " (subseq (car lines) end)))))
	   (fieldinate (if nextargline
			   (nthcdr (+ nextargline 1) lines)
			   (cdr lines)))))))))

(defun split-mail-to-head-and-body (file)
  (alexandria:when-let ((lines (read-file-to-list file)))
    (alexandria:when-let ((p (position-if (lambda (x) (string= "" x)) lines)))
      (cons (subseq lines 0 p) (subseq lines (+ p 1))))))

(defun flob (mailfile)
  (let ((l (split-mail-to-head-and-body mailfile)))
    (values (fieldinate (car l)) (fieldinate (cdr l)))))

(defun mail-element (name set)
  (let ((l (remove "" (mapcar (lambda (x) (string-trim " " x)) (cdr (assoc name set :test #'string=))) :test #'string=)))
    (if (cdr l) l (car l))))
(defun load-commit-from-mail-message (raw-header raw-body &optional hooks)
  (let ((header (fieldinate raw-header))
	(body (fieldinate raw-body)))
    (let ((list-id (mail-element "List-Id" header))
	  ;; favor the body Date node, but fall back to header if needed
	  (date (datify (or (mail-element "Date" body) (mail-element "Date" header))))
	  (revision (or (mail-element "Revision" body) (mail-element "New Revision" body)))
	  (author (mail-element "Author" body))
	  ;; the actual commit log message
	  (log (or (mail-element "Log" body) (mail-element "Log Message" body)))
	  (files (or (mail-element "Modified Paths" body) (alexandria:flatten
							   (list
							    (mail-element "Modified" body)
							    (mail-element "Added" body)
							    (mail-element "Deleted" body)))))
	  (project '())
	  (url (mail-element "URL" body)))
      (when (and list-id date revision author log)
	;; SVN::Notify likes to shove "-----" lines in, so try to eat those
	(when (and (listp log) (cl-ppcre:scan "^-*$" (car log)) (pop log)))
	(when (and (listp files) (cl-ppcre:scan "^-*$" (car files))) (pop files))
	(setf project (find-project-by-list-id
		       (let ((list-id (mail-element "List-Id" header)))
			 (if (listp list-id)
			     (format nil "~{~a~}" list-id)
			     list-id))))
	(when (listp revision)
	  (unless url
	    (when (or (string-equal "http://" (subseq (cadr revision) 0 7)) (string-equal "https://" (subseq (cadr revision) 0 8)))
	      (setf url (cadr revision))))
	  (setf revision (car revision)))
	(when (listp log)
	  (setf log (format nil "~{~a~}" log)))
	(unless (consp files) (setf files (list files)))
	(when (and project revision author)
	  (let ((commit (make-instance 'commit :files files :revision revision :date date :user author :url url :message log)))
	    (dolist (h hooks) (unless (funcall h commit project) (return '())))
	    (values commit project)))))))

(defun process-mail-dir-abstract (func maildir processed-maildir hooks &key (verbose nil))
  (bordeaux-threads:with-lock-held (*biglock*)
    (dolist (file (cl-fad:list-directory maildir))
      (when verbose (format t "processing ~s~%" file))
      (handler-case
       (let ((l (split-mail-to-head-and-body file)))
	 (when (funcall func (car l) (cdr l) hooks)
	   (rename-file file (merge-pathnames processed-maildir (file-namestring file)))))
	(sb-int:stream-decoding-error (e) (format t "Went ~s wonky on ~s~%" e file) '())))))

(defun process-mail-dir (&key (maildir +db-unprocessed-mail-dir+) (processed-maildir +db-processed-mail-dir+) (hooks '()) (verbose nil))
  "Parse all messages in a mail dir, adding parsed commit messages to the list and applying the hooks"
  (process-mail-dir-abstract #'load-commit-from-mail-message maildir processed-maildir (if hooks hooks (list #'add-message)) :verbose verbose))

(defun process-xml-mail-dir (&key (maildir +db-unprocessed-xmlmail-dir+) (processed-maildir +db-processed-xmlmail-dir+) (hooks '()) (verbose nil))
  "Parse all messages in a mail dir, adding parsed commit messages to the list and applying the hooks"
  (process-mail-dir-abstract
   (lambda (header body hooks)
     (declare (ignore header))
     (let ((retval t))
       (dolist (xml (parsexml (format nil "~{~a~}" body)))
	 (let ((message (caddr xml))
	       (project (car (find-project (car xml)))))
	   (when (and (string-equal (car xml) "BRL-CAD")
		      (string-equal (cadr xml) "http://brlcad.org"))
	     (setf project (car (find-project "brl-cad wiki"))))
	   (if hooks
	       (not (position '() (mapcar (lambda (h) (funcall h message project)) hooks)))
	       (let ((res (add-message message project)))
		 (unless res (setf retval '()))
		 res))))
       retval))
   maildir processed-maildir hooks :verbose verbose))

(defun match-fields (fields pairs)
  ""
  (not (position '() (mapcar (lambda (x) (string= (mail-element (car x) fields) (cadr x))) pairs))))

(defun escape-for-regex (str)
  (coerce
   (alexandria:flatten
    (loop for c being the elements of str collect
	 (if (find c '(#\[ #\] #\? #\( #\) #\. #\\))
	     (cons #\\ c) c)))
   'string))

(defun do-gci-email (header-fields body hooks)
  (declare (ignore hooks header-fields))
  (setf body (nthcdr 9 body))
  (setf body (format nil "~{~a~^ ~}" (mapcar (lambda (x) (string-trim " " x)) (subseq body 0 (position-if (lambda (x) (when (> (length x) 2) (string= (subseq x 0 2) "--"))) body)))))
  (let*	
      ((id (car (cl-ppcre:all-matches-as-strings "(?<=gci201[0-9]/)[0-9]*(?=:)" body)))
       (name (car (cl-ppcre:all-matches-as-strings ".*(?= has left)" body)))
       (task (car (cl-ppcre:all-matches-as-strings "(?<=comment at ).*(?= http://)" body)))
       (title (car (cl-ppcre:all-matches-as-strings (concatenate 'string "(?<=" id ":   ).*?(?=  )") body)))
       (comment (car (cl-ppcre:all-matches-as-strings (concatenate 'string "(?<=" (escape-for-regex title) "  ).*?(?=   Greet)") body)))
       (message (concatenate 'string (ascii-ize "GCI" 3) ":" name " * " id " " task ": " title " - " comment)))
    (post-message "#brlcad" (truncate-for-irc message 200))))

(defun process-brlcad-gci-email (header body hooks)
  (let ((header-fields (fieldinate header)))
    (cond
      ((match-fields header-fields '(("X-Google-Appengine-App-Id" "s~google-melange")
				     ("List-Id" "BRL-CAD Tracker Mailing List <brlcad-tracker.lists.sourceforge.net>")
				     ("From" "no-reply@google-melange.appspotmail.com")))
       (do-gci-email header-fields body hooks))
      (t '()))))

(defun process-brlcad-gci-mail-dir (&key
				      (verbose nil)
				      (maildir (merge-pathnames "unhandled-mail/new/" +db-dir+))
				      (processed-maildir (merge-pathnames "unhandled-mail/cur/" +db-dir+))
				      (hooks '()))
  (process-mail-dir-abstract #'process-brlcad-gci-email maildir processed-maildir hooks :verbose verbose))
(defun test-gci ()
  (let ((l (split-mail-to-head-and-body #P"/home/erik/db/cia/unhandled-mail/new/1354120503.92902_3.crit.brlcad.org")))
    (process-brlcad-gci-email (car l) (cdr l) '())))
(defun pump (&key (verbose nil))
  (process-mail-dir :verbose verbose)
  (process-xml-mail-dir :verbose verbose)
  (save-state)
  (process-brlcad-gci-mail-dir :verbose verbose))
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
