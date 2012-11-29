
(in-package :cl-cia)

(defun gettags (xml tag)
  (let ((e (find-if (lambda (x) (or (and (symbolp x) (eq tag x)) (and (listp x) (symbolp (car x)) (eq (car x) tag)))) xml)))
    (when (and e (listp e)) (cdr e))))
(defun gettag (xml tag) (let ((tags (gettags xml tag))) (when tags (car tags))))

(defun parsexml (str)
  (alexandria:when-let ((p (s-xml:parse-xml-string str)))
    (when (eq (car p) :|message|)
      (let* ((source (gettags p :|source|))
	     (proj (gettag source :|project|))
	     (branch (gettag source :|branch|))
	     (timestamp (parse-integer (gettag p :|timestamp|)))
	     (body (gettags p :|body|)))
	(mapcar (lambda (c)
		  (let ((author (gettag c :|author|))
			(rev (gettag c :|revision|))
			(log (gettag c :|log|))
			(url (gettag c :|url|))
			(files (gettags c :|files|)))
		    (when (and url log
			       (> (length log) (+ (length url) 3))
			       (string= (concatenate 'string " - " url) (subseq log (- (length log) (length url) 3))))
		      (setf log (subseq log 0 (- (length log) (length url) 3))))
		    (list
		     proj
		     branch
		     (make-instance 'commit
				    :user author
				    :revision rev
				    :date (local-time:unix-to-timestamp timestamp)
				    :files (mapcar (lambda (file) (string-trim " \t" (cadr file))) files)
				    :url url
				    :message log))))
		body)))))

(defun xmlrpc (str)
  (when str
    (let ((res (parsexml str)))
      (dolist (r res)
	(alexandria:when-let ((proj (find-project (car r))))
	  (add-message (caddr r) proj))))))

(defun parse-svn-logentry (xml &key user-map-func)
  (unless user-map-func (setf user-map-func (lambda (name) name)))
  (when (eq (caar xml) :|logentry|)
    (let ((author (funcall user-map-func (cadr (assoc :|author| xml))))
          (revision (parse-integer (nth (+ (position :|revision| (car xml)) 1) (car xml))))
          (date (local-time:parse-timestring (cadr (assoc :|date| xml))))
	  (files '())
          (msg '()))
      (unless (find :|msg| xml) ; to handle empty commit messages
	(setf msg (cadr (assoc :|msg| xml))))
      (setf files (mapcar #'cadr (cdr (assoc :|paths| xml))))
      (make-instance 'commit :user author :revision revision :date date :message msg :files files))))

(defun parse-svn-log-xml (xml &key user-map-func)
  (when (eq (car xml) :|log|)
    (mapcar (lambda (x) (parse-svn-logentry x :user-map-func user-map-func)) (cdr xml))))

(defun parse-svn-log-string (str &key user-map-func)
  (parse-svn-log-xml (s-xml:parse-xml (make-string-input-stream str)) :user-map-func user-map-func))

(defun parse-svn-log-file (file &key user-map-func)
  (with-open-file (stream file :external-format :utf-8)
    (parse-svn-log-xml (s-xml:parse-xml stream) :user-map-func user-map-func)))

(defun brlcad-usermap (oldname)
  (let ((map '(("bparker" "bob1961")
	       ("butler" "lbutler")
	       ("erikg" "erikgreenwald")
	       ("jlowens" "jlowenz")
	       ("jra" "johnranderson")
	       ("justin" "twingy")
	       ("kermit" "g2asc")
	       ("mgillich" "mjgillich")
	       ("mmark" "mm")
	       ("morrison" "brlcad"))))
    (if (assoc oldname map :test #'string-equal)
	(cadr (assoc oldname map :test #'string-equal))
	oldname)))
; (s

(defun import-svn-log-file-to-project (project file &key user-map-func)
  (bordeaux-threads:with-lock-held (*biglock*)
    (let ((commits (parse-svn-log-file file :user-map-func user-map-func)))
      (dolist (c (commits project))
	(setf commits (remove c commits :test 'equals)))
      (let ((len (length commits)))
	(setf (commits project) (sort (append (commits project) commits) (lambda (x y) (> (revision x) (revision y)))))
	len))))
