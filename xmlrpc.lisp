
(in-package :cl-cia)

#|

<message> 
  <generator>
    <name>CIA Trac plugin</name>
    <version>0.1</version>
  </generator>
  <source>
    <project>%s</project>
    <module>%s</module>
  </source>
  <body>
    <commit>
      <revision>%s</revision>
      <author>%s</author>
      <log>%s</log>
    </commit>
  </body>
</message>

|#

(defun xmlrpc (str)
  (s-xml:parse-xml (make-string-input-stream str)))

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
