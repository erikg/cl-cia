(in-package :cl-cia)

(defun get-json-project-name (json)
  (cdr (assoc :name (cdr (assoc :project json)))))

(defun jsonrpc (str)
  (when str
    (let ((res (cl-json:decode-json-from-string str)))
      (format t "~a~%" (get-json-project-name res))
      0
      )))

