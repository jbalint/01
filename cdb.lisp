(defun append-row (row tbl) (cons row tbl))

(let ((db (append-row '(1 'a)
	    (append-row '(2 'b)
			(append-row '(3 'c) '())))))
  (query db (lambda (r) (< 1 (car r)))))

(defun query (db pred) (delete nil (mapcar (lambda (r) (if (funcall pred r) r nil)) db)))

(defun get-last (l)
	   (if (eq (cdr l) nil)
	       (car l)
	       (get-last (cdr l))))

(defun element-at (l idx)
  (if (= 0 idx) (car l) (element-at (cdr l) (- idx 1))))

; assemble an associate list with the relevant data from the request
(defun process-request-line (l p)
  (let ((e (cond ((search "GET " l) (cons :method :get))
		 ((search "PUT " l) (cons :method :put))
		 ((let ((pos (search "/file/" l)))
		    (if pos (cons :file (subseq l (+ 6 pos) (or (search " " l) (length l)))))))
		 ((let ((pos (search "/data/" l)))
		    (if pos (cons :data (subseq l (+ 6 pos) (or (search " " l) (length l)))))))
		 ((search "Content-Length: " l) (cons :length (read-from-string (subseq l 16))))
		 ((search "Content-Type: " l) (cons :type (subseq l 14))))))
    (case e
      ((nil) p)
      (t (process-request-line (subseq l (+ 1 (or (search " " l) 0))) (cons e p))))))

(defun send-file-response (st f)
  (let ((mime (or (cdr (assoc :type f)) "text/html")))
    (format st "HTTP/1.0 200 OK~%")
    (format st "Content-Type: ~a~%~%" mime)
    (format st "~a" (cdr (assoc :contents f)))))

(defun handle-put-request (st p db)
  ; read the body into the `data' variable
  (let* ((buf (make-array (or (cdr (assoc :length p)) 50000)))
	 (len (read-sequence buf st))
	 (data (coerce (subseq buf 0 len) 'string))
	 (f (cdr (assoc :file p))) ; filename
	 (d (cdr (assoc :data p))) ; recordid
	 (type (or (cdr (assoc :type p)) "text/html")))
    (cond (f (let* ((db1 (db-del-file db f))
		    (db2 (db-add-file db1 (pairlis '(:type :contents :name) (list type data f)))))
	       (format st "HTTP/1.0 200 OK~%")
	       db2))
	  (d nil)
	  (t nil))))

(defun handle-get-request (st p db)
  (let ((f (cdr (assoc :file p)))
	(d (cdr (assoc :data p))))
    (cond (f (send-file-response st (find-file (car db) f)))
	  (d nil) ; **********************TODO -> get data
	  (t nil))))

(defun find-file (files name)
  (find-if (lambda (r) (string= name (cdr (assoc :name r)))) files))

(defun handle-request (st p db)
  (let ((l (read-line st)))
    (if (< 5 (length l)) ; we still have more request headers
	(handle-request st (process-request-line l p) db)
	(case (cdr (assoc :method p))
	  (:put (handle-put-request st p db))
	  (:get (handle-get-request st p db))
	  (t (format st "HTTP/1.0 400 Invalid method~%~%"))))))

; first list -> files ("text/html", "<html>")
; second list -> data records
(setf db '(nil nil))

(defun db-add-file (db r) (let ((files (car db))
			     (data (cdr db)))
			    (cons (cons r files) data)))
(defun db-del-file (db name) (let* ((data (cdr db))
				    (namematch (lambda (r) (if (not (string= name (cdr (assoc :name r)))) r)))
				    (newfiles (delete nil (mapcar namematch (car db)))))
			       (list newfiles data)))

(let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
  (setf (sb-bsd-sockets:sockopt-reuse-address s) t)
  (sb-bsd-sockets:socket-bind s #(50 57 143 141) 1098)
  (sb-bsd-sockets:socket-listen s 1)
  (let* ((c (sb-bsd-sockets:socket-accept s))
	 (st (sb-bsd-sockets:socket-make-stream c :output t :input t))
	 (newdb (handle-request st nil db)))
    (if newdb (setf db newdb)
	(format st "HTTP/1.0 500 Internal Server Error~%~%"))
    (force-output st)
    (sb-bsd-sockets:socket-close c))
  (sb-bsd-sockets:socket-close s))