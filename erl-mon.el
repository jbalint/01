
;; (defun erl-mon ()
;;   (interactive)
;;   (setq tabulated-list-format [("Application" 18 t)
;; 							   ("Class" 10 t)
;; 							   ("Vsn" 12 t)
;; 							   ("Description" 0 t)])
;;   (setq tabulated-list-padding 2)
;;   (setq tabulated-list-sort-key (cons "Application" nil))
;;   (tabulated-list-init-header)
;;   (setq tabulated-list-entries (list
;; 								(list "1" ["1" "2" "3" "4"])
;; 								(list "2" ["a" "b" "c" "d"])))
;;   (tabulated-list-print t))

;; (defun em--proc-name-to-tuple (x)
;;   (list x (vector "a" (replace-regexp-in-string ".*_" "" (replace-regexp-in-string "_[0-9]+$" "" x)) x)))

;; (defun em--show-processes ()
;;   (let ((buf (get-buffer-create "*erl-processes*"))
;; 		(entries (mapcar 'em--proc-name-to-tuple em--processes)))
;; 	(with-current-buffer buf
;; 	  (erase-buffer)
;; 	  (setq tabulated-list-format [("PID" 10 t)
;; 								   ("Class" 10 t)
;; 								   ("Name" 0 t)])
;; 	  (setq tabulated-list-padding 2)
;; 	  (setq tabulated-list-sort-key (cons "Name" nil))
;; 	  (tabulated-list-init-header)
;; 	  (setq tabulated-list-entries entries)
;; 	  (tabulated-list-print t))))

(defun em--get-list-from-capture (text)
  "String the leading and trailing crud from a captured list."
;;;;; TODO **** this needs tweaked to make sure it works with all output
  (let* ((no-newlines text);; (replace-regexp-in-string "\n" "" em--process-text))
		 (strip-front (replace-regexp-in-string "^rp(lists:map(fun.*\n\\[" "[" no-newlines)))
	(replace-regexp-in-string "\\]\nok\n.*busy_port.*\n.*$" "]" strip-front)))

(defun em--capture-process-text (process output)
  (setq em--process-text (concat em--process-text output))
  ;; when we have complete string, restore the comint-output-filter
  (when (string-match "ok\n" em--process-text)
	(set-process-filter (get-buffer-process (current-buffer)) em--old-proc-filter)
	(setq em--processes (em--get-list-from-capture em--process-text))))

(defvar em--get-processes-command
  "rp(lists:map(fun(X) -> {X, whereis(X), erlang:process_info(whereis(X)), application:get_application(whereis(X))} end, registered())).")

(defun em--get-processes ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
	(setq em--old-proc-filter (process-filter proc))
	(setq em--process-text "")
	(set-process-filter proc 'em--capture-process-text)
	(process-send-string proc (concat em--get-processes-command "\n"))))

;; when things get hairy:
;; (set-process-filter (get-buffer-process (current-buffer)) em--old-proc-filter)
;; (set-process-filter (get-buffer-process (current-buffer)) 'comin-output-filter)

;; rp(lists:map(fun(X) -> {X, whereis(X), erlang:process_info(whereis(X)), application:get_application(whereis(X))} end, registered())).
;; rp(lists:map(fun(X) -> {X, erlang:port_info(X)} end, erlang:ports())).

;; Application stuff:
;; application:which_applications().
;; rp(application:get_all_key(jrk1)).
;; rp(application:get_all_env(jrk1)).

(defun em--skip-whitespace (text)
  (let ((c (car text)))
	(while (and
			c
			(or
			 (= c ?\ )
			 (= c ?\n)))
	  (setq text (cdr text))
	  (setq c (car text)))
	text))

(defun em--parse-comma-separated (text tree ending-char type-symbol)
  "Parse a comma-separated list of structures/values such as LIST or TUPLE."
  (if (= (car text) ending-char)
	  (list (em--skip-whitespace (cdr text)) (cons `(,type-symbol nil) tree))
	(let* ((el-ret (em--parse-erl-structure text nil))
		   (el (car (cadr el-ret)))
		   (parsed (cons el nil))
		   (rest (em--skip-whitespace (car el-ret)))
		   (c (car rest)))
	  (while (and c (= c ?,))
		(setq el-ret (em--parse-erl-structure (em--skip-whitespace (cdr rest)) nil))
		(setq parsed (cons (car (cadr el-ret)) parsed))
		(setq rest (em--skip-whitespace (car el-ret)))
		(setq c (car rest)))
	  (when (or (not c) (not (= c ending-char)))
		(message "C %c" c)
		(error "Comma-separated sequence not terminated at '%s'" (concat rest)))
	  (list (cdr rest) (cons `(,type-symbol . ,(reverse parsed)) tree)))))

(defun em--parse-pid (text tree)
  (let* ((textstr (concat text))
		 (match-pos (string-match "^<\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)>" textstr)))
	(unless match-pos (error "No PID found at '%s'" (concat text)))
	(let ((rest (nthcdr (match-end 0) text))
		  (part1 (string-to-number (match-string 1 textstr)))
		  (part2 (string-to-number (match-string 2 textstr)))
		  (part3 (string-to-number (match-string 3 textstr))))
	  (list rest (cons `(pid . ,(list part1 part2 part3)) tree)))))

(defun em--parse-port (text tree)
  (let* ((textstr (concat text))
		 (match-pos (string-match "^#Port<\\([0-9]+\\)\\.\\([0-9]+\\)>" textstr)))
	(unless match-pos (error "No port found at '%s'" (concat text)))
	(let ((rest (nthcdr (match-end 0) text))
		  (part1 (string-to-number (match-string 1 textstr)))
		  (part2 (string-to-number (match-string 2 textstr))))
	  (list rest (cons `(port . ,(list part1 part2)) tree)))))

(defun em--parse-ref (text tree)
  (let* ((textstr (concat text))
		 (match-pos (string-match "^#Ref<\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)>" textstr)))
	(unless match-pos (error "No port found at '%s'" (concat text)))
	(let ((rest (nthcdr (match-end 0) text))
		  (part1 (string-to-number (match-string 1 textstr)))
		  (part2 (string-to-number (match-string 2 textstr)))
		  (part3 (string-to-number (match-string 3 textstr)))
		  (part4 (string-to-number (match-string 4 textstr))))
	  (list rest (cons `(ref . ,(list part1 part2 part3 part4)) tree)))))

(defun em--parse-numeric-lit (text tree)
  (let* ((c (car text))
		 (rest text)
		 (str nil))
	(while (and c
				(or
				 (and (>= c ?0) (<= c ?9))
				 (= ?. c)))
	  (setq str (cons c str))
	  (setq rest (cdr rest))
	  (setq c (car rest)))
	(if (= 0 (length str))
		(error "Failed to parse number from '%s'" (concat text)))
	(list rest (cons `(number . ,(string-to-number (concat (reverse str)))) tree))))

(defun em--parse-string-lit (text tree)
  (let* ((c (cadr text))
		 (rest (cdr text))
		 (str nil))
	(while (and
			c
			(or (not (= c ?\"))
				(and (= c ?\\)
					 (cadr rest)
					 (= (cadr rest) ?\"))))
	  (if (and (= c ?\\)
			   (cadr rest)
			   (= (cadr rest) ?\"))
		  ;; escaped "
		  (progn
			(setq str (cons ?\" str))
			(setq rest (cddr rest)))
		(progn
		  (setq str (cons c str))
		  (setq rest (cdr rest))))
	  (setq c (car rest)))
	(list (cdr rest) (cons `(string . ,(concat (reverse str))) tree))))

(defun em--parse-atom (text tree)
  (setq-local c (car text))
  (setq-local rest text)
  (setq-local atom nil)
  (while (and
		  c
		  (or
		   (and (>= c ?a) (<= c ?z))
		   (and (>= c ?A) (<= c ?Z))
		   (and (>= c ?0) (<= c ?9))
		   (= c ?_)))
	(setq-local atom (cons c atom))
	(setq-local rest (cdr rest))
	(setq-local c (car rest)))
  (if (= 0 (length atom))
	  (error "Failed to parse atom from '%s'" (concat text)))
  (list rest (cons `(atom . ,(concat (reverse atom))) tree)))

(defun em--parse-quoted-atom (text tree)
  (let* ((rest (cdr text))
		 (c (car rest))
		 (str nil))
	(while (and c
				(not (= c ?')))
	  (setq str (cons c str))
	  (setq rest (cdr rest))
	  (setq c (car rest)))
	(unless (= c ?')
	  (error "Failed to parse quoted atom from '%s'" (concat text)))
	(list (cdr rest) (cons `(atom . ,(concat (reverse str))) tree))))

;; pass text as (string-to-list text)
(defun em--parse-erl-structure (text tree)
  (let ((c (car text)))
	(cond
	 ((or (= ?\  c) (= ?\n c))
	  (em--parse-erl-structure (em--skip-whitespace (cdr text)) tree))
	 ((= ?\[ c)
	  (em--parse-comma-separated (em--skip-whitespace (cdr text)) tree ?\] 'list))
	 ((= ?\{ c)
	  (em--parse-comma-separated (em--skip-whitespace (cdr text)) tree ?} 'tuple))
	 ((= ?# c)
	  (cond
	   ((= ?P (cadr text))
		(em--parse-port text tree))
	   ((= ?R (cadr text))
		(em--parse-ref text tree))
	   (t
		(error "Invalid"))))
	 ((= ?\" c)
	  (em--parse-string-lit text tree))
	 ;; TODO check <<x>> binary syntax here if necessary
	 ((= ?< c)
	  (em--parse-pid text tree))
	 ((and (<= ?0 c) (>= ?9 c))
	  (em--parse-numeric-lit text tree))
	 ((= ?' c)
	  (em--parse-quoted-atom text tree))
	 ((and (>= c ?a) (<= c ?z))
	  (em--parse-atom text tree))
	 (t
	  (error "Cannot parse at '%s'" (concat text))))))

;; TODO need to remove \n from the string so regex work properly, else it's a crapshoot
