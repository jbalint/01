;; when things get hairy:
;; (set-process-filter (get-buffer-process (current-buffer)) em--old-proc-filter)
;; (set-process-filter (get-buffer-process (current-buffer)) 'comin-output-filter)

;; rp(lists:map(fun(X) -> {X, whereis(X), erlang:process_info(whereis(X)), application:get_application(whereis(X))} end, registered())).
;; rp(lists:map(fun(X) -> {X, erlang:port_info(X)} end, erlang:ports())).

;; Application stuff:
;; application:which_applications().
;; rp(application:get_all_key(jrk1)).
;; rp(application:get_all_env(jrk1)).

(define-derived-mode em-process-mode tabulated-list-mode "em-mode" "Mode stuff here"
  "Process list mode"
  (setq tabulated-list-format [("PID" 10 t)
							   ("Class" 10 t)
							   ("Application" 20 t)
							   ("Process" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Process" nil))
  (tabulated-list-init-header))

(defun em--proc-tuple-to-vector (x)
  "Convert a process tuple to a vector for tabulated list display."
  ;; should be of the form: (tuple (atom . "sasl_safe_sup") (pid 0 51 0) (tuple (atom . "ok") (atom . "sasl")))
  (let* ((name (cdr (cadr x)))
		 (class (replace-regexp-in-string ".*_" "" (replace-regexp-in-string "_[0-9]+$" "" name)))
		 (pid (car (nthcdr 2 x)))
		 (pid-str (apply 'format "<%s.%s.%s>" (cdr pid)))
		 (app-struct (car (nthcdr 3 x)))
		 (app (cond
			   ((eq 'tuple (car app-struct))
				(cdar (cddr app-struct)))
			   (t
				(cdr app-struct)))))
	(list pid-str (vector pid-str class app name))))

(defun em--display-processes ()
  (let ((buf (get-buffer-create "*erl-processes*"))
		(em--processes-struct (cdar (em--get-val-from-capture em--process-text)))
		(entries (mapcar 'em--proc-tuple-to-vector em--processes-struct)))
	(with-current-buffer buf
	  (erase-buffer)
	  (em-mode)
	  (setq tabulated-list-entries entries)
	  (tabulated-list-print t))
	(display-buffer buf)))

(defun em--get-val-from-capture (text)
  "String the leading and trailing crud from a captured list."
;;;;; TODO **** this needs tweaked to make sure it works with all output
  (let* ((no-newlines text);; (replace-regexp-in-string "\n" "" em--process-text))
		 (strip-front (replace-regexp-in-string "^rp(lists:map(fun.*\n\\[" "[" no-newlines))
		 (finaltext (replace-regexp-in-string "\nok\n.*> $" "" strip-front)))
	(cadr (em--parse-erl-structure (string-to-list finaltext) nil))))

(defun em--capture-process-text (process output)
  (setq em--process-text (concat em--process-text output))
  ;; when we have complete string, restore the comint-output-filter
  (when (string-match "ok\n" em--process-text)
	(set-process-filter (get-buffer-process (current-buffer)) em--old-proc-filter)
	(setq em--processes (em--get-list-from-capture em--process-text))))

(defvar em--get-processes-command
  "rp(lists:map(fun(X) -> {X, whereis(X), application:get_application(whereis(X))} end, registered()))."
  "Command used to retrieve the process list from the shell.")

(defun em--get-processes ()
  "Start the process of retrieving the process list."
  (let ((proc (get-buffer-process (current-buffer))))
	;; save the old filter, probably 'comint-output-filter
	(setq em--old-proc-filter (process-filter proc))
	;; init string to save captured value
	(setq em--process-text "")
	;; set our process filter
	(set-process-filter proc 'em--capture-process-text)
	;; send command
	(process-send-string proc (concat em--get-processes-command "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang data parser ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun em--skip-whitespace (text)
  "Skip whitespace chars while parsing"
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
		(error "Comma-separated sequence not terminated at '%s'" (concat rest)))
	  (list (cdr rest) (cons `(,type-symbol . ,(reverse parsed)) tree)))))

;; TODO note, use of regex in parsing is dangerous because the string
;; may have newlines and Emacs regexp will match ^ after newlines

(defun em--parse-pid (text tree)
  "Parse a pid <1.2.3> as '(pid 1 2 3)."
  (let* ((textstr (concat text))
		 (match-pos (string-match "^<\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)>" textstr)))
	(unless match-pos (error "No PID found at '%s'" (concat text)))
	(let ((rest (nthcdr (match-end 0) text))
		  (part1 (string-to-number (match-string 1 textstr)))
		  (part2 (string-to-number (match-string 2 textstr)))
		  (part3 (string-to-number (match-string 3 textstr))))
	  (list rest (cons `(pid . ,(list part1 part2 part3)) tree)))))

(defun em--parse-port (text tree)
  "Parse a port #Port<1.2> as '(port 1 2)."
  (let* ((textstr (concat text))
		 (match-pos (string-match "^#Port<\\([0-9]+\\)\\.\\([0-9]+\\)>" textstr)))
	(unless match-pos (error "No port found at '%s'" (concat text)))
	(let ((rest (nthcdr (match-end 0) text))
		  (part1 (string-to-number (match-string 1 textstr)))
		  (part2 (string-to-number (match-string 2 textstr))))
	  (list rest (cons `(port . ,(list part1 part2)) tree)))))

(defun em--parse-ref (text tree)
  "Parse a reference #Ref<1.2.3> as '(ref 1 2 3)."
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
  "Parse a numeric literal, e.g. 4 as '(number . 4)."
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
  "Parse a string literal, e.g. \"xyz\" as '(string \"xyz\")."
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
  "Parse an atom, e.g. anAtom as '(atom . \"anAtom\")."
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
  "Parse a quoted atom, e.g. 'an atom' as '(atom . \"an atom\")."
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
  "Parse an Erlang structure such as [1.2, anAtom, \"X\", {<1.2.3>, 'quoted'}}, etc."
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
