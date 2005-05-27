
(module status mzscheme
  (require (lib "file.ss")
	   (lib "list.ss")
	   (lib "date.ss")
	   (lib "unitsig.ss")
	   (lib "servlet-sig.ss" "web-server")
	   (lib "md5.ss" "handin-server"))

  (provide status-servlet)
  
  (define TOPICS-PER-PAGE 25)

  (define handin-dir (current-directory))
  (define active-dir (build-path handin-dir "active"))
  (define inactive-dir (build-path handin-dir "inactive"))

  (define master-password 
    (with-handlers ([exn:fail? (lambda (x) #f)])
      (cadr (assq 'master-password 
		  (with-input-from-file (build-path handin-dir "config.ss")
		    read)))))
  
  (define (clean-str s)
    (regexp-replace
     " *$" 
     (regexp-replace "^ *" s "")
     ""))

  (define (update-status status tag val)
    (let loop ([status status])
      (cond
       [(null? status) (list (cons tag val))]
       [(eq? (caar status) tag) (cons (cons tag val) (cdr status))]
       [else (cons (car status) (loop (cdr status)))])))

  (define (get-status status tag default)
    (let ([a (assq tag status)])
      (if a
	  (cdr a)
	  default)))

  (define (make-page title . body)
    `(html (head (title ,title))
	   (body ([bgcolor "white"])
		 (p ((align "center"))
		    ,title)
		 ,@body)))

  (define status-servlet
    (unit/sig ()
      (import servlet^)

      (define (make-k k tag)
	(format "~a~atag=~a" k 
		(if (regexp-match #rx"^[^#]*[?]" k)
		    "&"
		    "?")
		tag))
      (define (select-k request)
	(let ([a (assq 'tag (request-bindings request))])
	  (and a (cdr a))))
      (define (link-tag k tag label)
	`(a ((href ,(make-k k tag)))
	    ,label))

      (define (handin-link k user hi)
	(let* ([dir (build-path (if (directory-exists? (build-path "active" hi))
				    "active"
				    "inactive")
				hi
				user)]
	       [l (with-handlers ([exn:fail? (lambda (x) null)])
                    (parameterize ([current-directory dir])
                      (filter (lambda (f)
                                (and (file-exists? f) (not (equal? f "grade"))))
                              (directory-list))))])
	  (if (pair? l)
	      (cdr
	       (apply
		append
		(map
		 (lambda (i) `((br) ,i))
		 (map (lambda (f)
			 (let ([hi (build-path dir f)])
			   `(font
			     ()
			     (a ((href ,(make-k k hi))) ,f)
			     " ("
			     ,(date->string
			       (seconds->date
				(file-or-directory-modify-seconds hi))
			       #t)
			     ")")))
		      l))))
	      (list (format "No handins accepted so far for user ~s, assignment ~s" user hi)))))

      (define (solution-link k hi)
	(let* ([soln-dir (build-path (if (directory-exists? (build-path "active" hi))
					 "active"
					 "inactive")
				     hi
				     "solution")]
	       [soln (let ([f (build-path soln-dir (format "~asol.scm" hi))])
		       (if (or (file-exists? f)
			       (not (directory-exists? soln-dir)))
			   f
			   (let ([l (directory-list soln-dir)])
			     (if (= 1 (length l))
				 (build-path soln-dir (car l))
				 f))))])
	  (if (file-exists? soln)
	      `((a ((href ,(make-k k soln)))
		   "Solution"))
	      `((i "Solution not available")))))

      (define (handin-grade user hi)
	(let* ([dir (build-path (if (directory-exists? (build-path "active" hi))
				    "active"
				    "inactive")
				hi
				user)]
	       [grade (let ([filename (build-path dir "grade")])
			(and (file-exists? filename)
			     (with-input-from-file filename
			       (lambda () (read-string (file-size filename))))))])
	  (if grade
	      grade
	      "no grade so far")))

      (define (one-status-page status for-handin)
	(let ([user (get-status status 'user (lambda () "???"))])
	  (let ([next
		 (send/suspend
		  (lambda (k)
		    (make-page
		     (format "User: ~a, Handin: ~a" user for-handin)
		     `(p ,@(handin-link k user for-handin))
		     `(p "Grade: " ,(handin-grade user for-handin))
		     `(p ,@(solution-link k for-handin))
		     `(p (a ((href ,(make-k k "allofthem")))
			    ,(format "All handins for ~a" user))))))])
	    (let ([tag (select-k next)])
	      (if (string=? tag "allofthem")
		  (all-status-page status)
		  (download status tag))))))

      (define re:base "^([a-zA-Z]*)([0-9]+)")

      (define (all-status-page status)
	(let ([l (quicksort
		  (append (directory-list "active")
			  (with-handlers ([exn:fail? (lambda (x) null)])
			    (directory-list "inactive")))
		  (lambda (a b)
		    (let ([am (regexp-match re:base a)]
			  [bm (regexp-match re:base b)])
		      (if (and am bm
			       (string=? (cadr am) (cadr bm)))
			  (< (string->number (caddr am)) (string->number (caddr bm)))
			  (string<? a b)))))]
	      [user (get-status status 'user (lambda () "???"))])
	  (let ([next
		 (send/suspend
		  (lambda (k)
		    (make-page
		     (format "All Handins for ~a" user)
		     `(table
		       ((bgcolor "#ddddff"))
		       ,@(map (lambda (hi)
				`(tr (td ((bgcolor "white")) ,hi)
				     (td ((bgcolor "white")) ,@(handin-link k user hi))
				     (td ((bgcolor "white")) ,(handin-grade user hi))
				     (td ((bgcolor "white")) ,@(solution-link k hi))))
			      l)))))])
	    (let ([tag (select-k next)])
	      (download status tag)))))

      (define (download status tag)
	;; Make sure the user is allowed to read the requested file:
	(with-handlers ([exn:fail?
                         (lambda (exn)
                           (make-page "Error" "Illegal file access"))])
	  (let ([who (get-status status 'user (lambda () "???"))])
	    (let-values ([(base name dir?) (split-path tag)])
	      ;; Any file name is ok...
	      (unless (string? name) (error "bad"))
	      (let-values ([(base name dir?) (split-path base)])
		;; Directory must be user or "solution"
		(unless (or (string=? name who)
			    (string=? name "solution"))
		  (error "bad"))
		;; Any dir name is ok...
		(let-values ([(base name dir?) (split-path base)])
		  (unless (string? name) (error "bad"))
		  ;; Base must be active or inactive
		  (let-values ([(base name dir?) (split-path base)])
		    (unless (or (string=? name "active") 
				(string=? name "inactive"))
		      (error "bad"))
		    ;; No more to path
		    (unless (eq? base 'relative)
		      (error "bad")))))))
	  ;; Return the downloaded file
	  (let ([data (with-input-from-file tag
			(lambda ()
			  (read-string (file-size tag))))])
	    (make-response/full 200 "Okay"
				(current-seconds)
				"application/data"
				`((Content-length . ,(string-length data))
				  (Content-disposition 
				   .
				   ,(format "attachment; filename=~s"
					    (let-values ([(base name dir?) (split-path tag)])
					      name))))
				(list data)))))

      (define (status-page status for-handin)
	(if for-handin
	    (one-status-page status for-handin)
	    (all-status-page status)))

      (define (login-page status for-handin errmsg)
	(let ([request
	       (send/suspend
		(lambda (k)
		  (make-page
		   "Handin Status Login"
		   `(form ([action ,k] [method "post"])
			  (table
			   ((align "center"))
			   (tr (td ((colspan "2") (align "center"))
				   (font ((color "red"))
					 ,(if errmsg
					      errmsg
					      'nbsp))))
			   (tr (td "Username")
			       (td (input ([type "text"] [name "user"] [size "20"] [value ""]))))
			   (tr (td nbsp))
			   (tr (td "Password")
			       (td (input ([type "password"] [name "passwd"] [size "20"] [value ""]))))
			   (td ((colspan "2") (align "center"))
			       (input ([type "submit"] [name "post"] [value "Login"]))))))))])
	  (let ([user (clean-str (cdr (assq 'user (request-bindings request))))]
		[passwd (cdr (assq 'passwd (request-bindings request)))])
	    (let ([user-data (get-preference (string->symbol user)
					     (lambda () #f)
					     #f
					     "users.ss")])
	      (cond
	       [(and user-data
		     (string? passwd)
		     (let ([pw (md5 passwd)])
		       (or (equal? pw (car user-data))
			   (equal? pw master-password))))
		(status-page (update-status status 'user user) for-handin)]
	       [else
		(login-page status for-handin "Bad username or password")])))))

      (let ([a (assq 'handin (request-bindings initial-request))])
	(login-page null (and a (cdr a)) #f))

      )))

(require status)
status-servlet

