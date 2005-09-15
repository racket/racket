
(module status mzscheme
  (require (lib "file.ss")
	   (lib "list.ss")
	   (lib "string.ss")
	   (lib "date.ss")
	   (lib "unitsig.ss")
	   (lib "servlet-sig.ss" "web-server")
	   (lib "response.ss" "web-server")
	   (lib "md5.ss" "handin-server")
	   (lib "uri-codec.ss" "net"))

  (provide status-servlet)

  (define handin-dir   (or (getenv "HANDIN_SERVER_DIR") (current-directory)))
  (define active-dir   (build-path handin-dir "active"))
  (define inactive-dir (build-path handin-dir "inactive"))
  (define active/inactive-dirs (list active-dir inactive-dir))

  (define master-password
    (with-handlers ([exn:fail? (lambda (x) #f)])
      (cadr (assq 'master-password
		  (with-input-from-file (build-path handin-dir "config.ss")
		    read)))))

  (define get-user-data
    (let ([users-file (build-path handin-dir "users.ss")])
      (lambda (user)
        (get-preference (string->symbol user) (lambda () #f) #f users-file))))

  (define (clean-str s)
    (regexp-replace #rx" *$" (regexp-replace #rx"^ *" s "") ""))

  (define (update-status status tag val)
    (let loop ([status status])
      (cond
       [(null? status) (list (cons tag val))]
       [(eq? (caar status) tag) (cons (cons tag val) (cdr status))]
       [else (cons (car status) (loop (cdr status)))])))

  (define (get-status status tag default)
    (let ([a (assq tag status)])
      (if a (cdr a) default)))

  (define (make-page title . body)
    `(html (head (title ,title))
	   (body ([bgcolor "white"]) (p ((align "center")) ,title) ,@body)))

  (define status-servlet
    (unit/sig ()
      (import servlet^)

      (define handin-prefix-re
        ;; a regexp that turns a full path to a handin-dir relative path
        (regexp
         (string-append
          "^" (regexp-quote
               (regexp-replace
                #rx"/?$"
                (if (path? handin-dir) (path->string handin-dir) handin-dir)
                "/")))))
      (define (make-k k tag)
	(format "~a~atag=~a" k (if (regexp-match #rx"^[^#]*[?]" k) "&" "?")
                (uri-encode (regexp-replace
                             handin-prefix-re
                             (if (path? tag) (path->string tag) tag)
                             ""))))
      (define (select-k request)
	(let ([a (assq 'tag (request-bindings request))])
	  (and a (cdr a))))

      ;; `look-for' can be a username as a string (will find "bar+foo" for
      ;; "foo"), or a regexp that should match the whole directory name (used
      ;; with "^solution" below)
      (define (find-hi-entry hi look-for)
        (define (find-submission top)
          (let ([dir (build-path top hi)])
            (and (directory-exists? dir)
                 (ormap
                  (lambda (d)
                    (let ([d (path->string d)])
                      (and (cond [(string? look-for)
                                  (member look-for
                                          (regexp-split #rx" *[+] *" d))]
                                 [(regexp? look-for) (regexp-match look-for d)]
                                 [else (error 'find-hi-entry
                                              "internal error: ~e" look-for)])
                           (build-path dir d))))
                  (directory-list dir)))))
        (ormap find-submission active/inactive-dirs))

      (define (handin-link k user hi)
	(let* ([dir (find-hi-entry hi user)]
	       [l (and dir (with-handlers ([exn:fail? (lambda (x) null)])
                             (parameterize ([current-directory dir])
                               (quicksort
                                (filter (lambda (f)
                                          (and (not (equal? f "grade"))
                                               (file-exists? f)))
                                        (map path->string (directory-list)))
                                string<?))))])
	  (if (pair? l)
	      (cdr
	       (apply
		append
		(map
		 (lambda (i) `((br) ,i))
		 (map (lambda (f)
			 (let ([hi (build-path dir f)])
			   `(font ()
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
	(let ([soln (find-hi-entry hi #rx"^solution")]
              [none `((i "---"))])
          (cond [(not soln) none]
                [(file-exists? soln)
                 `((a ((href ,(make-k k soln))) "Solution"))]
                [(directory-exists? soln)
                 (parameterize ([current-directory soln])
                   (let ([files (mergesort (map path->string
                                                (filter file-exists?
                                                        (directory-list)))
                                           string<?)])
                     (if (null? files)
                       none
                       (apply append
                              (map (lambda (f)
                                     `((a ((href ,(make-k k (build-path soln f))))
                                          (tt ,f))
                                       (br)))
                                   files)))))]
                [else none])))

      (define (handin-grade user hi)
	(let* ([dir (find-hi-entry hi user)]
	       [grade (and dir
                           (let ([filename (build-path dir "grade")])
                             (and (file-exists? filename)
                                  (with-input-from-file filename
                                    (lambda ()
                                      (read-string (file-size filename)))))))])
	  (or grade "--")))

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

      (define re:base #rx"^([a-zA-Z]*)([0-9]+)")

      (define (all-status-page status)
	(let ([l (quicksort
		  (map path->string
		       (append (directory-list active-dir)
			       (with-handlers ([exn:fail? (lambda (x) null)])
				 (directory-list inactive-dir))))
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
                    (define (header text)
                      `(td ((bgcolor "#f0f0f0")) (big (strong ,text))))
		    (make-page
		     (format "All Handins for ~a" user)
		     `(table ([bgcolor "#ddddff"] [cellpadding "6"] [align "center"])
                       (tr () ,@(map header '(nbsp "Files" "Grade" "Solution")))
		       ,@(map (lambda (hi)
				`(tr ([valign "top"])
                                   ,(header hi)
				   (td ([bgcolor "white"]) ,@(handin-link k user hi))
				   (td ([bgcolor "white"] (align "right")) ,(handin-grade user hi))
				   (td ([bgcolor "white"]) ,@(solution-link k hi))))
			      l)))))])
	    (let ([tag (select-k next)])
	      (download status tag)))))

      (define (download status tag)
        (define (check path elts)
          (let loop ([path path] [elts (reverse elts)])
            (let*-values ([(base name dir?) (split-path path)]
                          [(name) (path->string name)]
                          [(check) (and (pair? elts) (car elts))])
              (if (null? elts)
                ;; must be rooted in active/inactive (why build-path instead of
                ;; using `path'? -- because path will have a trailing slash)
                (member (build-path base name) active/inactive-dirs)
                (and (cond [(eq? '* check) #t]
                           [(regexp? check) (regexp-match check name)]
                           [(string? check)
                            (or (equal? name check)
                                (member check
                                        (regexp-split #rx" *[+] *" name)))]
                           [else #f])
                     (loop base (cdr elts)))))))
        (define file (build-path handin-dir tag))
	(with-handlers ([exn:fail?
                         (lambda (exn)
                           (make-page "Error" "Illegal file access"))])
          (let ([who (get-status status 'user (lambda () "???"))])
	    ;; Make sure the user is allowed to read the requested file:
            (or (check file `(* ,who *))
                (check file `(* #rx"^solution"))
                (check file `(* #rx"^solution" *))
                (error "Boom!")))
	  ;; Return the downloaded file
	  (let* ([data (with-input-from-file file
                         (lambda () (read-bytes (file-size file))))]
                 [html? (regexp-match #rx"[.]html?$" (string-foldcase tag))]
                 [wxme? (regexp-match #rx#"^WXME" data)])
	    (make-response/full 200 "Okay" (current-seconds)
				(cond [html? #"text/html"]
                                      [wxme? #"application/data"]
                                      [else  #"text/plain"])
				`((Content-Length . ,(number->string (bytes-length data)))
				  ,@(if wxme?
                                      `((Content-Disposition
                                         .
                                         ,(format "attachment; filename=~s"
                                                  (let-values ([(base name dir?) (split-path file)])
                                                    (path->string name)))))
                                      '()))
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
	    (let ([user-data (get-user-data user)])
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

