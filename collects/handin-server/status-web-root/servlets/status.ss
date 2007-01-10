(module status mzscheme
  (require (lib "file.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "date.ss")
           (lib "servlet.ss" "web-server")
           (lib "response-structs.ss" "web-server")
           (lib "uri-codec.ss" "net")
           (lib "md5.ss"    "handin-server" "private")
           (lib "logger.ss" "handin-server" "private")
           (lib "config.ss" "handin-server" "private"))

  (define active-dir   (build-path server-dir "active"))
  (define inactive-dir (build-path server-dir "inactive"))
  (define active/inactive-dirs (list active-dir inactive-dir))

  (define get-user-data
    (let ([users-file (build-path server-dir "users.ss")])
      (lambda (user)
        (get-preference (string->symbol user) (lambda () #f) #f users-file))))

  (define (clean-str s)
    (regexp-replace #rx" *$" (regexp-replace #rx"^ *" s "") ""))

  (define (aget alist key)
    (cond [(assq key alist) => cdr] [else #f]))

  (define (make-page title . body)
    `(html (head (title ,title))
           (body ([bgcolor "white"]) (h1 ((align "center")) ,title) ,@body)))

  (define handin-prefix-re
    ;; a regexp that turns a full path to a server-dir relative path
    (regexp
     (string-append
      "^" (regexp-quote
           (regexp-replace
            #rx"/?$"
            (if (path? server-dir) (path->string server-dir) server-dir)
            "/")))))

  (define (make-k k tag)
    (format "~a~atag=~a" k (if (regexp-match #rx"^[^#]*[?]" k) "&" "?")
            (uri-encode (regexp-replace handin-prefix-re
                                        (if (path? tag) (path->string tag) tag)
                                        ""))))

  (define (select-k request)
    (aget (request-bindings request) 'tag))

  ;; `look-for' can be a username as a string (will find "bar+foo" for "foo"),
  ;; or a regexp that should match the whole directory name (used with
  ;; "^solution" below)
  (define (find-hi-entry hi look-for)
    (define (find-submission top)
      (let ([dir (build-path top hi)])
        (and (directory-exists? dir)
             (ormap
              (lambda (d)
                (let ([d (path->string d)])
                  (and (cond [(string? look-for)
                              (member look-for (regexp-split #rx" *[+] *" d))]
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
                           (sort (filter (lambda (f)
                                           (and (not (equal? f "grade"))
                                                (file-exists? f)))
                                         (map path->string (directory-list)))
                                 string<?))))])
      (if (pair? l)
        (cdr (apply append
                    (map (lambda (i) `((br) ,i))
                         (map (lambda (f)
                                (let ([hi (build-path dir f)])
                                  `(font ()
                                     (a ([href ,(make-k k hi)]) ,f)
                                     " ("
                                     ,(date->string
                                       (seconds->date
                                        (file-or-directory-modify-seconds hi))
                                       #t)
                                     ")")))
                              l))))
        (list (format "No handins accepted so far for user ~s, assignment ~s"
                      user hi)))))

  (define (solution-link k hi)
    (let ([soln (find-hi-entry hi #rx"^solution")]
          [none `((i "---"))])
      (cond [(not soln) none]
            [(file-exists? soln)
             `((a ((href ,(make-k k soln))) "Solution"))]
            [(directory-exists? soln)
             (parameterize ([current-directory soln])
               (let ([files (sort (map path->string
                                       (filter file-exists? (directory-list)))
                                  string<?)])
                 (if (null? files)
                   none
                   (apply append
                          (map (lambda (f)
                                 `((a ([href ,(make-k k (build-path soln f))])
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

  (define (one-status-page user for-handin)
    (let* ([next (send/suspend
                  (lambda (k)
                    (make-page (format "User: ~a, Handin: ~a" user for-handin)
                      `(p ,@(handin-link k user for-handin))
                      `(p "Grade: " ,(handin-grade user for-handin))
                      `(p ,@(solution-link k for-handin))
                      `(p (a ([href ,(make-k k "allofthem")])
                            ,(format "All handins for ~a" user))))))]
           [tag (select-k next)])
      (if (string=? tag "allofthem")
        (all-status-page user)
        (download user tag))))

  (define re:base #rx"^([a-zA-Z]*)([0-9]+)")
  (define (all-status-page user)
    (let* ([l (sort
               (map path->string
                    (append (directory-list active-dir)
                            (with-handlers ([exn:fail? (lambda (x) null)])
                              (directory-list inactive-dir))))
               (lambda (a b)
                 (let ([am (regexp-match re:base a)]
                       [bm (regexp-match re:base b)])
                   (if (and am bm
                            (string=? (cadr am) (cadr bm)))
                     (or (< (string->number (caddr am))
                            (string->number (caddr bm)))
                         (string<? a b))
                     (string<? a b)))))]
           [next
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
                          l)))))]
           [tag (select-k next)])
      (download user tag)))

  (define (download who tag)
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
                            (member check (regexp-split #rx" *[+] *" name)))]
                       [else #f])
                 (loop base (cdr elts)))))))
    (define file (build-path server-dir tag))
    (with-handlers ([exn:fail? (lambda (exn)
                                 (make-page "Error" "Illegal file access"))])
      ;; Make sure the user is allowed to read the requested file:
      (or (check file `(* ,who *))
          (check file `(* #rx"^solution"))
          (check file `(* #rx"^solution" *))
          (error "Boom!"))
      (log-line "Status file-get: ~s ~a" who file)
      ;; Return the downloaded file
      (let* ([data (with-input-from-file file
                     (lambda () (read-bytes (file-size file))))]
             [html? (regexp-match #rx"[.]html?$" (string-foldcase tag))]
             [wxme? (regexp-match #rx#"^WXME" data)])
        (make-response/full 200 "Okay" (current-seconds)
          (cond [html? #"text/html"]
                [wxme? #"application/data"]
                [else  #"text/plain"])
          `([Content-Length . ,(number->string (bytes-length data))]
            [Content-Disposition
             . ,(format "~a; filename=~s"
                        (if wxme? "attachment" "inline")
                        (let-values ([(base name dir?) (split-path file)])
                          (path->string name)))])
          (list data)))))

  (define (status-page user for-handin)
    (log-line "Status access: ~s" user)
    (if for-handin
      (one-status-page user for-handin)
      (all-status-page user)))

  (define (login-page status for-handin errmsg)
    (let* ([request
            (send/suspend
             (lambda (k)
               (make-page
                "Handin Status Login"
                `(form ([action ,k] [method "post"])
                   (table ([align "center"])
                     (tr (td ([colspan "2"] [align "center"])
                           (font ([color "red"]) ,(or errmsg 'nbsp))))
                     (tr (td "Username")
                         (td (input ([type "text"] [name "user"] [size "20"]
                                     [value ""]))))
                     (tr (td nbsp))
                     (tr (td "Password")
                         (td (input ([type "password"] [name "passwd"]
                                     [size "20"] [value ""]))))
                     (tr (td ([colspan "2"] [align "center"])
                           (input ([type "submit"] [name "post"]
                                   [value "Login"])))))))))]
           [user      (clean-str (aget (request-bindings request) 'user))]
           [passwd    (aget (request-bindings request) 'passwd)]
           [user-data (get-user-data user)])
      (cond [(and user-data
                  (string? passwd)
                  (let ([pw (md5 passwd)])
                    (or (equal? pw (car user-data))
                        (equal? pw (get-conf 'master-password)))))
             (status-page user for-handin)]
            [else (login-page status for-handin "Bad username or password")])))

  (define web-counter
    (let ([sema (make-semaphore 1)]
          [count 0])
      (lambda ()
        (dynamic-wind
          (lambda () (semaphore-wait sema))
          (lambda () (set! count (add1 count)) (format "w~a" count))
          (lambda () (semaphore-post sema))))))

  (define (start initial-request)
    (parameterize ([current-session (web-counter)])
      (login-page null (aget (request-bindings initial-request) 'handin) #f)))

  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout 180)
  )
