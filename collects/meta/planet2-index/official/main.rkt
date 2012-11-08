#lang racket/base
(require web-server/http
         web-server/servlet-env
         racket/file
         racket/function
         racket/runtime-path
         web-server/dispatch
         planet2/util
         racket/match
         racket/package
         racket/system
         racket/date
         racket/string
         web-server/servlet
         web-server/formlets
         racket/bool
         racket/list
         net/sendmail
         meta/planet2-index/basic/main
         web-server/http/id-cookie
         file/sha1)

(define-syntax-rule (while cond e ...)
  (let loop ()
    (when cond
      e ...
      (loop))))

(define (snoc l x)
  (append l (list x)))

(define (salty str)
  (sha1 (open-input-string str)))

(define-runtime-path src ".")

(define-runtime-path root "root")
(make-directory* root)
(define secret-key
  (make-secret-salt/file
   (build-path root "secret.key")))
(define users-path (build-path root "users"))
(make-directory* users-path)

(module+ main
  (define users-old-path (build-path root "users.old"))
  (when (directory-exists? users-old-path)
    (for ([u (in-list (directory-list users-old-path))])
      (define uop (build-path users-old-path u))
      (display-to-file (salty (file->string uop))
                       (build-path users-path u))
      (delete-file uop))
    (delete-directory users-old-path)))

(define pkgs-path (build-path root "pkgs"))
(make-directory* pkgs-path)

(define id-cookie-name "id")

;; XXX Add a caching system
(define (package-list)
  (sort (map path->string (directory-list pkgs-path))
        string-ci<=?))
(define (package-exists? pkg-name)
  (file-exists? (build-path pkgs-path pkg-name)))
(define (package-remove! pkg-name)
  (delete-file (build-path pkgs-path pkg-name)))
(define (package-info pkg-name)
  (file->value (build-path pkgs-path pkg-name)))
(define (package-info-set! pkg-name i)
  (write-to-file i (build-path pkgs-path pkg-name)
                 #:exists 'replace))

(define (package-ref pkg-info key)
  (hash-ref pkg-info key
            (λ ()
              (match key
                [(or 'author 'checksum 'source)
                 (error 'planet2 "Package ~e is missing a required field: ~e"
                        (hash-ref pkg-info 'name) key)]
                ['tags
                 empty]
                [(or 'last-checked 'last-edit 'last-updated)
                 -inf.0]))))

(define-values (main-dispatch main-url)
  (dispatch-rules
   [() page/main]
   [("") page/main]
   [("info" (string-arg)) page/info]
   [("search" (string-arg) ...) page/search]
   [("query" "search" (string-arg) ...) page/search/query]
   [("account" "login") page/login]
   [("account" "logout") page/logout]
   [("manage") page/manage]
   [("manage" "update") page/manage/update]
   [("manage" "edit" (string-arg)) page/manage/edit]
   [("manage" "upload") page/manage/upload]
   [else basic-start]))

(define (page/main req)
  (redirect-to (main-url page/search empty)))

(define (format-time s)
  (if s
    (parameterize ([date-display-format 'rfc2822])
      (date->string (seconds->date s #f) #t))
    ""))

(define (package-url->useful-url pkg-url-str)
  (define pkg-url
    (string->url pkg-url-str))
  (match (url-scheme pkg-url)
    ["github"
     (match-define (list* user repo branch path)
                   (url-path pkg-url))
     (url->string
      (struct-copy url pkg-url
                   [scheme "http"]
                   [path (list* user repo (path/param "tree" empty) branch path)]))]
    [_
     pkg-url-str]))

(define (page/info req pkg-name)
  (page/info-like
   (list (cons "Packages" (main-url page/main))
         pkg-name)
   #f
   (λ (embed/url t)
     (main-url page/search (list t)))
   req pkg-name))

(define (search-term-eval pkg-name info term)
  (match term
    [(regexp #rx"^author:(.*?)$" (list _ author))
     (equal? author (package-ref info 'author))]
    [_
     (define term-rx (regexp-quote term))
     (for/or ([tag (list* pkg-name (package-ref info 'tags))])
       (regexp-match? term-rx tag))]))

(define breadcrumb->string
  (match-lambda
   [(? string? label)
    label]
   [(cons (? string? label)
          (? string? url))
    label]))
(define breadcrumb->xexpr
  (match-lambda
   [(? string? label)
    `(span ,label)]
   [(cons (? string? label)
          (? string? url))
    `(span (a ([href ,url]) ,label))]))

(define (template req #:breadcrumb bc . xexpr-forest)
  (send/back
   (response/xexpr
    `(html
      (head
       (script ([src "/sorttable.js"]) " ")
       (link ([rel "stylesheet"]
              [type "text/css"]
              [href "/style.css"]))
       (title ,@(add-between (map breadcrumb->string bc) " > ")))
      (body
       (div ([class "breadcrumb"])
            ,@(add-between (map breadcrumb->xexpr bc) " > ")
            ,(cond
               [(current-user req #f)
                => (λ (user)
                     `(span ([id "logout"])
                            ,user
                            " | "
                            (a ([href ,(main-url page/logout)]) "logout")))]
               [else
                ""]))
       ,@xexpr-forest
       (div ([id "footer"])
            "Powered by "
            (a ([href "http://racket-lang.org/"]) "Racket") ". "
            "Written by "
            (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy")
            "."))))))

(define (page/logout req)
  (redirect-to
   (main-url page/main)
   #:headers
   (list (cookie->header (logout-id-cookie id-cookie-name)))))

(define (package-list/search ts)
  (filter
   (λ (p)
     (define i (package-info p))
     (for/and ([t (in-list ts)])
       (search-term-eval p i t)))
   (package-list)))

(define search-formlet
  (formlet
   ,{(to-string (required (text-input)))
     . => . new-terms}
   (string-split new-terms)))

(define (page/search/query req old-terms)
  (define terms (formlet-process search-formlet req))
  (redirect-to (main-url page/search (append old-terms terms))))

(define (page/search req terms)
  (define pkgs (package-list/search terms))
  (template
   req
   #:breadcrumb
   (list* (cons "Packages" (main-url page/main))
          "Search"
          (for/list ([t (in-list terms)])
            (cons t (main-url page/search (remove* (list t) terms)))))
   `(div ([id "menu"])
         (form ([action ,(main-url page/search/query terms)])
               (span ([class "menu_option"])
                     ,@(formlet-display search-formlet)
                     (input ([type "submit"] [value "Search"])))
               (span ([class "menu_option"])
                     (a ([href ,(main-url page/manage)])
                        ,(if (current-user req #f)
                           "Manage Your Packages"
                           "Contribute a Package")))))
   (package-table page/info pkgs #:terms terms)))

(define (page/login req)
  (login req)
  (redirect-to (main-url page/main)))

(define (login req [last-error #f])
  (define login-formlet
    (formlet
     (table
      (tr (td "Email Address:")
          (td  ,{(to-string (required (text-input))) . => . email}))
      (tr (td "Password:")
          (td ,{(to-string (required (password-input))) . => . passwd})))
     (values email passwd)))
  (define log-req
    (send/suspend
     (λ (k-url)
       (template
        req
        #:breadcrumb
        (list "Login")
        `(div ([id "login"])
              (form ([action ,k-url] [method "post"])
                    ,@(formlet-display login-formlet)
                    (input ([type "submit"] [value "Log in"])))
              (p "If you enter an unclaimed email address, then an account will be created.")
              (p "Passwords are stored in the delicious SHA1 format, but transfered as plain-text over the HTTPS connection.")
              ,@(if last-error
                  `((h1 ([class "error"]) ,last-error))
                  '()))))))
  (define-values
    (email passwd)
    (formlet-process login-formlet log-req))

  (define (authenticated!)
    (redirect/get
     #:headers
     (list
      (cookie->header
       (make-id-cookie id-cookie-name secret-key email)))))

  (when (regexp-match (regexp-quote "/") email)
    (send/back
     (template
      log-req
      #:breadcrumb
      (list "Login" "Account Registration Error")
      `(p "Email addresses may not contain / on Planet2:"
          (tt ,email)))))

  (when (string=? "" email)
    (send/back
     (template
      log-req
      #:breadcrumb
      (list "Login" "Account Registration Error")
      `(p "Email addresses must not be empty::"
          (tt ,email)))))

  (define password-path (build-path users-path email))

  (cond
    [(not (file-exists? password-path))
     (send/suspend
      (λ (k-url)
        (send-mail-message
         "planet2@racket-lang.org"
         "Account confirmation for Planet2"
         (list email)
         empty empty
         (list "Someone tried to register your email address for an account on Planet2. If you want to authorize this registration and log in, please click the following link:"
               ""
               (format "https://plt-etc.byu.edu:9004~a" k-url)
               ""
               "This link will expire, so if it is not available, you'll have to try to register again."))
        (template
         log-req
         #:breadcrumb
         (list "Login" "Account Registration")
         `(p "An email has been sent to "
             (tt ,email)
             ", please click the link it contains to register and log in."))))

     (when (not (file-exists? password-path))
       (display-to-file (salty passwd) password-path))

     (authenticated!)]
    [(not (bytes=? (string->bytes/utf-8 (salty passwd))
                   (file->bytes password-path)))
     (login req (format "The given password is incorrect for email address ~e"
                        email))]
    [else
     (authenticated!)]))

(define (current-user req required?)
  (define id
    (request-id-cookie id-cookie-name secret-key req))
  (cond
    [id
     id]
    [required?
     (current-user (login req) required?)]
    [else
     #f]))

(define (package-list/mine req)
  (define u (current-user req #t))
  (package-list/search (list (format "author:~a" u))))

(define (package-table page/package pkgs
                       #:terms [terms empty])
  `(table
    ([class "packages sortable"])
    (thead
     (tr (th "Package") (th "Author") (th "Description") (th "Tags")))
    (tbody
     ,@(for/list ([p (in-list pkgs)])
         (define i (package-info p))
         (define author (package-ref i 'author))
         `(tr
           ([class ,(if (< (- (current-seconds) (* 2 24 60 60))
                           (package-ref i 'last-updated))
                      "recent"
                      "")])
           (td (a ([href ,(main-url page/package p)])
                  ,p))
           (td (a ([href ,(main-url page/search
                                    (snoc terms
                                          (format "author:~a" author)))])
                  ,author))
           (td ,(package-ref i 'description))
           (td ,@(for/list ([t (in-list (package-ref i 'tags))])
                   `(span (a ([href ,(main-url page/search (snoc terms t))])
                             ,t)
                          " "))))))))

(define (page/manage req)
  (define pkgs (package-list/mine req))
  (template
   req
   #:breadcrumb
   (list (cons "Packages" (main-url page/main))
         (current-user req #t)
         "Manage")
   `(div ([id "menu"])
         (span ([class "menu_option"])
               (a ([href ,(main-url page/manage/upload)])
                  "Upload a new package"))
         (span ([class "menu_option"])
               (a ([href ,(main-url page/manage/update)])
                  "Update checksums")))
   (package-table page/manage/edit pkgs)))

(define (page/manage/upload req)
  (page/manage/edit req #f))

(define (request-binding/string req id [fail? #t])
  (define res
    (bindings-assq (string->bytes/utf-8 id)
                   (request-bindings/raw req)))
  (cond
    [res
     (bytes->string/utf-8
      (binding:form-value
       res))]
    [fail?
     (error 'planet2 "Missing field ~e" id)]
    [else
     #f]))

(define (page/manage/edit req pkg)
  (define (edit-details pkg-req)
    (define new-pkg (request-binding/string pkg-req "name"))
    (when (string=? new-pkg "")
      (error 'planet2 "Name must not be empty: ~e" new-pkg))
    (define new-source (request-binding/string pkg-req "source"))
    (when (string=? new-source "")
      (error 'planet2 "Source must not be empty: ~e" new-source))
    (define new-desc (request-binding/string pkg-req "description"))

    (when (regexp-match #rx"[^a-zA-Z0-9_\\-]" new-pkg)
      (error 'planet2
             "Illegal character in name; only alphanumerics, plus '-' and '_' allowed: ~e"
             new-pkg))

    (when (and (not (equal? pkg new-pkg))
               (or (regexp-match #rx"^[Pp][Ll][Tt]" new-pkg)
                   (regexp-match #rx"^[Pp][Ll][Aa][Nn][Ee][Tt]" new-pkg)
                   (regexp-match #rx"^[Rr][Aa][Cc][Kk][Ee][Tt]" new-pkg)))
      (error 'planet2
             "Packages that start with plt, planet, and racket are not allowed without special permission. Please create your package with a different name, then email curation to request a rename: ~e"
             new-pkg))

    (when (and (package-exists? new-pkg)
               (not (equal? (package-ref (package-info new-pkg) 'author)
                            (current-user pkg-req #t))))
      (error 'planet2
             "Packages may only be modified by their authors: ~e"
             new-pkg))

    (package-begin
     (define* i
       (if pkg
         (package-info pkg)
         (hasheq)))

     (define* i
       (hash-set i 'name new-pkg))
     (define* i
       (hash-set i 'source new-source))
     (define* i
       (hash-set i 'author (current-user pkg-req #t)))
     (define* i
       (hash-set i 'description new-desc))
     (define* i
       (hash-set i 'last-edit (current-seconds)))
     (define* i
       (if pkg
         i
         (hash-set i 'checksum "")))

     (package-info-set! new-pkg i))

    (unless (or (not pkg) (equal? new-pkg pkg))
      (package-remove! pkg))

    (update-checksum new-pkg)

    (define new-tag
      (request-binding/string pkg-req "tag" #f))
    (add-tag! new-pkg new-tag)

    (redirect-to
     (main-url page/manage/edit new-pkg)))

  (page/info-like
   (list* (cons "Packages" (main-url page/main))
          (current-user req #t)
          (cons "Manage" (main-url page/manage))
          (if pkg
            (list pkg
                  "Edit")
            (list "Upload")))
   edit-details
   (λ (embed/url t)
     (embed/url (remove-tag-handler pkg t)))
   req pkg))


(define (tags-normalize ts)
  (remove-duplicates (sort ts string-ci<?)))

(define ((remove-tag-handler pkg t) req)
  (define i (package-info pkg))
  (package-info-set!
   pkg
   (hash-update i 'tags
                (λ (old)
                  (tags-normalize
                   (remove t
                           old)))
                empty))
  (redirect-to
   (main-url page/manage/edit pkg)))

(define ((add-tag-handler pkg-name) req)
  (define new-tag
    (request-binding/string req "tag" #f))
  (add-tag! pkg-name new-tag)
  (redirect-to (main-url page/info pkg-name)))

(define (add-tag! pkg-name new-tag)
  (when (and new-tag
             (not (string=? new-tag "")))
    (define i (package-info pkg-name))
    (when (regexp-match #rx"[^a-zA-Z0-9]" new-tag)
      (error 'planet2
             "Illegal character in tag; only alphanumerics allowed: ~e"
             new-tag))
    (package-info-set!
     pkg-name
     (hash-update i 'tags
                  (λ (old)
                    (tags-normalize
                     (cons new-tag
                           old)))
                  empty))))

(define (page/info-like bc edit-details tag-url req pkg-name)
  (define form-handler
    (or edit-details
        (add-tag-handler pkg-name)))

  (send/suspend/dispatch
   (λ (embed/url)
     (define i (and pkg-name (package-info pkg-name)))
     (define (package-ref* i id def)
       (if i
         (package-ref i id)
         def))
     (define author (package-ref* i 'author ""))
     (define the-table
       `(table
         (tr
          (td "Name")
          (td ,(if edit-details
                 `(input ([name "name"]
                          [type "text"]
                          [value ,(or pkg-name "")]))
                 pkg-name)))
         (tr
          (td "Author")
          (td (a ([href ,(main-url page/search
                                   (list (format "author:~a" author)))])
                 ,author)))
         (tr
          (td "Source")
          (td
           ,(if edit-details
              `(span (input ([name "source"]
                             [type "text"]
                             [value ,(package-ref* i 'source "")]))
                     " (" (a ([href "XXX"]) "details") ")")
              `(a ([href
                    ,(package-url->useful-url (package-ref i 'source))])
                  ,(package-ref i 'source)))))
         (tr
          (td "Checksum")
          (td ,(package-ref* i 'checksum "")))
         (tr
          (td "Last Update")
          (td ,(format-time (package-ref* i 'last-updated #f))))
         (tr
          (td "Last Checked")
          (td ,(format-time (package-ref* i 'last-checked #f))))
         (tr
          (td "Last Edit")
          (td ,(format-time (package-ref* i 'last-edit #f))))
         (tr
          (td "Description")
          (td ,(if edit-details
                 `(textarea ([name "description"])
                            ,(package-ref* i 'description ""))
                 (package-ref i 'description))))
         (tr
          (td "Tags")
          (td
           (ul
            ,@(for/list ([t (in-list (package-ref* i 'tags empty))])
                `(li (a ([href ,(tag-url embed/url t)])
                        ,t)))
            ,(if pkg-name
               `(li (input ([name "tag"] [type "text"])))
               ""))))
         `(tr (td ([class "submit"] [colspan "2"])
                  (input ([type "submit"] [value "Submit"]))))))
     (template
      req
      #:breadcrumb
      bc
      `(div
        ([class "package"])
        (form ([action ,(embed/url form-handler)] [method "post"])
              ,the-table)
        (div ([class "install"])
             "Install this package with:" (br) (br)
             (tt "raco pkg install " ,pkg-name) (br) (br)
             "or, by evaluating:" (br)
             (pre
              ,(format "~a\n~a\n~a\n"
                       "#lang racket"
                       "(require planet2)"
                       (format "(install \"~a\")"
                               pkg-name)))))))))

(define (page/manage/update req)
  (update-checksums
   (package-list/mine req))
  (redirect-to (main-url page/manage)))

(define (update-checksums pkgs)
  (for-each update-checksum pkgs))

(define (update-checksum pkg-name)
  (define i (package-info pkg-name))
  (define old-checksum
    (package-ref i 'checksum))
  (define now (current-seconds))
  (define new-checksum
    (package-url->checksum (package-ref i 'source)))
  (package-begin
   (define* i
     (hash-set i 'checksum
               (or new-checksum
                   old-checksum)))
   (define* i
     (hash-set i 'last-checked now))
   (define* i
     (if (and new-checksum (equal? new-checksum old-checksum))
       i
       (hash-set i 'last-updated now)))
   (package-info-set! pkg-name i)))

(define basic-start
  (planet2-index/basic package-list package-info))

(define (go port)
  (printf "launching on port ~a\n" port)
  (thread
   (λ ()
     (while true
       (printf "updating checksums\n")
       (update-checksums (package-list))
       ;; update once per day based on whenever the server started
       (sleep (* 24 60 60)))))
  (serve/servlet
   main-dispatch
   #:command-line? #t
   #:listen-ip #f
   #:ssl? #t
   #:ssl-cert (build-path root "server-cert.pem")
   #:ssl-key (build-path root "private-key.pem")
   #:extra-files-paths
   (list (build-path src "static")
         (build-path root "static"))
   #:servlet-regexp #rx""
   #:port port))

(module+ main
  (go 9004))
