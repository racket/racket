#lang racket/base
(module+ test
  (require rackunit))
(require web-server/http
         web-server/servlet-env
         racket/file
         xml
         racket/function
         racket/runtime-path
         web-server/dispatch
         pkg/util
         (prefix-in pkg: pkg/lib)
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
         meta/pkg-index/basic/main
         web-server/http/id-cookie
         file/sha1
         (prefix-in bcrypt- bcrypt)
         version/utils)

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
(define users.new-path (build-path root "users.new"))
(make-directory* users.new-path)

(github-client_id (file->string (build-path root "client_id")))
(github-client_secret (file->string (build-path root "client_secret")))

(define pkgs-path (build-path root "pkgs"))
(make-directory* pkgs-path)

(define id-cookie-name "pnrid")

;; XXX Add a caching system
(define (package-list)
  (sort (map path->string (directory-list pkgs-path))
        string-ci<=?))
(define (package-exists? pkg-name)
  (file-exists? (build-path pkgs-path pkg-name)))
(define (package-remove! pkg-name)
  (delete-file (build-path pkgs-path pkg-name)))
(define (package-info pkg-name #:version [version #f])
  (define no-version (hash-set (file->value (build-path pkgs-path pkg-name)) 'name pkg-name))
  (cond
    [(and version
          (hash-ref no-version 'versions #f)
          (hash-ref (hash-ref no-version 'versions) version #f))
     =>
     (λ (version-ht)
       (hash-merge version-ht no-version))]
    [else
     no-version]))
(define (package-info-set! pkg-name i)
  (write-to-file i (build-path pkgs-path pkg-name)
                 #:exists 'replace))

(define (hash-merge from to)
  (for/fold ([to to])
      ([(k v) (in-hash from)])
    (hash-set to k v)))

(define (package-ref pkg-info key)
  (hash-ref pkg-info key
            (λ ()
              (match key
                [(or 'author 'checksum 'source)
                 (error 'pkg "Package ~e is missing a required field: ~e"
                        (hash-ref pkg-info 'name) key)]
                ['ring
                 *default-ring*]
                ['checksum-error
                 #f]
                ['tags
                 empty]
                ['versions
                 (hash)]
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
   [("curate") page/curate]
   [("curate" "edit" (string-arg) (number-arg)) page/curate/edit]
   [("rss") page/rss]
   [("api" "upload") #:method "post" api/upload]
   [else basic-start]))

(define (api/upload req)
  (define req-data (read (open-input-bytes (or (request-post-data/raw req) #""))))
  (match-define (list email given-password pis) req-data)
  (define password-path (build-path users.new-path email))
  (define expected-password (file->bytes password-path))
  (cond
    [(not (and (bcrypt-check expected-password given-password)
               (curation-administrator? email)))
     (response/sexpr #f)]
    [else
     (for ([(p more-pi) (in-hash pis)])
       (define pi (if (package-exists? p)
                    (package-info p)
                    #hash()))
       (define new-pi (hash-deep-merge pi more-pi))
       (define updated-pi (let ([now (current-seconds)])
                            (for/fold ([pi new-pi]) ([k (in-list '(last-edit last-checked last-updated))])
                              (hash-set pi k now))))
       (package-info-set! p updated-pi)
       (thread (λ () (update-checksum #t p))))
     (response/sexpr #t)]))

(define (hash-deep-merge ht more-ht)
  (for/fold ([ht ht])
      ([(k new-v) (in-hash more-ht)])
    (hash-update ht k
                 (λ (old-v)
                   (cond
                     [(not old-v)
                      new-v]
                     [(hash? old-v)
                      (hash-deep-merge old-v new-v)]
                     [else
                      new-v]))
                 #f)))

(define (author->list as)
  (string-split as))

(define (page/rss req)
  (define ps
    (sort (map package-info (package-list))
          >
          #:key (λ (i) (package-ref i 'last-updated))))
  (define top (package-ref (first ps) 'last-updated))
  (define (atom-format-time t)
    (format "~aZ" (format-time t)))
  (response/xexpr
   #:mime-type #"application/atom+xml"
   `(feed
     ([xmlns "http://www.w3.org/2005/Atom"])
     (title ,(cdata #f #f (format "<![CDATA[~a]]>"
                                  "Racket Package Updates")))
     (link ([href "https://pkg.racket-lang.org/rss"]
            [rel "self"]))
     (link ([href "https://pkg.racket-lang.org/"]))
     (updated ,(atom-format-time top))
     (id "https://pkg.racket-lang.org/")
     ,@(for/list ([i (in-list ps)])
         (define p (hash-ref i 'name))
         (define this-url
           (format "https://pkg.racket-lang.org~a"
                   (main-url page/info p)))
         (define lu (atom-format-time (package-ref i 'last-updated)))
         (define a (first (author->list (package-ref i 'author))))
         (match-define (regexp #rx"^([^@]+)" (list _ n)) a)
         `(entry
           (title ([type "html"])
                  ,(cdata #f #f (format "<![CDATA[~a]]>" p)))
           (link ([href ,this-url]))
           (updated ,lu)
           (author (name ,n) (email ,a))
           (id ,this-url)
           (content
            ([type "html"])
            ,(cdata #f #f
                    (format "<![CDATA[~a]]>"
                            (xexpr->string
                             `(p
                               ,(format "~a package updated on ~a."
                                        p lu)))))))))))

(define (page/main req)
  (redirect-to (main-url page/search (list "!main-distribution" "!main-tests"))))

(define (format-time s)
  (if s
    (with-handlers ([exn:fail? (λ (x) "")])
      (parameterize ([date-display-format 'iso-8601])
        (date->string (seconds->date s #f) #t)))
    ""))

(define (package-url->useful-url pkg-url-str)
  (define pkg-url
    (string->url pkg-url-str))
  (match (url-scheme pkg-url)
    ["github"
     (match (url-path pkg-url)
       [(list* user repo branch path)
        (url->string
         (struct-copy url pkg-url
                      [scheme "http"]
                      [path (list* user repo (path/param "tree" empty) branch path)]))]
       [_
        pkg-url-str])]
    [_
     pkg-url-str]))

(define (page/info req pkg-name)
  (page/info-like
   (list (cons "Packages" (main-url page/main))
         pkg-name)
   #f
   #f
   #f
   req pkg-name))

(define (search-term-eval pkg-name info term)
  (match term
    [(regexp #rx"^ring:(.*?)$"
             (list _ (app string->number (and (not #f) ring))))
     (equal? ring (package-ref info 'ring))]
    [(regexp #rx"^author:(.*?)$" (list _ author))
     (member author (author->list (package-ref info 'author)))]
    [":no-tag:"
     (empty? (package-ref info 'tags))]
    [":error:"
     (hash-ref info 'checksum-error #f)]
    [(regexp #rx"^!(.*?)$" (list _ sub))
     (not (search-term-eval pkg-name info sub))]
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
                            ,@(if (curation-administrator? user)
                                `(" | "
                                  (a ([href ,(main-url page/curate)])
                                     "curate"))
                                empty)
                            " | "
                            (a ([href ,(main-url page/rss)]) "rss")

                            " | "
                            (a ([href ,(main-url page/login)]) "re-login")

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
   (list (cookie->header (logout-id-cookie id-cookie-name #:path "/")))))

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

(define (package-tags pkgs terms)
  (define tag->count (make-hash))
  (for* ([p (in-list pkgs)]
         [t (in-list (package-ref (package-info p) 'tags))])
    (hash-update! tag->count t add1 0))
  (define tags
    (sort (hash-keys tag->count)
          >
          #:key (λ (t) (hash-ref tag->count t))))
  `(p ,@(for/list ([t (in-list tags)])
          `(span (a ([href ,(main-url page/search (snoc terms t))])
                    ,t)
                 " "))
      (a ([href ,(main-url page/search (snoc terms ":no-tag:"))])
         ":no-tag:")
      (a ([href ,(main-url page/search (snoc terms ":error:"))])
         ":error:")))

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
                           "Contribute a Package")))
               ,(if (current-user req #f)
                  ""
                  `(span ([class "menu_option"])
                         (a ([href ,(main-url page/login)])
                            "Login")))))
   (package-tags pkgs terms)
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
              (p "Passwords are stored in the delicious bcrypt format, but transfered as plain-text over the HTTPS connection.")
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
       (make-id-cookie id-cookie-name secret-key email #:path "/")))))

  (when (regexp-match (regexp-quote "/") email)
    (send/back
     (template
      log-req
      #:breadcrumb
      (list "Login" "Account Registration Error")
      `(p "Email addresses may not contain / on the Racket PNR:"
          (tt ,email)))))

  (when (string=? "" email)
    (send/back
     (template
      log-req
      #:breadcrumb
      (list "Login" "Account Registration Error")
      `(p "Email addresses must not be empty::"
          (tt ,email)))))

  (define password-path (build-path users.new-path email))
  (define old-password-path (build-path users-path email))

  (cond
    [(and (not (file-exists? password-path))
          (file-exists? old-password-path))
     (cond
       [(not (bytes=? (file->bytes old-password-path)
                      (string->bytes/utf-8 (salty passwd))))
        (login req (format "The given password is incorrect for email address ~e"
                           email))]
       [else
        (display-to-file (bcrypt-encode (string->bytes/utf-8 passwd))
                         password-path)
        (delete-file old-password-path)
        (authenticated!)])]
    [(not (file-exists? password-path))
     (send/suspend
      (λ (k-url)
        (send-mail-message
         "pkg@racket-lang.org"
         "Account confirmation for Racket PNR"
         (list email)
         empty empty
         (list "Someone tried to register your email address for an account on the Racket PNR. If you want to authorize this registration and log in, please click the following link:"
               ""
               (format "https://pkg.racket-lang.org~a" k-url)
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
       (display-to-file (bcrypt-encode (string->bytes/utf-8 passwd))
                        password-path))

     (authenticated!)]
    [(not (bcrypt-check (file->bytes password-path)
                        (string->bytes/utf-8 passwd)))
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
     (tr (th "Package") (th "Authors") (th "Description") (th "Tags")))
    (tbody
     ,@(for/list ([p (in-list pkgs)])
         (define i (package-info p))
         (define authors (package-ref i 'author))
         `(tr
           ([class ,(if (< (- (current-seconds) (* 2 24 60 60))
                           (package-ref i 'last-updated))
                      "recent"
                      "")])
           (td (a ([href ,(main-url page/package p)])
                  ,p))
           (td ,@(author-links authors terms))
           (td ,(package-ref i 'description))
           (td ,@(for/list ([t (in-list (package-ref i 'tags))])
                   `(span (a ([href ,(main-url page/search (snoc terms t))])
                             ,t)
                          " "))))))))

(define (author-links authors terms)
  (for/list ([author (in-list (author->list authors))])
    `(span
      (a ([href ,(main-url page/search
                           (snoc terms
                                 (format "author:~a" author)))])
         ,author)
      nbsp)))

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

(define (page/manage/edit req pkg)
  (define (edit-details pkg-req)
    (define new-pkg (request-binding/string pkg-req "name"))
    (when (string=? new-pkg "")
      (error 'pnr "Name must not be empty: ~e" new-pkg))
    (define new-source (request-binding/string pkg-req "source"))
    (when (string=? new-source "")
      (error 'pnr "Source must not be empty: ~e" new-source))
    (define new-desc (request-binding/string pkg-req "description"))

    (unless (valid-name? new-pkg)
      (error 'pnr
             "Illegal character in name; only alphanumerics, plus '-' and '_' allowed: ~e"
             new-pkg))

    (when (and (not (equal? pkg new-pkg))
               (or (regexp-match #rx"^[Pp][Ll][Tt]" new-pkg)
                   (regexp-match #rx"^[Pp][Ll][Aa][Nn][Ee][Tt]" new-pkg)
                   (regexp-match #rx"^[Rr][Aa][Cc][Kk][Ee][Tt]" new-pkg)))
      (error 'pnr
             "Packages that start with plt, planet, and racket are not allowed without special permission. Please create your package with a different name, then email curation to request a rename: ~e"
             new-pkg))

    (when (and (package-exists? new-pkg)
               (not (member (current-user pkg-req #t)
                            (author->list (package-ref (package-info new-pkg) 'author)))))
      (error 'pnr
             "Packages may only be modified by their authors: ~e"
             new-pkg))

    (define new-author (request-binding/string pkg-req "author"))
    (when (string=? new-author "")
      (error 'pnr "Author must not be empty: ~e" new-author))
    (unless (member (current-user pkg-req #t)
                    (author->list new-author))
      (error 'pnr
             "You(~e) must remain an author of the package: ~e"
             (current-user pkg-req #t)
             new-author))

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
       (hash-set i 'author new-author))
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

    (thread (λ () (update-checksum #t new-pkg)))

    (define new-tag
      (request-binding/string pkg-req "tag" #f))
    (add-tag! new-pkg new-tag)

    (redirect-to
     (main-url page/manage/edit new-pkg)))

  (define (delete! pkg-req)
    (when (and (package-exists? pkg)
               (not (member (current-user pkg-req #t)
                            (author->list (package-ref (package-info pkg) 'author)))))
      (error 'pnr
             "Packages may only be modified by their authors: ~e"
             pkg))

    (when pkg
      (package-remove! pkg))

    (redirect-to
     (main-url page/manage)))

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
   delete!
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

(define (valid-name? t)
  (not (regexp-match #rx"[^a-zA-Z0-9_\\-]" t)))

(module+ test
  (check-equal? (valid-name? "net") #t)
  (check-equal? (valid-name? "120") #t)
  (check-equal? (valid-name? "Web") #t)
  (check-equal? (valid-name? "foo-bar") #t)
  (check-equal? (valid-name? "!meh") #f)
  (check-equal? (valid-name? "foo_bar") #t))

(define (add-tag! pkg-name new-tag)
  (when (and new-tag
             (not (string=? new-tag "")))
    (define i (package-info pkg-name))
    (unless (valid-name? new-tag)
      (error 'pnr
             "Illegal character in tag; only alphanumerics allowed, plus '_' and '-': ~e"
             new-tag))
    (package-info-set!
     pkg-name
     (hash-update i 'tags
                  (λ (old)
                    (tags-normalize
                     (cons new-tag
                           old)))
                  empty))))

(define ((pkg-info-edit-version bc pkg-name v) req)
  (define version-formlet
    (if v
      (formlet
       (table
        (tr (td "Version:")
            (td  ,v))
        (tr (td "Source:")
            (td ,{(to-string (required (text-input))) . => . source})))
       (values v source))
      (formlet
       (table
        (tr (td "Version:")
            (td  ,{(to-string (required (text-input))) . => . version}))
        (tr (td "Source:")
            (td ,{(to-string (required (text-input))) . => . source})))
       (values version source))))

  (define version-req
    (send/suspend
     (λ (k-url)
       (template
        req
        #:breadcrumb
        bc
        `(div([class "package"])
             (form ([action ,k-url] [method "post"])
                   ,@(formlet-display version-formlet)
                   (input ([type "submit"] [value "Edit Version Exception"]))))))))

  (define-values (version source)
    (formlet-process version-formlet version-req))

  (unless (valid-version? version)
    (error 'pnr "Must use valid version for exception: ~e"
           version))
  (when (and (package-exists? pkg-name)
             (not (member (current-user version-req #t)
                          (author->list (package-ref (package-info pkg-name) 'author)))))
    (error 'pnr
           "Packages may only be modified by their authors: ~e"
           pkg-name))

  (package-info-set!
   pkg-name
   (hash-update (package-info pkg-name) 'versions
                (λ (v-ht)
                  (hash-set v-ht version
                            (hasheq 'source source
                                    'checksum "")))
                hash))

  (thread (λ () (update-checksum #t pkg-name)))

  (redirect-to (main-url page/manage/edit pkg-name)))

(define (page/info-like bc edit-details tag-url delete-handler req pkg-name)
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
     (define authors (package-ref* i 'author (or (current-user req #f) "")))
     (define the-table
       `(table
         (tr
          (td (a ([href "http://docs.racket-lang.org/pkg/Package_Concepts.html#%28tech._package._name%29"]) "Package Name"))
          (td ,(if edit-details
                 `(input ([name "name"]
                          [type "text"]
                          [value ,(or pkg-name "")]))
                 pkg-name)))
         (tr
          (td "Ring")
          (td ,(ring-format (package-ref* i 'ring *default-ring*))))
         (tr
          (td "Authors")
          (td ,@(if edit-details
                  `((input ([name "author"]
                            [type "text"]
                            [value ,(or authors "")]))
                    (br)
                    (span "Use spaces to separator each author's email address."))
                  (author-links authors empty))))
         (tr
          (td "Source")
          (td
           ,(if edit-details
              `(span (input ([name "source"]
                             [type "text"]
                             [value ,(package-ref* i 'source "")]))
                     " (" (a ([href "http://docs.racket-lang.org/pkg/Package_Concepts.html#%28tech._package._source%29"]) "details") ")")
              `(a ([href
                    ,(package-url->useful-url (package-ref i 'source))])
                  ,(package-ref i 'source)))))
         (tr
          (td "Checksum")
          (td ,(package-ref* i 'checksum "")))
         ,@(if (package-ref* i 'checksum-error #f)
             `((tr (td "Error")
                   (td "The last time we attempted to update this checksum. The following error was thrown:"
                       (pre ,(package-ref* i 'checksum-error "")))))
             empty)
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
                `(li
                  (a ([href ,(main-url page/search (list t))])
                     ,t)
                  ,@(if tag-url
                      (list " ["
                            `(a ([href ,(tag-url embed/url t)])
                                "delete tag")
                            "]")
                      empty)))
            ,(if pkg-name
               `(li (input ([name "tag"] [type "text"])))
               ""))))
         (tr
          (td "Version Exceptions")
          (td
           (table
            ,@(for/list ([(v vi) (in-hash (package-ref* i 'versions (hash)))])
                `(tr
                  (td ,(if edit-details
                         `(a ([href ,(embed/url (pkg-info-edit-version bc pkg-name v))])
                             ,v)
                         v))
                  (td ,(hash-ref vi 'source ""))
                  (td ,(hash-ref vi 'checksum ""))))
            ,@(if edit-details
                `((tr
                   (td ([colspan "3"] [style "text-align: center;"])
                       (a ([href ,(embed/url (pkg-info-edit-version bc pkg-name #f))])
                          "Add exception"))))
                '()))))
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
        ,(if pkg-name
           `(div ([class "install"])
                 "Install this package with:" (br) (br)
                 (tt "raco pkg install " ,pkg-name) (br) (br)
                 "or, with the 'File|Install Package...' menu option in DrRacket.")
           "")
        ,(if (and pkg-name delete-handler)
           `(div ([class "delete"])
                 (a ([href ,(embed/url delete-handler)])
                    "Delete this package")
                 " (Warning: There is no undo.)")
           ""))))))

(define (page/manage/update req)
  (thread
   (λ ()
     (update-checksums
      #t
      (package-list/mine req))))
  (redirect-to (main-url page/manage)))

(define (update-checksums force? pkgs)
  (for-each (curry update-checksum force?) pkgs))

(define (update-checksum force? pkg-name)
  (with-handlers
      ([exn:fail?
        (λ (x)
          (define i (package-info pkg-name))
          (package-info-set! 
           pkg-name 
           (hash-set i 'checksum-error (exn-message x))))])
    (define i (package-info pkg-name))
    (define old-checksum
      (package-ref i 'checksum))
    (define now (current-seconds))
    (define last (hash-ref i 'last-checked -inf.0))
    (when (or force?
              (>= (- now last) (* 24 60 60)))
      (printf "\tupdating ~a\n" pkg-name)
      (define new-checksum
        (package-url->checksum
         (package-ref i 'source)))
      (package-begin
       (define* i
         (hash-set i 'checksum
                   (or new-checksum
                       old-checksum)))
       (define* i
         (hash-set i 'last-checked now))
       (define* i
         (hash-update i 'versions
                      (λ (v-ht)
                        (for/hash ([(v vi) (in-hash v-ht)])
                          (define old-checksum (hash-ref vi 'checksum ""))
                          (define new-checksum
                            (package-url->checksum
                             (hash-ref vi 'source)))
                          (values v
                                  (hash-set vi 'checksum
                                            (or new-checksum
                                                old-checksum)))))
                      hash))
       (define* i
         (if (and new-checksum (equal? new-checksum old-checksum)
                  ;; update if 'modules was not present:
                  (hash-ref i 'modules #f))
           i
           (hash-set (update-from-content i) 'last-updated now)))
       (define* i
         (hash-set i 'checksum-error #f))
       (package-info-set! pkg-name i)))))

(define (update-from-content i)
  (define-values (checksum module-paths dependencies)
    (pkg:get-pkg-content (pkg:pkg-desc (hash-ref i 'source)
                                       #f
                                       #f
                                       (hash-ref i 'checksum)
                                       #f)))
  (package-begin
   (define* i (hash-set i 'modules module-paths))
   (define* i (hash-set i 'dependencies dependencies))
   i))

(define basic-start
  (pkg-index/basic+versions package-list package-info))

;; Curation
(define (curation-administrator? u)
  (member u '("jay.mccarthy@gmail.com" "mflatt@cs.utah.edu")))

(define *default-ring* 2)
(define (ring-format i)
  (format "~a" i))

(define (page/curate/edit req pkg dir)
  (define u (current-user req #t))
  (when (curation-administrator? u)
    (define i (package-info pkg))
    (package-info-set!
     pkg
     (hash-set i 'ring (+ dir (package-ref i 'ring)))))
  (redirect-to (main-url page/curate)))

(define (module-lists-conflict? left right)
  (define seen? (make-hash))
  (for ([l (in-list left)])
    (hash-set! seen? l #t))
  (for/or ([r (in-list right)])
    (hash-ref seen? r #f)))

(define (string-min x y)
  (if (string<=? x y)
    x
    y))

(define (string-max x y)
  (if (string<? x y)
    y
    x))

(define (page/curate req)
  (define u (current-user req #t))
  (define (packages-conflict? left right)
    (define left-i (package-info left))
    (define right-i (package-info right))
    (define left-m (and left-i (hash-ref left-i 'modules #f)))
    (define right-m (and right-i (hash-ref right-i 'modules #f)))
    (if (and left-m right-m)
      (module-lists-conflict? left-m right-m)
      ;; We have to say #t here because otherwise things with no
      ;; information won't be conflicting.
      #t))
  (define conflict-cache
    (make-hash))
  (define (packages-conflict?/cache left right)
    (define smin (string-min left right))
    (define smax (string-max left right))
    (hash-ref! conflict-cache
               (cons smin smax)
               (λ ()
                 (packages-conflict? smin smax))))
  (define (ring i)
    (package-list/search (list (format "ring:~a" i))))
  (define ring-01
    (append (ring 0) (ring 1)))
  (define (package-conflicts? pkg)
    (filter (λ (other-pkg)
              (if (equal? pkg other-pkg)
                #f
                (packages-conflict?/cache pkg other-pkg)))
            ring-01))
  (cond
    [(curation-administrator? u)
     (template
      req
      #:breadcrumb
      (list "Packages"
            "Curation")
      ;; XXX maybe I should change these so that it
      ;;  1. displays a distinct link to change it to each ring
      ;;  2. displays the conflicts (if any)
      ;;  3. displays the update time
      ;;  4. doesn't display other stuff
      ;;  5. conflicts
      ;;  6. proposals (ring 2 with no conflicts)
      ;; Then I think I will just need one table
      `(table
        ([class "packages sortable"])
        (thead
         (tr (th "Ring") (th "Package") (th "Authors") (th "Last Update") (th "Conflicts")))
        (tbody
         ,@(for/list ([p (in-list (package-list))])
             (define i (package-info p))
             (define authors (package-ref i 'author))
             (define r (package-ref i 'ring))
             (define conflicts (package-conflicts? p))
             (define lu (package-ref i 'last-updated))
             `(tr
               ([class
                    ,(cond
                       [(and (= r 2) (empty? conflicts))
                        "proposal"]
                       [(and (< r 2) (cons? conflicts))
                        "problem"]
                       [else ""])])
               (td ,(if (< 0 r)
                      `(a ([href ,(main-url page/curate/edit p -1)])
                          blacktriangledown)
                      `blacktriangledown)
                   ,(number->string r)
                   ,(if (< r 2)
                      `(a ([href ,(main-url page/curate/edit p +1)])
                          blacktriangle)
                      `blacktriangle))
               (td ,p)
               (td ,authors)
               (td ([sorttable_customkey ,(number->string lu)])
                   ,(format-time lu))
               (td
                ,@(for/list ([c (in-list conflicts)])
                    `(span ,c " "))))))))]
    [else
     (template
      req
      #:breadcrumb
      (list "Curation")
      `(p ([class "error"]) "You are not authorized to curate."))]))

;; Start
(define (go port)
  (printf "launching on port ~a\n" port)
  (thread
   (λ ()
     (while true
       (printf "updating checksums\n")
       (let loop ([pkg*ts
                   (for/list ([pkg (in-list (package-list))])
                     (cons pkg (thread (λ () (update-checksum #f pkg)))))]
                  [the-alarm
                   (alarm-evt (+ (current-inexact-milliseconds)
                                 (* 1000 (* 24 60 60))))])
         (define (tprintf fmt arg)
           (printf "[~a] ~a: ~a"
                   (date->string (seconds->date (current-seconds)) #t)
                   (length pkg*ts)
                   (format fmt arg)))
         (apply
          sync
          (handle-evt the-alarm
                      (λ _
                        (for ([pkg*t (in-list pkg*ts)])
                          (match-define (cons pkg t) pkg*t)
                          (when (thread-running? t)
                            (tprintf "~a checksum thread stalled\n" pkg)
                            (kill-thread t)))))
          (for/list ([pkg*t (in-list pkg*ts)])
            (match-define (cons pkg t) pkg*t)
            (handle-evt t
                        (λ _
                          (tprintf "~a thread finished\n" pkg)
                          (loop (remove pkg*t pkg*ts) the-alarm)))))))))
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
