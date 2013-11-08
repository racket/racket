#lang racket/base
(require web-server/http
         "common.rkt"
         "jsonp.rkt"
         web-server/servlet-env
         racket/file
         web-server/dispatch
         racket/match
         racket/string
         net/url
         racket/list
         net/sendmail
         meta/pkg-index/basic/main
         file/sha1
         (prefix-in bcrypt- bcrypt)
         version/utils)

(define (package-remove! pkg-name)
  (delete-file (build-path pkgs-path pkg-name)))

(define (package-exists? pkg-name)
  (file-exists? (build-path pkgs-path pkg-name)))

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

(define (curation-administrator? u)
  (member u '("jay.mccarthy@gmail.com" "mflatt@cs.utah.edu")))

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
       (define updated-pi
         (let ([now (current-seconds)])
           (for/fold ([pi new-pi])
               ([k (in-list '(last-edit last-checked last-updated))])
             (hash-set pi k now))))
       (package-info-set! p updated-pi)
       (signal-update! (list p)))
     (response/sexpr #t)]))

(define (redirect-to-static req)
  (redirect-to
   (url->string
    (struct-copy url (request-uri req)
                 [scheme "http"]
                 [host "pkgs.racket-lang.org"]
                 [port 80]))))

(define-syntax-rule (define-jsonp/auth (f . pat) . body)
  (define-jsonp
    (f
     ['email email]
     ['passwd passwd]
     . pat)
    (ensure-authenticate email passwd (λ () . body))))

(define (salty str)
  (sha1 (open-input-string str)))

(define current-user (make-parameter #f))
(define (ensure-authenticate email passwd body-fun)
  (define passwd-path (build-path users.new-path email))
  (define old-passwd-path (build-path users-path email))

  (define (authenticated!)
    (parameterize ([current-user email])
      (body-fun)))

  (cond
    [(and (not (file-exists? passwd-path))
          (file-exists? old-passwd-path))
     (cond
       [(not (bytes=? (file->bytes old-passwd-path)
                      (string->bytes/utf-8 (salty passwd))))
        "failed"]
       [else
        (display-to-file (bcrypt-encode (string->bytes/utf-8 passwd))
                         passwd-path)
        (delete-file old-passwd-path)
        (authenticated!)])]
    [(not (file-exists? passwd-path))
     "new-user"]
    [(not (bcrypt-check (file->bytes passwd-path)
                        (string->bytes/utf-8 passwd)))
     "failed"]
    [else
     (authenticated!)]))

(define email-codes (make-hash))
(define-jsonp
  (jsonp/authenticate
   ['email email]
   ['passwd passwd]
   ['code email-code])

  (define passwd-path (build-path users.new-path email))
  (define (generate-a-code email)
    (define correct-email-code
      (number->string (random (expt 10 8))))

    (hash-set! email-codes email correct-email-code)

    correct-email-code)
  (define (check-code-or true false)
    (cond
      [(and (not (string=? "" email-code))
            (hash-ref email-codes email #f))
       => (λ (correct-email-code)
            (cond
              [(equal? correct-email-code email-code)
               (display-to-file (bcrypt-encode (string->bytes/utf-8 passwd))
                                passwd-path
                                #:exists 'replace)

               (hash-remove! email-codes email)

               (true)]
              [else
               "wrong-code"]))]
      [else
       (false)

       #f]))

  (match (ensure-authenticate email passwd (λ () #t))
    ["failed"
     (check-code-or
      (λ () (hasheq 'curation (curation-administrator? email)))
      (λ ()
        (send-mail-message
         "pkg@racket-lang.org"
         "Account password reset for Racket Package Catalog"
         (list email)
         empty empty
         (list
          "Someone tried to login with your email address for an account on the Racket Package Catalog, but failed."
          "If you this was you, please use this code to reset your password:"
          ""
          (generate-a-code email)
          ""
          "This code will expire, so if it is not available, you'll have to try to again."))))]
    ["new-user"
     (check-code-or
      (λ () #t)
      (λ ()
        (send-mail-message
         "pkg@racket-lang.org"
         "Account confirmation for Racket Package Catalog"
         (list email)
         empty empty
         (list
          "Someone tried to register your email address for an account on the Racket Package Catalog."
          "If you want to proceed, use this code:"
          ""
          (generate-a-code email)
          ""
          "This code will expire, so if it is not available, you'll have to try to register again."))))]
    [#t
     (hasheq 'curation (curation-administrator? email))]))

(define-jsonp/auth
  (jsonp/package/modify
   ['pkg pkg]
   ['name mn-name]
   ['description mn-desc]
   ['source mn-source])
  (cond
    [(equal? pkg "")
     (cond
       [(package-exists? mn-name)
        #f]
       [else
        (package-info-set! mn-name
                           (hasheq 'name mn-name
                                   'source mn-source
                                   'author (current-user)
                                   'description mn-desc
                                   'last-edit (current-seconds)))
        (signal-update! (list mn-name))
        #t])]
    [else
     (ensure-package-author
      pkg
      (λ ()
        (cond
          [(equal? mn-name pkg)
           (package-info-set! pkg
                              (hash-set* (package-info pkg)
                                         'source mn-source
                                         'description mn-desc
                                         'last-edit (current-seconds)))
           (signal-update! (list pkg))
           #t]
          [(and (valid-name? mn-name)
                (not (package-exists? mn-name)))
           (package-info-set! mn-name
                              (hash-set* (package-info pkg)
                                         'name mn-name
                                         'source mn-source
                                         'description mn-desc
                                         'last-edit (current-seconds)))
           (package-remove! pkg)
           (signal-update! (list mn-name))
           #t]
          [else
           #f])))]))

(define-jsonp/auth
  (jsonp/package/version/add
   ['pkg pkg]
   ['version version]
   ['source source])
  (ensure-package-author
   pkg
   (λ ()
     (cond
       [(valid-version? version)
        (package-info-set!
         pkg
         (hash-update (package-info pkg) 'versions
                      (λ (v-ht)
                        (hash-set v-ht version
                                  (hasheq 'source source
                                          'checksum "")))
                      hash))
        (signal-update! (list pkg))
        #t]
       [else
        #f]))))

(define-jsonp/auth
  (jsonp/package/version/del
   ['pkg pkg]
   ['version version])
  (ensure-package-author
   pkg
   (λ ()
     (cond
       [(valid-version? version)
        (package-info-set!
         pkg
         (hash-update (package-info pkg) 'versions
                      (λ (v-ht)
                        (hash-remove v-ht version))
                      hash))
        (signal-update! (list pkg))
        #t]
       [else
        #f]))))

(define (tags-normalize ts)
  (remove-duplicates (sort ts string-ci<?)))

(define-jsonp/auth
  (jsonp/package/tag/add
   ['pkg pkg]
   ['tag tag])
  (cond
    [(valid-tag? tag)
     (define i (package-info pkg))
     (package-info-set!
      pkg
      (hash-set i 'tags (tags-normalize (cons tag (package-ref i 'tags)))))
     (signal-static! (list pkg))
     #t]
    [else
     #f]))

(define-jsonp/auth
  (jsonp/package/tag/del
   ['pkg pkg]
   ['tag tag])
  (ensure-package-author
   pkg
   (λ ()
     (define i (package-info pkg))
     (package-info-set!
      pkg
      (hash-set i 'tags
                (remove tag
                        (package-ref i 'tags))))
     (signal-static! (list pkg))
     #t)))

(define-jsonp/auth
  (jsonp/package/author/add
   ['pkg pkg]
   ['author author])
  (ensure-package-author
   pkg
   (λ ()
     (cond
       [(valid-author? author)
        (define i (package-info pkg))
        (package-info-set!
         pkg
         (hash-set i 'author (format "~a ~a" (package-ref i 'author) author)))
        (signal-static! (list pkg))
        #t]
       [else
        #f]))))

(define (ensure-package-author pkg f)
  (cond
    [(package-author? pkg (current-user))
     (f)]
    [else
     #f]))

(define-jsonp/auth
  (jsonp/package/author/del
   ['pkg pkg]
   ['author author])
  (ensure-package-author
   pkg
   (λ ()
     (cond
       [(not (equal? (current-user) author))
        (define i (package-info pkg))
        (package-info-set!
         pkg
         (hash-set i 'author
                   (string-join
                    (remove author
                            (author->list (package-ref i 'author))))))
        (signal-static! (list pkg))
        #t]
       [else
        #f]))))

(define-jsonp/auth
  (jsonp/package/del
   ['pkg pkg])
  (ensure-package-author
   pkg
   (λ ()
     (package-remove! pkg)
     (signal-static! empty)
     #f)))

(define-jsonp/auth
  (jsonp/package/curate
   ['pkg pkg]
   ['ring ring-s])
  (cond
    [(curation-administrator? (current-user))
     (define i (package-info pkg))
     (define ring-n (string->number ring-s))
     (package-info-set!
      pkg
      (hash-set i 'ring (min 2 (max 0 ring-n))))
     (signal-static! (list pkg))
     #t]
    [else
     #f]))

(define (package-author? p u)
  (define i (package-info p))
  (member u (author->list (package-ref i 'author))))

(define (packages-of u)
  (filter (λ (p) (package-author? p u)) (package-list)))

(define-jsonp/auth
  (jsonp/update)
  (signal-update! (packages-of (current-user)))
  #t)

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("jsonp" "authenticate") jsonp/authenticate]
   [("jsonp" "update") jsonp/update]
   [("jsonp" "package" "del") jsonp/package/del]
   [("jsonp" "package" "modify") jsonp/package/modify]
   [("jsonp" "package" "version" "add") jsonp/package/version/add]
   [("jsonp" "package" "version" "del") jsonp/package/version/del]
   [("jsonp" "package" "tag" "add") jsonp/package/tag/add]
   [("jsonp" "package" "tag" "del") jsonp/package/tag/del]
   [("jsonp" "package" "author" "add") jsonp/package/author/add]
   [("jsonp" "package" "author" "del") jsonp/package/author/del]
   [("jsonp" "package" "curate") jsonp/package/curate]
   [("api" "upload") #:method "post" api/upload]
   [else redirect-to-static]))

(define-syntax-rule (forever . body)
  (let loop () (begin . body) (loop)))

(define (go port)
  (printf "launching on port ~a\n" port)
  (signal-static! empty)
  (thread
   (λ ()
     (forever
      (sleep (* 1 60 60))
      (printf "Running scheduled update.\n")
      (signal-update! empty))))
  (serve/servlet
   main-dispatch
   #:command-line? #t
   #:listen-ip #f
   #:ssl? #t
   #:ssl-cert (build-path root "server-cert.pem")
   #:ssl-key (build-path root "private-key.pem")
   #:extra-files-paths empty
   #:servlet-regexp #rx""
   #:port port))

(module+ main
  (go 9004))
