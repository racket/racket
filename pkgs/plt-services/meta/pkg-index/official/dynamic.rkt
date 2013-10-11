#lang racket/base
(require web-server/http
         "common.rkt"
         "jsonp.rkt"
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

(define (package-exists? pkg-name)
  (file-exists? (build-path pkgs-path pkg-name)))

(define (hash-merge from to)
  (for/fold ([to to])
      ([(k v) (in-hash from)])
    (hash-set to k v)))

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
       (define updated-pi (let ([now (current-seconds)])
                            (for/fold ([pi new-pi]) ([k (in-list '(last-edit last-checked last-updated))])
                              (hash-set pi k now))))
       (package-info-set! p updated-pi)
       (signal-update! #t p))
     (response/sexpr #t)]))

(define (signal-update! force? pkg)
  ;; XXX
  (void))

(define (redirect-to-static req)
  (redirect-to
   (url->string
    (struct-copy url (request-uri req)
                 [scheme "http"]
                 ;; XXX change these to the real static site
                 [host "localhost"]
                 [port 8001]))))

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
  (match (ensure-authenticate email passwd (λ () #t))
    ["failed" #f]
    ["new-user"
     (define passwd-path (build-path users.new-path email))

     (cond
       [(and (not (string=? "" email-code))
             (hash-ref email-codes email #f))
        => (λ (correct-email-code)
             (cond
               [(equal? correct-email-code email-code)
                (when (not (file-exists? passwd-path))
                  (display-to-file (bcrypt-encode (string->bytes/utf-8 passwd))
                                   passwd-path))

                (hash-remove! email-codes email)

                #t]
               [else
                "wrong-code"]))]
       [else
        (define correct-email-code
          (number->string (random (expt 10 8))))

        (hash-set! email-codes email correct-email-code)

        (send-mail-message
         "pkg@racket-lang.org"
         "Account confirmation for Racket PNR"
         (list email)
         empty empty
         (list "Someone tried to register your email address for an account on the Racket Package Catalog."
               "If you want to proceed, use this code:"
               ""
               correct-email-code
               ""
               "This code will expire, so if it is not available, you'll have to try to register again."))

        "emailed"])]
    [#t
     (hasheq 'curation (curation-administrator? email))]))

;; XXX
(define-jsonp/auth
  (jsonp/package/modify
   ['pkg pkg]
   ['name mn-name]
   ['description mn-desc]
   ['source mn-source])
  #f)

;; XXX
(define-jsonp/auth
  (jsonp/package/version/add
   ['pkg pkg]
   ['version version]
   ['source source])
  #f)

;; XXX
(define-jsonp/auth
  (jsonp/package/version/del
   ['pkg pkg]
   ['version version])
  #f)

;; XXX
(define-jsonp/auth
  (jsonp/package/tag/add
   ['pkg pkg]
   ['tag tag])
  #f)

;; XXX
(define-jsonp/auth
  (jsonp/package/tag/del
   ['pkg pkg]
   ['tag tag])
  #f)

;; XXX
(define-jsonp/auth
  (jsonp/package/author/add
   ['pkg pkg]
   ['author author])
  #f)

;; XXX
(define-jsonp/auth
  (jsonp/package/author/del
   ['pkg pkg]
   ['author author])
  #f)

;; XXX
(define-jsonp/auth
  (jsonp/package/curate
   ['pkg pkg]
   ['ring ring-s])
  #f)

;; XXX
(define-jsonp/auth
  (jsonp/update)
  #f)

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("jsonp" "authenticate") jsonp/authenticate]
   [("jsonp" "update") jsonp/update]
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

(define (go port)
  (printf "launching on port ~a\n" port)
  (serve/servlet
   (λ (req)
     (displayln (url->string (request-uri req)))
     (main-dispatch req))
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
