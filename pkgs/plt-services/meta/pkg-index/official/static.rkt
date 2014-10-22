#lang racket/base
(require web-server/http
         web-server/dispatch
         racket/file
         racket/port
         racket/system
         racket/match
         json
         racket/date
         net/url
         xml
         racket/list
         racket/path
         racket/promise
         meta/pkg-index/basic/main
         "common.rkt"
         "notify.rkt"
         "s3.rkt")

(define convert-to-json
  (match-lambda
   [(? list? l)
    (map convert-to-json l)]
   [(? string? s)
    s]
   [(? hash? ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (convert-to-json-key k)
              (convert-to-json v)))]
   [(? number? n)
    n]
   [(? boolean? b)
    b]
   [(? symbol? s)
    (symbol->string s)]
   [(? keyword? s)
    (hasheq 'kw (keyword->string s))]
   [x
    (error 'convert-to-json "~e" x)]))
(define convert-to-json-key
  (match-lambda
   [(? string? s)
    (string->symbol s)]
   [(? symbol? s)
    s]
   [x
    (error 'convert-to-json-key "~e" x)]))

(define (file->value* p dv)
  (if (file-exists? p)
    (file->value p)
    dv))

;; From pkg-build/summary
(struct doc/main (name path) #:prefab)
(struct doc/extract (name path) #:prefab)
(struct doc/salvage (name path) #:prefab)
(struct doc/none (name) #:prefab)
(struct conflicts/indirect (path) #:prefab)

(define (generate-static)
  (define pkg-list (package-list))
  (define pkg-ht (make-hash))
  (define build-summary (file->value* SUMMARY-PATH (hash)))

  (for ([pkg-name (in-list pkg-list)])
    (define ht (file->value (build-path pkgs-path pkg-name)))

    (define versions-ht
      (hash-set (hash-ref ht 'versions (hash))
                'default
                (hasheq 'source (hash-ref ht 'source "")
                        'checksum (hash-ref ht 'checksum ""))))

    (define (hash-ref-or ht ks)
      (or (for/or ([k (in-list ks)])
            (hash-ref ht k #f))
          (error 'hash-ref-or "Keys (~v) not found in ~e" ks ht)))

    (define versions-5.3.6
      (hash-ref-or versions-ht '("5.3.6" default)))
    (define source-5.3.6
      (hash-ref versions-5.3.6 'source))
    (define checksum-5.3.6
      (hash-ref versions-5.3.6 'checksum))

    (hash-set!
     pkg-ht pkg-name
     (hash-set* ht
                'name pkg-name
                'source source-5.3.6
                'checksum checksum-5.3.6
                'last-updated (hash-ref ht 'last-updated (current-seconds))
                'last-checked (hash-ref ht 'last-checked (current-seconds))
                'last-edit (hash-ref ht 'last-edit (current-seconds))
                'versions versions-ht
                'ring (hash-ref ht 'ring 2)
                'dependencies (hash-ref ht 'dependencies empty)
                'modules (hash-ref ht 'modules empty)
                'tags (hash-ref ht 'tags empty)
                'authors (author->list (hash-ref ht 'author "")))))

  (define (package-info pn)
    (hash-ref pkg-ht pn))

  (define (format-time s)
    (if s
      (with-handlers ([exn:fail? (λ (x) "")])
        (parameterize ([date-display-format 'iso-8601])
          (date->string (seconds->date s #f) #t)))
      ""))

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

  (define ring-01
    (filter (λ (p) (member (hash-ref (package-info p) 'ring) '(0 1))) pkg-list))

  (define (package-conflicts? pkg)
    (filter (λ (other-pkg)
              (if (equal? pkg other-pkg)
                #f
                (packages-conflict?/cache pkg other-pkg)))
            ring-01))

  (define (package-url->useful-url pkg-url-str)
    (define pkg-url
      (string->url pkg-url-str))
    (match (url-scheme pkg-url)
      ["github"
       (match (url-path pkg-url)
         [(list* user repo branch path)
          (url->string
           (struct-copy
            url pkg-url
            [scheme "http"]
            [path (list* user repo (path/param "tree" empty) branch path)]))]
         [_
          pkg-url-str])]
      ["git"
       (match (url-path pkg-url)
         ;; xxx make this more robust
         [(list user repo)
          (url->string
           (struct-copy
            url pkg-url
            [scheme "http"]
            [path (list user repo (path/param "tree" empty)
                        (path/param "master" empty))]))]
         [_
          pkg-url-str])]
      [_
       pkg-url-str]))

  (for ([pkg (in-hash-keys pkg-ht)])
    (define pb (hash-ref build-summary pkg #f))
    (define (pbl k)
      (and pb (hash-ref pb k #f)))

    (hash-update!
     pkg-ht pkg
     (λ (ht)
       (define conflicts (package-conflicts? pkg))
       (hash-set*
        ht
        'build
        (hash 'success-log (pbl 'success-log)
              'failure-log (pbl 'failure-log)
              'dep-failure-log (pbl 'dep-failure-log)
              'conflicts-log (pbl 'conflicts-log)
              'docs
              (for/list ([d (in-list (or (pbl 'docs) empty))])
                (match d
                  [(doc/main n p) (list "main" n p)]
                  [(doc/extract n p) (list "extract" n p)]
                  [(doc/salvage n p) (list "salvage" n p)]
                  [(doc/none n) (list "none" n)])))
        'conflicts conflicts
        'versions
        (for/hash ([(v vht) (in-hash (hash-ref ht 'versions))])
          (values v
                  (hash-set vht 'source_url
                            (package-url->useful-url (hash-ref vht 'source)))))
        'search-terms
        (let* ([st (hasheq)]
               [st (for/fold ([st st])
                       ([t (in-list (hash-ref ht 'tags))])
                     (hash-set st (string->symbol t) #t))]
               [st (hash-set
                    st
                    (string->symbol
                     (format "ring:~a" (hash-ref ht 'ring))) #t)]
               [st (for/fold ([st st])
                       ([a (in-list (author->list (hash-ref ht 'author)))])
                     (hash-set
                      st (string->symbol (format "author:~a" a)) #t))]
               [st (if (empty? (hash-ref ht 'tags))
                     (hash-set st ':no-tag: #t)
                     st)]
               [st (if (hash-ref ht 'checksum-error #f)
                     (hash-set st ':error: #t)
                     st)]
               [st (if (equal? "" (hash-ref ht 'description ""))
                     (hash-set st ':no-desc: #t)
                     st)]
               [st (if (empty? conflicts)
                     st
                     (hash-set st ':conflicts: #t))]
               [st (if (pbl 'success-log)                     
                     (hash-set st ':build-success: #t)
                     st)]
               [st (if (pbl 'failure-log)                     
                     (hash-set st ':build-fail: #t)
                     st)]
               [st (if (pbl 'dep-failure-log)                     
                     (hash-set st ':build-dep-fail: #t)
                     st)]
               [st (if (pbl 'conflicts-log)                     
                     (hash-set st ':build-conflicts: #t)
                     st)]
               [pb-docs (pbl 'docs)]
               [st (if (and pb-docs (cons? pb-docs)
                            (andmap (λ (d)
                                      (or (doc/main? d)
                                          (doc/extract? d)
                                          (doc/salvage? d)))
                                    pb-docs))
                     (hash-set st ':docs: #t)
                     st)]
               [st (if (and pb-docs (cons? pb-docs)
                            (andmap (λ (d)
                                      (or (doc/extract? d)
                                          (doc/salvage? d)
                                          (doc/none? d)))
                                    pb-docs))
                     (hash-set st ':docs-error: #t)
                     st)])
          st)))))


  (define basic-dispatch
    (pkg-index/basic
     (λ () pkg-list)
     (λ (pkg-name) (hash-ref pkg-ht pkg-name))))

  (define (page/atom.xml req)
    (define ps
      (sort (map package-info pkg-list)
            >
            #:key (λ (i) (hash-ref i 'last-updated))))
    (define top (hash-ref (first ps) 'last-updated))
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
             (format "http://pkg.racket-lang.org/#[~a]"
                     p))
           (define lu (atom-format-time (hash-ref i 'last-updated)))
           (define a (first (author->list (hash-ref i 'author))))
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
                               `(div
                                 (p ,(format "~a package updated on ~a."
                                             p lu))
                                 (p ,(format
                                      "Checksum: ~a"
                                      (hash-ref (hash-ref (hash-ref i 'versions (hash)) 
                                                          'default (hasheq))
                                                'checksum "")))
                                 (p ,(hash-ref i 'description))))))))))))

  (define-values (main-dispatch main-url)
    (dispatch-rules
     [("atom.xml") page/atom.xml]
     [else basic-dispatch]))

  (define (url->request u)
    (make-request #"GET" (string->url u) empty
                  (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))

  (define (cache url file)
    (define p (build-path static-path file))
    (make-directory* (path-only p))

    (define bs
      (with-output-to-bytes
       (λ ()
         ((response-output (main-dispatch (url->request url)))
          (current-output-port)))))
    (unless (and (file-exists? p)
                 (bytes=? bs (file->bytes p)))
      (with-output-to-file p
        #:exists 'replace
        (λ () (display bs)))
      (with-output-to-file (path-add-suffix p #".json")
        #:exists 'replace
        (λ () (write-json (convert-to-json (file->value p))))))
    (void))

  (system (format "cp -pr ~a/* ~a/"
                  static.src-path
                  static-path))

  (cache "/atom.xml" "atom.xml")
  (cache "/pkgs" "pkgs")
  (cache "/pkgs-all" "pkgs-all")
  (for ([p (in-list pkg-list)])
    (cache (format "/pkg/~a" p) (format "pkg/~a" p)))

  (let ()
    (define pkg-path (build-path static-path "pkg"))
    (for ([f (in-list (directory-list pkg-path))]
          #:unless (regexp-match #"json$" (path->string f))
          #:unless (member (path->string f) pkg-list))
      (with-handlers ([exn:fail:filesystem? void])
        (delete-file (build-path pkg-path f))
        (delete-file (build-path pkg-path (path-add-suffix f #".json")))))))

(define (do-static pkgs)
  (notify! "update upload being computed: the information below may not represent all recent changes and updates")
   ;; FUTURE make this more efficient by looking at pkgs
  (generate-static)
  (run-s3! pkgs))
(define (run-static! pkgs)
  (run! do-static pkgs))
(define (signal-static! pkgs)
  (thread (λ () (run-static! pkgs))))

(provide do-static
         run-static!
         signal-static!)

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "static"
   #:args pkgs
   (do-static pkgs)))
