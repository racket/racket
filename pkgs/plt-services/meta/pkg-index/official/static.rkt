#lang racket/base
(require web-server/http
         web-server/dispatch
         racket/file
         racket/match
         json
         racket/date
         net/url
         xml
         racket/list
         racket/path
         racket/promise
         meta/pkg-index/basic/main)

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

(module+ main
  (require "common.rkt")

  (define pkg-list
    (map path->string (directory-list pkgs-path)))
  (define pkg-ht
    (for/hash ([pkg-name (in-list pkg-list)])
      (define ht (file->value (build-path pkgs-path pkg-name)))

      (values pkg-name
              (hash-set* ht
                         'name pkg-name
                         'tags (hash-ref ht 'tags empty)
                         'search-terms
                         (let* ([st (hasheq)]
                                [st (for/fold ([st st])
                                        ([t (in-list (hash-ref ht 'tags empty))])
                                      (hash-set st (string->symbol t) #t))]
                                [st (hash-set st (string->symbol (format "ring:~a" (hash-ref ht 'ring 2))) #t)]
                                [st (for/fold ([st st])
                                        ([a (in-list (author->list (hash-ref ht 'author "")))])
                                      (hash-set st (string->symbol (format "author:~a" a)) #t))]
                                [st (if (empty? (hash-ref ht 'tags empty))
                                      (hash-set st ':no-tag: #t)
                                      st)]
                                [st (if (hash-ref ht 'checksum-error #f)
                                      (hash-set st ':error: #t)
                                      st)])
                           st)
                         'authors (author->list (hash-ref ht 'author ""))))))

  (define basic-dispatch
    (pkg-index/basic
     (λ () pkg-list)
     (λ (pkg-name) (hash-ref pkg-ht pkg-name))))

  (define (package-info pn)
    (hash-ref pkg-ht pn))

  (define (format-time s)
    (if s
      (with-handlers ([exn:fail? (λ (x) "")])
        (parameterize ([date-display-format 'iso-8601])
          (date->string (seconds->date s #f) #t)))
      ""))

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
                               `(p
                                 ,(format "~a package updated on ~a."
                                          p lu)))))))))))

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
    (with-output-to-file p
      #:exists 'replace
      (λ () ((response-output (main-dispatch (url->request url))) (current-output-port))))
    (with-output-to-file (path-add-suffix p #".json")
      #:exists 'replace
      (λ () (write-json (convert-to-json (file->value p)))))
    (void))

  (cache "/atom.xml" "atom.xml")
  (cache "/pkgs" "pkgs")
  (cache "/pkgs-all" "pkgs-all")
  (for ([p (in-list pkg-list)])
    (cache (format "/pkg/~a" p) (format "pkg/~a" p))))
