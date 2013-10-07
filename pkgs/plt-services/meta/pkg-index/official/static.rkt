#lang racket/base
(require web-server/http
         racket/file
         racket/match
         json
         net/url
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
   [#f
    #f]
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

  (define dispatch
    (pkg-index/basic
     (λ () pkg-list)
     (λ (pkg-name)
       (define ht (file->value (build-path pkgs-path pkg-name)))
       (hash-set* ht
                  'name pkg-name
                  'tags (hash-ref ht 'tags empty)
                  'authors (author->list (hash-ref ht 'author ""))))))

  (define (url->request u)
    (make-request #"GET" (string->url u) empty
                  (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))

  (define (cache url file)
    (define p (build-path static-path file))
    (make-directory* (path-only p))
    (with-output-to-file p
      #:exists 'replace
      (λ () ((response-output (dispatch (url->request url))) (current-output-port))))
    (with-output-to-file (path-add-suffix p #".json")
      #:exists 'replace      
      (λ () (write-json (convert-to-json (file->value p)))))
    (void))

  (cache "/pkgs" "pkgs")
  (cache "/pkgs-all" "pkgs-all")
  (for ([p (in-list pkg-list)])
    (cache (format "/pkg/~a" p) (format "pkg/~a" p))))
