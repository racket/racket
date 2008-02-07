#lang scheme/base

(require "search.ss" scheme/cmdline)

(define go-if-one?     #t)
(define exact-search?  #f)
(define regexp-search? #f)

(command-line
 #:once-any
 [("--go" "-g") "Go directly to search result if only one (default)"
  (set! go-if-one? #t)]
 [("--no-go" "-G") "Show search results page even if only one result"
  (set! go-if-one? #t)]
 #:once-each
 [("--exact" "-x") "Search for the given term exactly"
  (set! exact-search? #t)]
 [("--regexp" "-r") "Search for the given regexp"
  (set! regexp-search? #t)]
 #:args search-terms
 (let ([one? (= 1 (length search-terms))])
   (cond [(and regexp-search? (not one?))
          (error 'plt-help "expected a single regexp after -r or --regexp")]
         [(and exact-search? (not one?))
          (error 'plt-help "expected a single search term after -x or --exact")]
         [(null? search-terms) (send-main-page)]
         [else (perform-search (if regexp-search?
                                 (list (regexp (car search-terms)))
                                 search-terms)
                               #:exact? (or exact-search? regexp-search?)
                               #:go-if-one? go-if-one?)])))
