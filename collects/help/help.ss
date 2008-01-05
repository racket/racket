#lang scheme/base

(require "search.ss"
         scheme/cmdline)

(define exact-search? #f)

(command-line
 #:once-any (["--exact" "-x"] "Go directly to the first exact hit for the search term" (set! exact-search? #t))
 #:args search-term
 (cond
   [exact-search?
    (when (null? search-term)
      (error 'plt-help "expected a search term after -x or --exact"))
    (unless (null? (cdr search-term))
      (error 'plt-help "expected a single search term, got ~s" search-term))
    (send-exact-results (car search-term))]
   [(null? search-term)
    (send-main-page)]
   [else
    (generate-search-results search-term)]))
