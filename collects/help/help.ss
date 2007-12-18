#lang scheme/base

(require "search.ss"
         net/sendurl
         setup/dirs
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
    (let ([dest-path (build-path (find-doc-dir) "start" "index.html")])
      (send-url (format "file://~a" (path->string dest-path))))]
   [else
    (generate-search-results search-term)]))
