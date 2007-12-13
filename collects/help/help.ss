#lang scheme/base

(require "search.ss"
         net/sendurl
         setup/dirs
         scheme/cmdline)

(command-line
 #:args search-term
 (cond
  [(null? search-term)
   (let ([dest-path (build-path (find-doc-dir) "start" "index.html")])
     (send-url (format "file://~a" (path->string dest-path))))]
  [else
   (generate-search-results search-term)]))
