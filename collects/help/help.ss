#lang scheme/base

(require "search.ss"
         browser/external
         setup/dirs
         mzlib/cmdline)

(define search-terms '())
(command-line "Help Desk"
              (current-command-line-arguments)
              (args search-term (set! search-terms search-term)))
              
(cond
  [(null? search-terms)
   (let ([dest-path (build-path (find-doc-dir) "start" "index.html")])
     (send-url (format "file://~a" (path->string dest-path))))]
  [else
   (generate-search-results search-terms)])
