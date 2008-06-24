#lang scheme/base

(require "search.ss" scheme/cmdline scheme/list scheme/string)

;; Minimal command-line arguments, the query string can contain all
;; kinds of magic.
(command-line
 #:handlers
 (lambda (_ . ts)
   (if (null? ts)
     (send-main-page)
     (perform-search (string-append* (add-between ts " ")))))
 '("search-terms")
 (lambda (help-str)
   (display help-str)
   (display " See the search page for the syntax of queries\n")
   (exit 0)))
