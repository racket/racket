#lang scheme/base

(require "search.ss" scheme/cmdline scheme/list)

;; Minimal command-line arguments, the query string can contain all
;; kinds of magic.
(command-line
 #:handlers
 (lambda (_ . ts)
   (perform-search (apply string-append (add-between ts " "))))
 '("search-terms")
 (lambda (help-str)
   (display help-str)
   (display " See the search page for the syntax of queries\n")
   (exit 0)))
