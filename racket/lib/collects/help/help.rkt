#lang scheme/base

(require "search.rkt" scheme/cmdline scheme/list scheme/string
         raco/command-name) 

;; Minimal command-line arguments, the query string can contain all
;; kinds of magic.
(command-line
 #:program (short-program+command-name)
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
