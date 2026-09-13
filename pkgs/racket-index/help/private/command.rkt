#lang racket/base

(require racket/cmdline
         racket/list
         racket/string
         raco/command-name
         "search.rkt"
         "family.rkt")

(define language-family #f)

;; Minimal command-line arguments, the query string can contain all
;; kinds of magic.
(command-line
 #:program (short-program+command-name)
 #:once-each
 [("-f" "--family") name ("Navigate documentation as language family <name>"
                          "or the best found match to <name>")
                    (set! language-family name)]
 #:handlers
 (lambda (_ . ts)
   (define family-name (get-family-name language-family
                                        #:who (string->symbol (short-program+command-name))))
   (if (null? ts)
       (send-language-family-page family-name)
       (perform-search (string-append* (add-between ts " ")) #:language-family family-name)))
 '("search-terms")
 (lambda (help-str)
   (display help-str)
   (display " See the search page for the syntax of queries\n")
   (exit 0)))

(module test racket/base)
