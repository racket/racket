#lang plt-web

(require (only-in "../www/resources.rkt" www-site))

(define docs-site (site "stubs/docs" #:resources (site-resources www-site)))

(provide documentation)
(define documentation
  ;; This is a stub page to get the header for tweaked doc pages
  (page #:site docs-site
        #:file "" #:link-title "Documentation" #:window-title "{{{TITLE}}}"
        "\n{{{BODY}}}\n"))
