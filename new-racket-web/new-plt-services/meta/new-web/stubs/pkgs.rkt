#lang plt-web

(require (only-in "../www/resources.rkt" www-site))

(define pkgs-site (site "stubs/pkgs"
                        #:url "http://pkgs.racket-lang.org/"
                        #:always-abs-url? #t
                        #:page-style? #f
                        #:share-from www-site))

(provide pkgs)
(define pkgs
  ;; This is a stub page to get the header for tweaked doc pages
  (page #:site pkgs-site
        #:file "" #:link-title "Racket Packages"
        "Placeholder...\n"))
