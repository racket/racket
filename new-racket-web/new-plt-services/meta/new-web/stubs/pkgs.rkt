#lang plt-web

(define pkgs-site (site "stubs/pkgs"
                        #:page-style? #f))

(provide pkgs)
(define pkgs
  ;; This is a stub page to get the header for tweaked doc pages
  (page #:site pkgs-site
        #:file "" #:link-title "Racket Packages"
        "Placeholder...\n"))
