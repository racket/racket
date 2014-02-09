#lang plt-web
(require plt-web/style)

(provide www-site)

(define www-site
  (site "www"
        #:navigation
        (list
         @a[href: "http://pkgs.racket-lang.org"]{Packages}
         @a[href: "http://docs.racket-lang.org"]{Documentation}
         @a[href: "http://blog.racket-lang.org"]{Blog}
         @navigation-button[@(a href: (resource "download/" #f) "Download")])))
