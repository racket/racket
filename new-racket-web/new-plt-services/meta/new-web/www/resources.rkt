#lang plt-web
(require plt-web/style)

(provide www-site)

(define www-site
  (site "www"
        #:navigation
        (list
         @a[href: (resource "stubs/pkgs" #f)]{Packages}
         @a[href: (resource "stubs/docs" #f)]{Documentation}
         @a[href: (resource "stubs/blog" #f)]{Blog}
         @navigation-button[@(a href: (resource "download/" #f) "Download")])))
