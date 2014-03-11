#lang plt-web
(require "../www/resources.rkt"
         "../identity.rkt"
         "../testing.rkt")

(provide con-site
         rcon)

(define con-site
  (site "con"
        #:url (rewrite-for-testing "http://con.racket-lang.org/")
        #:page-headers (identity-headers)
        #:share-from www-site))

(define (rcon [year #f] . text)
  (define years '(2014 2013 2012 2011))
  (a href: (list ((resource "con/" #f))
                 (and year (not (eq? year (car years))) (list year "/")))
     (cond [(pair? text) text]
           [(not year) "RacketCon"]
           [else year])))

