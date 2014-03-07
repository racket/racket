#lang plt-web
(require "../testing.rkt")

(provide download-site)

(define download-site (site "download"
                            #:url (rewrite-for-testing "http://download.racket-lang.org/")))
