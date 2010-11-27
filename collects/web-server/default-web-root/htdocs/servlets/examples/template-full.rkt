#lang racket
(require web-server/templates
         web-server/http)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

(define (start initial-request)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (include-template "static.html"))))
