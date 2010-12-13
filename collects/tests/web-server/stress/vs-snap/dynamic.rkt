#lang racket/base
(require web-server/servlet-env
         web-server/http
         racket/list)

(define resp
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list #"PONG")))

(define (start req)
  resp)

(serve/servlet start
               #:servlet-regexp #rx""
               #:port 8000
               #:command-line? #t)
