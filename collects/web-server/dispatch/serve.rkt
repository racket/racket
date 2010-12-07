#lang racket
(require web-server/servlet-env
         web-server/servlet/servlet-structs
         web-server/http)

(define (serve/dispatch dispatch)
  (serve/servlet dispatch
                 #:servlet-path "/"
                 #:servlet-regexp #rx""))

(provide/contract
 [serve/dispatch ((request? . -> . can-be-response?) . -> . void)])
