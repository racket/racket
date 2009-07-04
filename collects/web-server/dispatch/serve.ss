#lang scheme
(require web-server/servlet-env
         web-server/http)

(define (serve/dispatch dispatch)
  (serve/servlet dispatch
                 #:servlet-path "/"
                 #:servlet-regexp #rx""))

(provide/contract
 [serve/dispatch ((request? . -> . response/c) . -> . void)])
