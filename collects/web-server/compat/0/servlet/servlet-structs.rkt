#lang racket/base
(require racket/contract
         unstable/contract
         web-server/http)  

(define current-response/c
  (make-parameter any/c))
(define response/c
  (dynamic/c any/c current-response/c response?))

(define k-url?
  string?)

(define response-generator/c
  (k-url? . -> . response/c))

(define expiration-handler/c
  (or/c false/c
        (request? . -> . response/c)))

(define embed/url/c
  ((request? . -> . any) . -> . string?))

(provide/contract
 [current-response/c (parameter/c contract?)]
 [response/c contract?]
 [response-generator/c contract?]
 [k-url? contract?]
 [expiration-handler/c contract?]
 [embed/url/c contract?])
