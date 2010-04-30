#lang racket/base
(require racket/contract
         web-server/http)  

(define k-url?
  string?)

(define response-generator/c
  (k-url? . -> . response/c))

(define expiration-handler/c
  (or/c false/c
        (request? . -> . response/c)))

(define embed/url/c
  ((request? . -> . any/c) . -> . string?))

(provide/contract
 [response-generator/c contract?]
 [k-url? (any/c . -> . boolean?)]
 [expiration-handler/c contract?]
 [embed/url/c contract?])
