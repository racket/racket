#lang scheme/base
(require scheme/contract
         web-server/http)  

(define k-url?
  string?)

(define response-generator/c
  (k-url? . -> . response?))

(define expiration-handler/c
  (or/c false/c
        (request? . -> . response?)))

(define embed/url/c
  (((request? . -> . any/c)) (expiration-handler/c) . ->* . string?))

(provide/contract
 [response-generator/c contract?]
 [k-url? (any/c . -> . boolean?)]
 [expiration-handler/c contract?]
 [embed/url/c contract?])
