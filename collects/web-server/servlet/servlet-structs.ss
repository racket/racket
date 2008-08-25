#lang scheme/base
(require scheme/contract)
(require "../private/request-structs.ss"
         "../private/response-structs.ss")  

(define k-url?
  string?)

(define response-generator/c
  (k-url? . -> . response?))

(define url-transform/c
  (k-url? . -> . k-url?))

(define expiration-handler/c
  (or/c false/c
        (request? . -> . response?)))

(define embed/url/c
  (((request? . -> . any/c)) (expiration-handler/c) . ->* . string?))

(provide/contract
 [response-generator/c contract?]
 [k-url? (any/c . -> . boolean?)]
 [url-transform/c contract?]
 [expiration-handler/c contract?]
 [embed/url/c contract?])
