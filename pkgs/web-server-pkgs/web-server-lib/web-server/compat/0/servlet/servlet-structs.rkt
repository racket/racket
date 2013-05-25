#lang racket/base
(require racket/contract
         web-server/servlet/servlet-structs
         web-server/http)  

(define k-url?
  string?)

(define response-generator/c
  (k-url? . -> . can-be-response?))

(define expiration-handler/c
  (or/c false/c
        (request? . -> . can-be-response?)))

(define embed/url/c
  ((request? . -> . any) . -> . string?))

(provide/contract
 [response-generator/c contract?]
 [k-url? contract?]
 [expiration-handler/c contract?]
 [embed/url/c contract?])
