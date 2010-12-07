#lang racket/base
(require racket/contract
         unstable/contract
         web-server/http)  

(define current-response/c
  (make-parameter any/c))
(define response/c
  (dynamic/c any/c current-response/c response?))

(provide/contract
 [current-response/c (parameter/c contract?)]
 [response/c contract?])
