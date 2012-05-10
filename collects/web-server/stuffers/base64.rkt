#lang racket/base
(require racket/contract
         web-server/stuffers/stuffer
         net/base64)

(define base64-stuffer
  (make-stuffer base64-encode base64-decode))

(provide/contract
 [base64-stuffer (stuffer/c bytes? bytes?)])
