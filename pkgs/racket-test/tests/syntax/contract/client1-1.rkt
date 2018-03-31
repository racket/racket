#lang racket/base
(require tests/syntax/contract/macro1)
(provide go)

(define (go)
  (m 'not-a-string))

(module+ test
  (void))
