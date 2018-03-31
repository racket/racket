#lang racket/base
(require "macro1.rkt")
(provide go)

(define (go)
  (m 'not-a-string))

(module+ test
  (void))
