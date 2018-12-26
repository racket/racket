#lang racket/base
(require racket/fasl)

(provide fasl->s-exp/intern)

(define (fasl->s-exp/intern s)
  (fasl->s-exp s #:datum-intern? #t))
