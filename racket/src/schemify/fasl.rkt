#lang racket/base
(require racket/fasl)

(provide ->fasl
         fasl->)

;; Variants without keyword arguments:
(define (->fasl v) (s-exp->fasl v))
(define (fasl-> f) (fasl->s-exp f))
