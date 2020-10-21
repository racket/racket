#lang racket/base
(require racket/fasl)

(provide ->fasl
         fasl->)

;; Variants without keyword arguments:
(define (->fasl v [handle-fail #f]) (s-exp->fasl v #:handle-fail handle-fail))
(define (fasl-> f) (fasl->s-exp f))
