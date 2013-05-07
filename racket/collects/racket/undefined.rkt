#lang racket/base
(require '#%kernel)

(provide check-not-undefined
         undefined
         undefined?)

(define (undefined? v) (eq? v undefined))
