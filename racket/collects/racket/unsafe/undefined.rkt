#lang racket/base
(require '#%unsafe)

(provide check-not-unsafe-undefined
         unsafe-undefined
         unsafe-undefined?
         prop:chaperone-unsafe-undefined)

(define (unsafe-undefined? v) (eq? v unsafe-undefined))
