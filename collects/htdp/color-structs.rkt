#lang racket/base
(provide (struct-out color)
         (struct-out alpha-color))
(define-struct color (red green blue) #:transparent)
(define-struct alpha-color (alpha red green blue) #:transparent)
