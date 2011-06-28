#lang at-exp typed/racket

(define contents
 (lambda args args))

(define doc @contents{x y})

@contents{x y}
