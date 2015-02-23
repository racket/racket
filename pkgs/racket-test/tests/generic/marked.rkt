#lang racket/base

(require racket/generic)

(define-generics thing
  (process thing stuff))

(define-syntax-rule (define-thing name)
  (struct name []
    #:methods gen:thing
    [(define-syntax-rule (define-super method)
       (define/generic method process))
     (define-super super)
     (define (process x y)
       (super y x))]))

(define-thing type)
