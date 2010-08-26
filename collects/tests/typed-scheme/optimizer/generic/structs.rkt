#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(define-struct: pt ((x : Integer) (y : Integer)) #:mutable)
(define a (pt 3 4))
(pt-x a)
(set-pt-y! a 5)
