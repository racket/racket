#lang typed/scheme

(define-struct: x ([y : Any]) #:mutable)

(define: the-x : Any (make-x 1))

(if (x? the-x)
    (x-y the-x)
    0)

