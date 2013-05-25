#lang typed/racket/base
(define-type (adder lhs rhs) (lhs rhs -> Number))
(define-struct: (lhs rhs) adder-box ((a : adder)))
