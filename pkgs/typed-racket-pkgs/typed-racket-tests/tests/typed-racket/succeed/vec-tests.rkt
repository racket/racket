#lang typed-scheme
(define: x : (Vectorof Number) (build-vector 5 (lambda: ([x : Number]) 0)))
(define: y : Number (vector-ref x 1))

