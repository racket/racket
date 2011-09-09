#lang typed-scheme
(define-typed-struct/exec X ([a : Number] [b : Boolean]) [(lambda: ([x : X]) (+ 3 (X-a x))) : (X -> Number)])
((make-X 1 #f))
