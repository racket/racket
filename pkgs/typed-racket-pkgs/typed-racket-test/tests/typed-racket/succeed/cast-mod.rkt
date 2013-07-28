#lang typed/racket/base

(cast 2 Number)
(cast 2 Integer)
(cast (list 2 4) (Listof Byte))
(cast (vector 2 4) (Vectorof Byte))


((cast (lambda (x) 7) (String -> Number)) "seven")
