#lang racket
(require tests/eli-tester
         "ex-matthias-a.rkt")

(define memory (new memory%))

(define a (send memory malloc))
(test
 (send memory free a)
 (send memory free a) =error> #rx"disallowed call")
