#lang typed/racket

(define-type CyclicSymbols (Rec X (Pair Symbol X)))

(: mycar : (CyclicSymbols -> Symbol))
(define (mycar lst)
  (car lst))

