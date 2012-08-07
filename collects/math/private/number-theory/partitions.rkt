#lang typed/racket
(provide partitions)

(define-predicate natural? Natural)

(define memo ((inst make-hasheqv Natural Natural)))

(: partitions : Integer -> Natural)
; http://en.wikipedia.org/wiki/Partition_(number_theory)
(define (partitions n)
  (cond
    [(< n 0) 0]
    [(= n 0) 1]
    [else  
     (define m ((inst hash-ref Natural Natural False) memo n (λ () #f)))
     (cond
       [m m]
       [else
        (define: sum : Integer 0)
        (for: ([k : Integer (in-range 1 (add1 (inexact->exact (floor (assert (/ (+ 1.0 (sqrt (+ 1.0 (* 24.0 n)))) 6.0) real?)))))])
          (set! sum (+ sum (* (if (odd? k) 1 -1)
                              (+ (partitions (- n (quotient (* k (- (* 3 k) 1)) 2)))
                                 (partitions (- n (quotient (* k (+ (* 3 k) 1)) 2))))))))
        (hash-set! memo n (assert sum natural?))
        (assert sum natural?)])]))

