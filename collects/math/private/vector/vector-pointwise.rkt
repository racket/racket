#lang typed/racket

(require racket/unsafe/ops)

(provide vector=)

(: vector= (case-> ((Vectorof Float)   (Vectorof Float)   -> Boolean)
                   ((Vectorof Integer) (Vectorof Integer) -> Boolean)
                   ((Vectorof Real)    (Vectorof Real)    -> Boolean)
                   ((Vectorof Number)  (Vectorof Number)  -> Boolean)))
(define (vector= xs ys)
  (define n (vector-length xs))
  (cond [(not (= n (vector-length ys)))  #f]
        [else
         (let loop ([#{j : Nonnegative-Fixnum} 0])
           (cond [(j . < . n)
                  (cond [(not (= (unsafe-vector-ref xs j) (unsafe-vector-ref ys j)))  #f]
                        [else  (loop (+ j 1))])]
                 [else  #t]))]))
