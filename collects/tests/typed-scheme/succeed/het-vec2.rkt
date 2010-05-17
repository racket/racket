#lang typed/racket

(ann (vector 1 2 3) (U String (Vector Integer Integer Integer) (Vectorof Number)))

(define v (ann (vector 1 2 3) (Vector Integer Integer Integer)))
(define v* (ann #(1 2 3) (Vector Integer Integer Integer)))

(vector-ref v 2)
(vector-ref v 0)
(define: x : Natural 0)
(define: x* : 0 0)

(vector-ref (vector 1 2 3) x)
(vector-ref v x*)

;(vector-set! v x 7)
(vector-set! v x* 7)


(vector-length v)
