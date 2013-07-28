#lang typed/scheme

(ann (vector-ref (ann #(1 foo 3) (Vector Integer Symbol Any)) 0) Integer)

(define: x : (Vector Number String Symbol) (vector 1 "foo" 'bar))

(define: y : 2 2)

(ann (vector-ref x 1) String)
(ann (vector-ref x y) Symbol)

(: ref : Integer -> (U Number String Symbol))
(define (ref i) (vector-ref x i))

(ann (vector-ref #(1.0 2.0) 0) Flonum)
