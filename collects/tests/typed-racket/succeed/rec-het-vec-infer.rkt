#lang typed/racket

(define-type (Set X) (Rec Set (U Null (Vector X Set))))

(: get-set-root (All (X) ((Set X) -> X)))
(define (get-set-root s) (error 'fail))

(: set-size (All (X) ((Set X) -> X)))
(define (set-size x) (get-set-root x))
