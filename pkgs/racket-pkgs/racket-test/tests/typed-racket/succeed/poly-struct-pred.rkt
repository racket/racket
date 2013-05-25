#lang typed/racket

(struct: (X) s ([v : X]))

(: f : (All (X) (U 'foo (s X)) -> (s X)))
(define (f t)
  (match t
    [(s value) (s value)]
    [_ (error 'fail)]))
