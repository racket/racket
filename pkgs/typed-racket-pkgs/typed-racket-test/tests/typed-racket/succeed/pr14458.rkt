#lang typed/racket

;; Test for PR 14458. Make sure that overlap checking on
;; invariant struct types works against simple data types.

(struct: (X) S ([z : (Vectorof X)]))
(define-type (T X) (U 'Leaf (S X)))

(: f (∀ (X) (T X) → Any))
(define (f s)
  (match s
    [(S _) 42]))
