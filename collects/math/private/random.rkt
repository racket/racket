#lang typed/racket/base

(provide signed-random)

(: signed-random (-> Float))
;; Chooses a random flonum in [-1,1] in a way that preserves the precision of the random flonum
;; returned by (random)
(define (signed-random)
  (define r (random))
  (if ((random) . > . 0.5) r (- r)))
