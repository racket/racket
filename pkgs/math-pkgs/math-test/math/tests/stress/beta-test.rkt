#lang racket

;; This is a stress test for Racket, not for math.
;; (Producing a smaller test for Racket is difficult.)

(require math/special-functions
         math/statistics)

(define (f x)
  (flbeta-inc 2.0 3.0 x #f #t))

(define-values (l r)
  (count-samples
   (for/list ([_  (in-range 100000)])
     (f #i40/499))))

(unless (= 1 (length l))
  (error "failed: ~s ~s\n" l r))
