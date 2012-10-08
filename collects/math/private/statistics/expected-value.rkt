#lang typed/racket #:no-optimize

(struct: (A) real-expectation ([rv : (A -> Real)] [value : Real] [samples : Natural])
  #:transparent)

(: update-real-expectation (All (A) ((real-expectation A) A -> (real-expectation A))))
(define (update-real-expectation e x)
  (define f (real-expectation-rv e))
  (define r0 (real-expectation-value e))
  (define n (add1 (real-expectation-samples e)))
  (define r (+ r0 (/ (- r0 (f x)) n)))
  (real-expectation f r n))
