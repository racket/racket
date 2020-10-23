#lang racket/base
(require racket/place)

(define N 100)

(define (go)
  (place
   pch
   (for/fold ([v 0]) ([x (in-range N)])
     (place-channel-put pch (for/vector ([l 1000])
                              (/ (- (expt 2 300) (random 10))
                                 (expt 2 81))))
     (apply + (vector->list (place-channel-get pch))))))

(module+ main
  (define p1 (go))
  (for ([i (in-range N)])
    (define v (place-channel-get p1))
    (place-channel-put p1 v))
  (place-wait p1))

(module+ test
  (require (submod ".." main)))
