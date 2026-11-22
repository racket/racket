#lang racket/base
(require racket/place)

;; Bignum division and printing use temporary memory managed by GMP's
;; allocation stack, which must be place-local. Run those operations
;; concurrently in multiple places; if the GMP state is shared, this
;; crashes quickly (e.g., a bad free or an out-of-memory abort due to
;; a corrupted size).

(define ITERATIONS 2000)

(define (work id)
  (for ([i (in-range ITERATIONS)])
    (define a (expt 3 (+ 50 (random 300))))
    (define b (+ 1 (expt 2 (random 200))))
    (define q (quotient (* a b) b))
    (unless (= q a)
      (error 'place-bignum "bad quotient in ~a" id))
    (define s (number->string a))
    (unless (= a (string->number s))
      (error 'place-bignum "bad printing in ~a" id))
    (exact->inexact (/ a b))))

(define (start-worker id)
  (define p (place ch
                   (define id (place-channel-get ch))
                   (work id)
                   (place-channel-put ch 'done)))
  (place-channel-put p id)
  p)

(define (go)
  (define ps (for/list ([id (in-range 2)])
               (start-worker id)))
  (work 'main)
  (for ([p (in-list ps)])
    (unless (eq? 'done (place-channel-get p))
      (error 'place-bignum "worker failed"))
    (unless (zero? (place-wait p))
      (error 'place-bignum "worker exited with failure"))))

(module+ main
  (go))

(module+ test
  (go))
