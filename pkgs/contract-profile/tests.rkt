#lang racket/base

(require contract-profile (only-in contract-profile/utils dry-run?))

(module+ test
  (require rackunit)

  (dry-run? #t) ; don't output to files

  ;; reported by Greg Hendershott
  (check-true (contract-profile #t))

  (require math)
  (let ()
    (define dim 200)
    (define big1 (build-matrix dim dim (lambda (i j) (random))))
    (define big2 (build-matrix dim dim (lambda (i j) (random))))
    (define (main) (matrix* big1 big2))
    (check-true (matrix? (contract-profile (main)))))
  )
