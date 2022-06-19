#lang racket

(require rackunit
         data/bit-vector)

(define bv (bit-vector #f #f #f))

(test-case "bit-vector-set!"
  (check-exn #rx"index is out of range"
             (λ () (bit-vector-set! bv 4 #t)))

  (check-exn #rx"expected: natural\\?"
             (λ () (bit-vector-set! bv -1 #t)))

  (check-exn #rx"expected: bit-vector\\?"
             (λ () (bit-vector-set! 1 0 #t))))

(test-case "bit-vector-ref"
  (check-exn #rx"index is out of range"
             (λ () (bit-vector-ref bv 4)))

  (check-exn #rx"expected: natural\\?"
             (λ () (bit-vector-ref bv -1)))

  (check-exn #rx"expected: bit-vector\\?"
             (λ () (bit-vector-ref 1 0))))
