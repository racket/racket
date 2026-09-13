#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type num_t (union_t
                         [i int64_t]
                         [d double_t]))

(check-equal? (ffi2-sizeof num_t) 8)
(check-equal? (ffi2-offsetof num_t i) 0)
(check-equal? (ffi2-offsetof num_t d) 0)

(let ()
  (define n (ffi2-malloc num_t))
  (check-true (num_t*? n))
  (set-num_t-i! n 1)
  (check-equal? (num_t-i n) 1)
  (check-equal? (num_t-d n) 5e-324)
  (check-equal? (ffi2-ref n double_t) 5e-324)
  (set-num_t-d! n 100.0)
  (check-equal? (num_t-i n) 4636737291354636288))

(let ()
  (define n (num_t i 1))
  (check-true (num_t*? n))
  (check-equal? (num_t-i n) 1)
  (check-equal? (num_t-d n) 5e-324))

(let ()
  (define n (num_t d 100.0))
  (check-true (num_t*? n))
  (check-equal? (num_t-i n) 4636737291354636288)
  (check-equal? (num_t-d n) 100.0))

(let ()
  (define ns (ffi2-malloc num_t))
  (check-true (num_t*? ns))
  (check-equal? (num_t*-ref ns 0) ns)
  (check-equal? (num_t*-ref ns 1) (ffi2-add ns num_t 1)))
