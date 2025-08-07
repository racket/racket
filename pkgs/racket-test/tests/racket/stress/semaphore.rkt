#lang racket/base
(require ffi/unsafe/atomic)

(define N 100000)

(define s1 (make-semaphore 1))
(define s2 (make-semaphore))

(define (go s1 s2)
  (for ([i (in-range N)])
    (semaphore-wait s1)
    (semaphore-post s2)))

(define (cham pool)
  (time
   (for-each
    thread-wait
    (list
     (thread #:pool pool (lambda () (go s1 s2)))
     (thread #:pool pool (lambda () (go s2 s1)))))))

(cham #f)
(cham 'own)
(cham (make-parallel-thread-pool 1))
(cham (make-parallel-thread-pool 2))
