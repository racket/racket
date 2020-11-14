#lang racket/base

;; This test should take tens of milliseconds. With bad hash
;; functions, it can easily take tens of seconds.

(define N 30000)

(define start-time (current-process-milliseconds))

(define (test-hashing ht)
  (time
   (for ([i N])
     (hash-set! ht i i)))

  (hash-clear! ht)
  (time
   (for ([i N])
     (hash-set! ht (+ 0.0 i) i)))

  (hash-clear! ht)
  (time
   (for ([i N])
     (hash-set! ht (arithmetic-shift i 16) i)))

  (hash-clear! ht)
  (time
   (for ([i N])
     (hash-set! ht (arithmetic-shift i 32) i)))

  (hash-clear! ht)
  (time
   (for ([i N])
     (hash-set! ht (arithmetic-shift i 100) i))))

(test-hashing (make-hash))
(test-hashing (make-hasheqv))
(test-hashing (make-hasheq))

(test-hashing (make-weak-hash))
(test-hashing (make-weak-hasheqv))
(test-hashing (make-weak-hasheq))

(when (> (- (current-process-milliseconds) start-time)
         (* 1000.0 10))
  (error "number hashing seems to take orders of magnitude too long"))

