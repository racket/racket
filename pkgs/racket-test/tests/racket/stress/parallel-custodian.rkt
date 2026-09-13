#lang racket/base

(for ([i 10000])
  (parallel-thread-pool-close (make-parallel-thread-pool 1)))

(for ([i 10000])
  (parameterize ([current-custodian (make-custodian)])
    (make-parallel-thread-pool 1)
    (custodian-shutdown-all (current-custodian))))

(for ([i (in-range 1000)])
  (define ts
    (for/list ([j (in-range 8)])
      (parameterize ([current-custodian (make-custodian)])
        (thread
         (lambda ()
           (custodian-shutdown-all (current-custodian)))
         #:pool 'own))))
  (map thread-wait ts))

(for ([i (in-range 1000)])  
  (define ts
    (for/list ([j (in-range 8)])
      (parameterize ([current-custodian (make-custodian)])
        (thread
         (lambda ()
           (custodian-shutdown-all (current-custodian)))
         #:pool (make-parallel-thread-pool 1)))))
  (map thread-wait ts))
