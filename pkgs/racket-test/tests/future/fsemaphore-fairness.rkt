#lang racket/base
(require racket/future)

(define counts (make-vector 4))
(define total-count 0)

(define f (make-fsemaphore 0))
(define ts
  (for/list ([idx (in-range (vector-length counts))])
    (thread #:pool (and (even? idx) 'own)
            (lambda ()
              (let loop ()
                (fsemaphore-wait f)
                (define c (add1 total-count))
                (set! total-count (add1 total-count))
                (vector-set! counts idx (+ 1 (vector-ref counts idx)))
                (fsemaphore-post f)
                (unless (>= c 10000)
                  (loop)))))))

(sync (system-idle-evt))
;; all threads are now in the fsemaphore queue
(fsemaphore-post f)

(for-each thread-wait ts)

counts

(unless (eq? (system-type 'vm) 'racket) ; BC implementation is not fair
  (for ([i (in-range (sub1 (vector-length counts)))])
    (unless (< (/ (vector-ref counts 0) 2)
               (vector-ref counts (add1 i))
               (* (vector-ref counts 0) 2))
      (error "imbalanced"))))
