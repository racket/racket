#lang racket/base
(require ffi/unsafe)

(define f (lambda () (void)))
(define f-ptr (cast f (_fun #:atomic? #t #:async-apply (lambda (f) (f)) -> _void) _fpointer))
(define f-back (cast f-ptr _fpointer (_fun -> _void)))

(define (call)
  (f-back))

(define (run-loop n-iters n-threads
                  #:make-pool [make-pool void]
                  #:pick-pool [pick-pool (lambda (p) (make-parallel-thread-pool 1))]
                  #:close-pool [close-pool void])
  (for ([i (in-range n-iters)])
    (printf "~s\n" i)
    (define pool (make-pool))
    (define ts
      (for/list ([j (in-range n-threads)])
        (parameterize ([current-custodian (make-custodian)])
          (thread
           #:pool (pick-pool pool)
           (lambda ()
             (let loop () (call) '(sleep) '(loop)))))))
    (close-pool pool)
    (sleep 0.1)
    (map kill-thread ts)))

(run-loop 20 1)
(run-loop 5 4 #:make-pool (lambda () (make-parallel-thread-pool))
          #:pick-pool (lambda (p) p)
          #:close-pool parallel-thread-pool-close)
(run-loop 5 4)
(run-loop 5 4 #:pick-pool (lambda (pool) 'own))
(let ([p (make-parallel-thread-pool)])
  (run-loop 5 4 #:make-pool (lambda () p)
            #:pick-pool (lambda (p) p))
  (parallel-thread-pool-close p))
