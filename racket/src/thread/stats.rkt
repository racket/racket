#lang racket/base
(require "check.rkt"
         "thread.rkt"
         (submod "thread.rkt" for-stats)
         "time.rkt"
         "schedule.rkt"
         (only-in "host.rkt"
                  host:get-system-stats)
         "thread-group.rkt")

(provide vector-set-performance-stats!)

(define/who (vector-set-performance-stats! vec [thd #f])
  (check who (lambda (v) (and (vector? v) (not (immutable? v))))
         #:contract "(and/c vector? (not/c immutable?))"
         vec)
  (check who thread? #:or-false thd)
  (define (maybe-set! i v)
    (when (< i (vector-length vec))
      (vector-set! vec i v)))
  (cond
    [(not thd)
     (define-values (gc-count) (host:get-system-stats))
     (maybe-set! 0 (current-process-milliseconds))
     (maybe-set! 1 (current-milliseconds))
     (maybe-set! 2 (current-gc-milliseconds))
     (maybe-set! 3 gc-count)
     (maybe-set! 4 thread-swap-count) ; # of thread swaps
     (maybe-set! 5 0) ; # of stack overflows
     (maybe-set! 6 num-threads-in-groups) ; # of threads scheduled for running
     (maybe-set! 7 0) ; # of syntax objects read
     (maybe-set! 8 0) ; # of hash table searches
     (maybe-set! 9 0) ; # of hash table collisions
     (maybe-set! 10 0) ; non-GCed memory allocated for machine code
     (maybe-set! 11 (current-memory-use 'peak))
     (void)]
    [else
     (maybe-set! 0 (thread-running? thd))
     (maybe-set! 1 (thread-dead? thd))
     (maybe-set! 2 (and (not (thread-dead? thd))
                        (or (thread-descheduled? thd)
                            (thread-sched-info thd)))) ; blocked for synchronization?
     (maybe-set! 3 0) ; continuation size in bytes
     (void)]))
