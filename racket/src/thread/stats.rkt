#lang racket/base
(require "check.rkt"
         "thread.rkt"
         "time.rkt")

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
     (maybe-set! 0 (current-process-milliseconds))
     (maybe-set! 1 (current-milliseconds))
     (maybe-set! 2 (current-gc-milliseconds))
     (maybe-set! 3 0) ; # of GCs
     (maybe-set! 4 0) ; # of thread switches
     (maybe-set! 5 0) ; # of stack overflows
     (maybe-set! 6 0) ; # of threads scheduled for running
     (maybe-set! 7 0) ; # of syntax objects read
     (maybe-set! 8 0) ; # of hash table searches
     (maybe-set! 9 0) ; # of hash table collisions
     (maybe-set! 10 0) ; non-GCed memory allocated for machine code
     (maybe-set! 11 0) ; peak memory use before a GC
     (void)]
    [else
     (maybe-set! 0 (thread-running? thd))
     (maybe-set! 1 (thread-dead? thd))
     (maybe-set! 2 #f) ; blocked for synchronization?
     (maybe-set! 3 #f) ; continuation size in bytes
     (void)]))
