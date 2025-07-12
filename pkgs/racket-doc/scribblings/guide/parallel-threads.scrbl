#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "parallel-threads"]{Parallel Threads}

A @tech{thread} that is created with a @deftech{parallel thread pool}
can use a different hardware processor than other threads, providing
parallelism in addition to concurrency. When hardware is available,
using these @deftech{parallel threads} can reduce the latency of a
computation. @margin-note{A trade-off with parallel threads is that a
computation may instead get slower, instead of faster, due to overhead
creating and managing parallel tasks.} Create a parallel thread using
the @racket[#:pool] argument to @racket[thread], where @racket['own]
means that the thread gets its own, single-thread pool.

@racketblock[
(define (fib n)
  (cond
   [(= n 0) 1]
   [(= n 1) 1]
   [else (+ (fib (- n 1)) (fib (- n 2)))]))   
(define n1 (thread (lambda () (fib 35))
                   #:pool 'own
                   #:keep 'results))
(define n2 (thread (lambda () (fib 35))
                   #:pool 'own
                   #:keep 'results))
(time (= (thread-wait n1)
         (thread-wait n2)))
]

Whether parallel threads improve performance depends on the nature of
the computation. The kinds of computations that can run efficiently in
parallel are mostly the same ones that can run in a @tech{future}, and
@secref["effective-futures"] provides more information. The difference
with a parallel thread is that when a future would block, a parallel
thread instead synchronizes with other threads in the hope of reaching
a continuation afterward that is clear of blocking actions. Also,
parallel threads implement the same interface for synchronization as
@tech{coroutine threads}, including @racket[thread-wait],
@racket[thread-send], and @racket[kill-thread].
