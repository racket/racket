#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "parallel-threads"]{Parallel Threads}

A @tech{thread} that is created with a @deftech{parallel thread pool}
can use a different hardware processor than other threads, providing
parallelism in addition to concurrency. When hardware is available,
using these @deftech{parallel threads} can reduce the latency of a
computation.

@margin-note{A trade-off with parallel threads is that a computation
may instead get slower, instead of faster, due to overhead creating
and managing parallel tasks.}

@margin-note{The @tech{BC} implementation of Racket treats all threads as
@tech{coroutine threads} and does not run them in parallel.}

Create a parallel thread using
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
@secref["effective-futures"] provides more information and describes
how the @tech{futures visualizer} can help explain a program's
performance. The difference with a parallel thread is that when a
future would block, a parallel thread instead synchronizes enough with
other threads to continue---in the hope of quickly reaching a point
that is clear of blocking actions. Also, a parallel thread can freely
use @tech{parameters} and exception handlers, which are limited in
futures until a @racket[touch] provides a full dynamic context to the
future. Finally, parallel threads implement the same interface for
synchronization as @tech{coroutine threads}, including
@racket[thread-wait], @racket[thread-send], and @racket[kill-thread].

Racket's predefined constructs, such as input and output ports or
mutable hash tables, are thread-safe and use locks internally as
needed. Threads can perform some input and output operations in
parallel, especially on regular files that never block. Meanwhile,
blocking input and output operations not only interfere with
parallelism, they can be significantly more expensive for parallel
threads than for coroutine threads, because extra synchronization is
required. Operations that interact with thread scheduling, such
@racket[sleep], @racket[sync], @racket[thread], @racket[thread-wait],
@racket[kill-thread], and @racket[custodian-shutdown-all], all require
synchronization that can be more expensive when triggered from a
parallel thread.

The @racket[semaphore-post] and @racket[semaphore-wait] operations can
be inexpensive in a parallel thread as long as no thread ends up
blocking on the semaphore. When a thread is blocked on a semaphore,
then operations on the semaphore interact with thread scheduling and
are therefore slower in parallel threads. In some cases, using a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{future
semaphore} (which is more limited than a normal semaphore) can provide
better performance for parallel threads.
