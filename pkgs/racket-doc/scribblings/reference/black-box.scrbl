#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "black-box"]{Black-Box Procedure}

As Racket programs are compiled (see @secref["compiler"]), the
compiler may reorder or even remove @defterm{pure} computations that
have no visible effect. For compilation purposes, the time needed to
perform a computation is not considered a visible effect. The compiler
takes into account memory used by a computation, including values that
the computation keeps reachable, only to the degree that it will not
increase the asymptotic memory use of a program, but it may remove or
reorder computations in a way that reduces memory use. The
@racket[black-box] function inhibits many of these optimizations
without adding additional overhead.

@defproc[(black-box [v any/c]) any/c]{

Returns @racket[v].

As far as the Racket compiler is concerned, @racket[black-box] returns
an unknown value, and it has a side effect involving @racket[v], which
means that a call to @racket[black-box] or its argument cannot be
eliminated at compile time, and its evaluation cannot be reordered
with respect to other side effects.

@mz-examples[     
(let ([to-power 100])
  (let loop ([i 1000])
    (unless (zero? i)
      (code:comment "call to `expt` is optimized away entirely, since")
      (code:comment "there's no effect and the result is unused:")
      (expt 2 to-power)
      (loop (sub1 i)))))

(let ([to-power 100])
  (let loop ([i 1000])
    (unless (zero? i)
      (code:comment "call to `expt` is optimized to just returning a folded")
      (code:comment "constant, instead of calling `expt` each iteration:")
      (black-box (expt 2 to-power))
      (loop (sub1 i)))))

(let ([to-power (black-box 100)])
  (let loop ([i 1000])
    (unless (zero? i)
      (code:comment "in safe mode, calls `expt`, because `to-power` is not")
      (code:comment "known to be a number; optimized away in unsafe mode:")
      (expt 2 to-power)
      (loop (sub1 i)))))

(let ([to-power (black-box 100)])
  (let loop ([i 1000])
    (unless (zero? i)
      (code:comment "arithmetic really performed every iteration, since the")
      (code:comment "`to-power` value is assumed unknown, and the `expt`")
      (code:comment "result is assumed to be used, even in unsafe mode:")
      (black-box (expt 2 to-power))
      (loop (sub1 i)))))
]

@history[#:added "8.18.0.17"]}
