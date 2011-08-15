#lang scribble/doc
@(require scribble/manual "guide-utils.rkt"
          (for-label racket/flonum racket/future))

@title[#:tag "effective-futures"]{Parallelism with Futures}

The @racketmodname[racket/future] library provides support for
performance improvement through parallelism with the @racket[future]
and @racket[touch] functions. The level of parallelism available from
those constructs, however, is limited by several factors, and the
current implementation is best suited to numerical tasks.

@margin-note{Other functions, such as @racket[thread], support the
creation of reliably concurrent tasks. However, thread never run truly
in parallel, even if the hardware and operating system support
parallelism.}

As a starting example, the @racket[any-double?] function below takes a
list of numbers and determines whether any number in the list has a
double that is also in the list:

@racketblock[
(define (any-double? l)
  (for/or ([i (in-list l)])
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))
]

This function runs in quadratic time, so it can take a long time (on
the order of a second) on large lists like @racket[l1] and
@racket[l2]:

@racketblock[
(define l1 (for/list ([i (in-range 5000)]) 
             (+ (* 2 i) 1)))
(define l2 (for/list ([i (in-range 5000)]) 
             (- (* 2 i) 1)))
(or (any-double? l1)
    (any-double? l2))
]

The best way to speed up @racket[any-double?]  is to use a different
algorithm. However, on a machine that offers at least two processing
units, the example above can run in about half the time using
@racket[future] and @racket[touch]:

@racketblock[
(let ([f (future (lambda () (any-double? l2)))])
  (or (any-double? l1)
      (touch f)))
]

The future @racket[f] runs @racket[(any-double? l2)] in parallel to
@racket[(any-double? l1)], and the result for @racket[(any-double?
l2)] becomes available about the same time that it is demanded by
@racket[(touch f)].

Futures run in parallel as long as they can do so safely, but the
notion of ``safe'' for parallelism is inherently tied to the system
implementation. The distinction between ``safe'' and ``unsafe''
operations may be far from apparent at the level of a Racket program.

Consider the following core of a Mandelbrot-set computation:

@racketblock[
(define (mandelbrot iterations x y n)
  (let ([ci (- (/ (* 2.0 y) n) 1.0)]
        [cr (- (/ (* 2.0 x) n) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (* zr zr)]
                [ziq (* zi zi)])
            (cond
              [(> (+ zrq ziq) 4.0) i]
              [else (loop (add1 i)
                          (+ (- zrq ziq) cr)
                          (+ (* 2.0 zr zi) ci))]))))))
]

The expressions @racket[(mandelbrot 10000000 62 500 1000)] and
@racket[(mandelbrot 10000000 62 501 1000)] each take a while to
produce an answer. Computing them both, of course, takes twice as
long:

@racketblock[
(list (mandelbrot 10000000 62 500 1000)
      (mandelbrot 10000000 62 501 1000))
]

Unfortunately, attempting to run the two computations in parallel with
@racket[future] does not improve performance:

@racketblock[
 (let ([f (future (lambda () (mandelbrot 10000000 62 501 1000)))])
   (list (mandelbrot 10000000 62 500 1000)
         (touch f)))
]

One problem is that the @racket[*] and @racket[/] operations in the
first two lines of @racket[mandelbrot] involve a mixture of exact and
inexact real numbers. Such mixtures typically trigger a slow path in
execution, and the general slow path is not safe for
parallelism. Consequently, the future created in this example is
almost immediately suspended, and it cannot resume until
@racket[touch] is called.

Changing the first two lines of @racket[mandelbrot] addresses that
first the problem:

@racketblock[
(define (mandelbrot iterations x y n)
  (let ([ci (- (/ (* 2.0 (->fl y)) (->fl n)) 1.0)]
        [cr (- (/ (* 2.0 (->fl x)) (->fl n)) 1.5)])
    ....))
]

With that change, @racket[mandelbrot] computations can run in
parallel. Nevertheless, performance still does not improve. The
problem is that most every arithmetic operation in this example
produces an inexact number whose storage must be allocated. Especially
frequent allocation triggers communication between parallel tasks that
defeats any performance improvement.

By using @tech{flonum}-specific operations (see
@secref["fixnums+flonums"]), we can re-write @racket[mandelbot] to use
much less allocation:

@racketblock[
(define (mandelbrot iterations x y n)
  (let ([ci (fl- (fl/ (* 2.0 (->fl y)) (->fl n)) 1.0)]
        [cr (fl- (fl/ (* 2.0 (->fl x)) (->fl n)) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (fl* zr zr)]
                [ziq (fl* zi zi)])
            (cond
              [(fl> (fl+ zrq ziq) 4.0) i]
              [else (loop (add1 i)
                          (fl+ (fl- zrq ziq) cr)
                          (fl+ (fl* 2.0 (fl* zr zi)) ci))]))))))
]

This conversion can speed @racket[mandelbrot] by a factor of 8, even
in sequential mode, but avoiding allocation also allows
@racket[mandelbrot] to run usefully faster in parallel.

As a general guideline, any operation that is inlined by the
@tech{JIT} compiler runs safely in parallel, while other operations
that are not inlined (including all operations if the JIT compiler is
disabled) are considered unsafe. The @exec{mzc} decompiler tool
annotates operations that can be inlined by the compiler (see
@secref[#:doc '(lib "scribblings/raco/raco.scrbl") "decompile"]), so the
decompiler can be used to help predict parallel performance.

To more directly report what is happening in a program that uses
@racket[future] and @racket[touch], operations are logged when they
suspend a computation or synchronize with the main computation.  For
example, running the original @racket[mandelbrot] in a future produces
the following output in the @racket['debug] log level:

@margin-note{To see @racket['debug] logging output on stderr, set the
@envvar{PLTSTDERR} environment variable to @tt{debug} or start
@exec{racket} with @Flag{W} @tt{debug}.}

@verbatim[#:indent 2]|{
  future 1, process 1: BLOCKING on process 0; time: ....
  ....
  future 1, process 0: HANDLING: *; time: ....
}|

The messages indicate which internal future-running task became
blocked on an unsafe operation, the time it blocked (in terms of
@racket[current-inexact-miliseconds]), and the operation that caused
the computation it to block.

The first revision to @racket[mandelbrot] avoids suspending at
@racket[*], but produces many log entries of the form

@verbatim[#:indent 2]|{
  future 1, process 0: synchronizing: [allocate memory]; time: ....
}|

The @tt{[allocate memory]} part of the message indicates that
synchronization was needed for memory allocation.
