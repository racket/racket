#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label racket/flonum racket/future future-visualizer))

@(define future-eval (make-base-eval))
@(interaction-eval #:eval future-eval (require racket/future 
                                               future-visualizer/private/visualizer-drawing 
                                               future-visualizer/trace))

@title[#:tag "effective-futures"]{Parallelism with Futures}

The @racketmodname[racket/future] library provides support for
performance improvement through parallelism with @deftech{futures} and the @racket[future]
and @racket[touch] functions. The level of parallelism available from
those constructs, however, is limited by several factors, and the
current implementation is best suited to numerical tasks.

@margin-note{Other functions, such as @racket[thread], support the
creation of reliably concurrent tasks. However, threads never run truly
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
notion of ``future safe'' is inherently tied to the
implementation. The distinction between ``future safe'' and ``future unsafe''
operations may be far from apparent at the level of a Racket program.
The remainder of this section works through an example to illustrate
this distinction and to show how to use the future visualizer
can help shed light on it.

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
              [(> (+ zrq ziq) 4) i]
              [else (loop (add1 i)
                          (+ (- zrq ziq) cr)
                          (+ (* 2 zr zi) ci))]))))))
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

To see why, use the @racketmodname[future-visualizer], like this:

@racketblock[ 
  (require future-visualizer) 
  (visualize-futures 
   (let ([f (future (lambda () (mandelbrot 10000000 62 501 1000)))])
     (list (mandelbrot 10000000 62 500 1000)
           (touch f))))] 
 
This opens a window showing a graphical view of a trace of the computation.
The upper-left portion of the window contains an execution timeline:

@(interaction-eval 
  #:eval future-eval 
  (define bad-log 
    (list (indexed-future-event 0 '#s(future-event #f 0 create 1334778390997.936 #f 1))
          (indexed-future-event 1 '#s(future-event 1 1 start-work 1334778390998.137 #f #f))
          (indexed-future-event 2 '#s(future-event 1 1 sync 1334778390998.145 #f #f))
          (indexed-future-event 3 '#s(future-event 1 0 sync 1334778391001.616 [allocate memory] #f))
          (indexed-future-event 4 '#s(future-event 1 0 result 1334778391001.629 #f #f))
          (indexed-future-event 5 '#s(future-event 1 1 result 1334778391001.643 #f #f))
          (indexed-future-event 6 '#s(future-event 1 1 block 1334778391001.653 #f #f))
          (indexed-future-event 7 '#s(future-event 1 1 suspend 1334778391001.658 #f #f))
          (indexed-future-event 8 '#s(future-event 1 1 end-work 1334778391001.658 #f #f))
          (indexed-future-event 9 '#s(future-event 1 0 block 1334778392134.226 > #f))
          (indexed-future-event 10 '#s(future-event 1 0 result 1334778392134.241 #f #f))
          (indexed-future-event 11 '#s(future-event 1 1 start-work 1334778392134.254 #f #f))
          (indexed-future-event 12 '#s(future-event 1 1 sync 1334778392134.339 #f #f))
          (indexed-future-event 13 '#s(future-event 1 0 sync 1334778392134.375 [allocate memory] #f))
          (indexed-future-event 14 '#s(future-event 1 0 result 1334778392134.38 #f #f))
          (indexed-future-event 15 '#s(future-event 1 1 result 1334778392134.387 #f #f))
          (indexed-future-event 16 '#s(future-event 1 1 block 1334778392134.39 #f #f))
          (indexed-future-event 17 '#s(future-event 1 1 suspend 1334778392134.391 #f #f))
          (indexed-future-event 18 '#s(future-event 1 1 end-work 1334778392134.391 #f #f))
          (indexed-future-event 19 '#s(future-event 1 0 touch-pause 1334778392134.432 #f #f))
          (indexed-future-event 20 '#s(future-event 1 0 touch-resume 1334778392134.433 #f #f))
          (indexed-future-event 21 '#s(future-event 1 0 block 1334778392134.533 * #f))
          (indexed-future-event 22 '#s(future-event 1 0 result 1334778392134.537 #f #f))
          (indexed-future-event 23 '#s(future-event 1 2 start-work 1334778392134.568 #f #f))
          (indexed-future-event 24 '#s(future-event 1 2 sync 1334778392134.57 #f #f))
          (indexed-future-event 25 '#s(future-event 1 0 touch-pause 1334778392134.587 #f #f))
          (indexed-future-event 26 '#s(future-event 1 0 touch-resume 1334778392134.587 #f #f))
          (indexed-future-event 27 '#s(future-event 1 0 block 1334778392134.6 [allocate memory] #f))
          (indexed-future-event 28 '#s(future-event 1 0 result 1334778392134.604 #f #f))
          (indexed-future-event 29 '#s(future-event 1 2 result 1334778392134.627 #f #f))
          (indexed-future-event 30 '#s(future-event 1 2 block 1334778392134.629 #f #f))
          (indexed-future-event 31 '#s(future-event 1 2 suspend 1334778392134.632 #f #f))
          (indexed-future-event 32 '#s(future-event 1 2 end-work 1334778392134.633 #f #f))
          (indexed-future-event 33 '#s(future-event 1 0 touch-pause 1334778392134.64 #f #f))
          (indexed-future-event 34 '#s(future-event 1 0 touch-resume 1334778392134.64 #f #f))
          (indexed-future-event 35 '#s(future-event 1 0 block 1334778392134.663 > #f))
          (indexed-future-event 36 '#s(future-event 1 0 result 1334778392134.666 #f #f))
          (indexed-future-event 37 '#s(future-event 1 1 start-work 1334778392134.673 #f #f))
          (indexed-future-event 38 '#s(future-event 1 1 block 1334778392134.676 #f #f))
          (indexed-future-event 39 '#s(future-event 1 1 suspend 1334778392134.677 #f #f))
          (indexed-future-event 40 '#s(future-event 1 1 end-work 1334778392134.677 #f #f))
          (indexed-future-event 41 '#s(future-event 1 0 touch-pause 1334778392134.704 #f #f))
          (indexed-future-event 42 '#s(future-event 1 0 touch-resume 1334778392134.704 #f #f))
          (indexed-future-event 43 '#s(future-event 1 0 block 1334778392134.727 * #f))
          (indexed-future-event 44 '#s(future-event 1 0 result 1334778392134.73 #f #f))
          (indexed-future-event 45 '#s(future-event 1 2 start-work 1334778392134.737 #f #f))
          (indexed-future-event 46 '#s(future-event 1 2 block 1334778392134.739 #f #f))
          (indexed-future-event 47 '#s(future-event 1 2 suspend 1334778392134.74 #f #f))
          (indexed-future-event 48 '#s(future-event 1 2 end-work 1334778392134.741 #f #f))
          (indexed-future-event 49 '#s(future-event 1 0 touch-pause 1334778392134.767 #f #f))
          (indexed-future-event 50 '#s(future-event 1 0 touch-resume 1334778392134.767 #f #f))
          (indexed-future-event 51 '#s(future-event 1 0 block 1334778392134.79 > #f))
          (indexed-future-event 52 '#s(future-event 1 0 result 1334778392134.793 #f #f))
          (indexed-future-event 53 '#s(future-event 1 1 start-work 1334778392134.799 #f #f))
          (indexed-future-event 54 '#s(future-event 1 1 block 1334778392134.801 #f #f))
          (indexed-future-event 55 '#s(future-event 1 1 suspend 1334778392134.802 #f #f))
          (indexed-future-event 56 '#s(future-event 1 1 end-work 1334778392134.803 #f #f))
          (indexed-future-event 57 '#s(future-event 1 0 touch-pause 1334778392134.832 #f #f))
          (indexed-future-event 58 '#s(future-event 1 0 touch-resume 1334778392134.832 #f #f))
          (indexed-future-event 59 '#s(future-event 1 0 block 1334778392134.854 * #f))
          (indexed-future-event 60 '#s(future-event 1 0 result 1334778392134.858 #f #f))
          (indexed-future-event 61 '#s(future-event 1 2 start-work 1334778392134.864 #f #f))
          (indexed-future-event 62 '#s(future-event 1 2 block 1334778392134.876 #f #f))
          (indexed-future-event 63 '#s(future-event 1 2 suspend 1334778392134.877 #f #f))
          (indexed-future-event 64 '#s(future-event 1 2 end-work 1334778392134.882 #f #f))
          (indexed-future-event 65 '#s(future-event 1 0 touch-pause 1334778392134.918 #f #f))
          (indexed-future-event 66 '#s(future-event 1 0 touch-resume 1334778392134.918 #f #f))
          (indexed-future-event 67 '#s(future-event 1 0 block 1334778392134.94 > #f))
          (indexed-future-event 68 '#s(future-event 1 0 result 1334778392134.943 #f #f))
          (indexed-future-event 69 '#s(future-event 1 1 start-work 1334778392134.949 #f #f))
          (indexed-future-event 70 '#s(future-event 1 1 block 1334778392134.952 #f #f))
          (indexed-future-event 71 '#s(future-event 1 1 suspend 1334778392134.953 #f #f))
          (indexed-future-event 72 '#s(future-event 1 1 end-work 1334778392134.96 #f #f))
          (indexed-future-event 73 '#s(future-event 1 0 touch-pause 1334778392134.991 #f #f))
          (indexed-future-event 74 '#s(future-event 1 0 touch-resume 1334778392134.991 #f #f))
          (indexed-future-event 75 '#s(future-event 1 0 block 1334778392135.013 * #f))
          (indexed-future-event 76 '#s(future-event 1 0 result 1334778392135.016 #f #f))
          (indexed-future-event 77 '#s(future-event 1 2 start-work 1334778392135.027 #f #f))
          (indexed-future-event 78 '#s(future-event 1 2 block 1334778392135.033 #f #f))
          (indexed-future-event 79 '#s(future-event 1 2 suspend 1334778392135.034 #f #f))
          (indexed-future-event 80 '#s(future-event 1 2 end-work 1334778392135.04 #f #f))
          (indexed-future-event 81 '#s(future-event 1 0 touch-pause 1334778392135.075 #f #f))
          (indexed-future-event 82 '#s(future-event 1 0 touch-resume 1334778392135.075 #f #f))
          (indexed-future-event 83 '#s(future-event 1 0 block 1334778392135.098 > #f))
          (indexed-future-event 84 '#s(future-event 1 0 result 1334778392135.101 #f #f))
          (indexed-future-event 85 '#s(future-event 1 1 start-work 1334778392135.107 #f #f))
          (indexed-future-event 86 '#s(future-event 1 1 block 1334778392135.117 #f #f))
          (indexed-future-event 87 '#s(future-event 1 1 suspend 1334778392135.118 #f #f))
          (indexed-future-event 88 '#s(future-event 1 1 end-work 1334778392135.123 #f #f))
          (indexed-future-event 89 '#s(future-event 1 0 touch-pause 1334778392135.159 #f #f))
          (indexed-future-event 90 '#s(future-event 1 0 touch-resume 1334778392135.159 #f #f))
          (indexed-future-event 91 '#s(future-event 1 0 block 1334778392135.181 * #f))
          (indexed-future-event 92 '#s(future-event 1 0 result 1334778392135.184 #f #f))
          (indexed-future-event 93 '#s(future-event 1 2 start-work 1334778392135.19 #f #f))
          (indexed-future-event 94 '#s(future-event 1 2 block 1334778392135.191 #f #f))
          (indexed-future-event 95 '#s(future-event 1 2 suspend 1334778392135.192 #f #f))
          (indexed-future-event 96 '#s(future-event 1 2 end-work 1334778392135.192 #f #f))
          (indexed-future-event 97 '#s(future-event 1 0 touch-pause 1334778392135.221 #f #f))
          (indexed-future-event 98 '#s(future-event 1 0 touch-resume 1334778392135.221 #f #f))
          (indexed-future-event 99 '#s(future-event 1 0 block 1334778392135.243 > #f))
          )))
                   
@interaction-eval-show[
    #:eval future-eval 
           (timeline-pict bad-log 
                          #:x 0 
                          #:y 0 
                          #:width 600 
                          #:height 300) 
]

Each horizontal row represents an OS-level thread, and the colored 
dots represent important events in the execution of the program (they are 
color-coded to distinguish one event type from another).  The upper-left blue 
dot in the timeline represents the future's creation.  The future 
executes for a brief period (represented by a green bar in the second line) on thread 
1, and then pauses to allow the runtime thread to perform a future-unsafe operation.

In the Racket implementation, future-unsafe operations fall into one of two categories. 
A @deftech{blocking} operation halts the evaluation of the future, and will not allow 
it to continue until it is touched.  After the operation completes within @racket[touch], 
the remainder of the future's work will be evaluated sequentially by the runtime 
thread.  A @deftech{synchronized} operation also halts the future, but the runtime thread 
may perform the operation at any time and, once completed, the future may continue 
running in parallel.  Memory allocation and JIT compilation are two common examples 
of synchronized operations.

In the timeline, we see an orange dot just to the right of the green bar on thread 1 -- 
this dot represents a synchronized operation (memory allocation).  The first orange 
dot on thread 0 shows that the runtime thread performed the allocation shortly after 
the future paused.  A short time later, the future halts on a blocking operation 
(the first red dot) and must wait until the @racket[touch] for it to be evaluated 
(slightly after the 1049ms mark).

When you move your mouse over an event, the visualizer shows you 
detailed information about the event and draws arrows
connecting all of the events in the corresponding future.
This image shows those connections for our future.

@interaction-eval-show[
     #:eval future-eval 
            (timeline-pict bad-log 
                           #:x 0
                           #:y 0 
                           #:width 600 
                           #:height 300 
                           #:selected-event-index 6)
]

The dotted orange line connects the first event in the future to
the future that created it, and the purple lines connect adjacent
events within the future. 

The reason that we see no parallelism is that the @racket[<] and @racket[*] operations 
in the lower portion of the loop in @racket[mandelbrot] involve a mixture of 
floating-point and fixed (integer) values.  Such mixtures typically trigger a slow 
path in execution, and the general slow path will usually be blocking.

Changing constants to be floating-points numbers in @racket[mandelbrot] addresses that 
first problem: 

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

With that change, @racket[mandelbrot] computations can run in 
parallel.  Nevertheless, we still see a special type of 
slow-path operation limiting our parallelism (orange dots):

@interaction-eval[
    #:eval future-eval            
    (define better-log 
      (list (indexed-future-event 0 '#s(future-event #f 0 create 1334779296782.22 #f 2))
            (indexed-future-event 1 '#s(future-event 2 2 start-work 1334779296782.265 #f #f))
            (indexed-future-event 2 '#s(future-event 2 2 sync 1334779296782.378 #f #f))
            (indexed-future-event 3 '#s(future-event 2 0 sync 1334779296795.582 [allocate memory] #f))
            (indexed-future-event 4 '#s(future-event 2 0 result 1334779296795.587 #f #f))
            (indexed-future-event 5 '#s(future-event 2 2 result 1334779296795.6 #f #f))
            (indexed-future-event 6 '#s(future-event 2 2 sync 1334779296795.689 #f #f))
            (indexed-future-event 7 '#s(future-event 2 0 sync 1334779296795.807 [allocate memory] #f))
            (indexed-future-event 8 '#s(future-event 2 0 result 1334779296795.812 #f #f))
            (indexed-future-event 9 '#s(future-event 2 2 result 1334779296795.818 #f #f))
            (indexed-future-event 10 '#s(future-event 2 2 sync 1334779296795.827 #f #f))
            (indexed-future-event 11 '#s(future-event 2 0 sync 1334779296806.627 [allocate memory] #f))
            (indexed-future-event 12 '#s(future-event 2 0 result 1334779296806.635 #f #f))
            (indexed-future-event 13 '#s(future-event 2 2 result 1334779296806.646 #f #f))
            (indexed-future-event 14 '#s(future-event 2 2 sync 1334779296806.879 #f #f))
            (indexed-future-event 15 '#s(future-event 2 0 sync 1334779296806.994 [allocate memory] #f))
            (indexed-future-event 16 '#s(future-event 2 0 result 1334779296806.999 #f #f))
            (indexed-future-event 17 '#s(future-event 2 2 result 1334779296807.007 #f #f))
            (indexed-future-event 18 '#s(future-event 2 2 sync 1334779296807.023 #f #f))
            (indexed-future-event 19 '#s(future-event 2 0 sync 1334779296814.198 [allocate memory] #f))
            (indexed-future-event 20 '#s(future-event 2 0 result 1334779296814.206 #f #f))
            (indexed-future-event 21 '#s(future-event 2 2 result 1334779296814.221 #f #f))
            (indexed-future-event 22 '#s(future-event 2 2 sync 1334779296814.29 #f #f))
            (indexed-future-event 23 '#s(future-event 2 0 sync 1334779296820.796 [allocate memory] #f))
            (indexed-future-event 24 '#s(future-event 2 0 result 1334779296820.81 #f #f))
            (indexed-future-event 25 '#s(future-event 2 2 result 1334779296820.835 #f #f))
            (indexed-future-event 26 '#s(future-event 2 2 sync 1334779296821.089 #f #f))
            (indexed-future-event 27 '#s(future-event 2 0 sync 1334779296825.217 [allocate memory] #f))
            (indexed-future-event 28 '#s(future-event 2 0 result 1334779296825.226 #f #f))
            (indexed-future-event 29 '#s(future-event 2 2 result 1334779296825.242 #f #f))
            (indexed-future-event 30 '#s(future-event 2 2 sync 1334779296825.305 #f #f))
            (indexed-future-event 31 '#s(future-event 2 0 sync 1334779296832.541 [allocate memory] #f))
            (indexed-future-event 32 '#s(future-event 2 0 result 1334779296832.549 #f #f))
            (indexed-future-event 33 '#s(future-event 2 2 result 1334779296832.562 #f #f))
            (indexed-future-event 34 '#s(future-event 2 2 sync 1334779296832.667 #f #f))
            (indexed-future-event 35 '#s(future-event 2 0 sync 1334779296836.269 [allocate memory] #f))
            (indexed-future-event 36 '#s(future-event 2 0 result 1334779296836.278 #f #f))
            (indexed-future-event 37 '#s(future-event 2 2 result 1334779296836.326 #f #f))
            (indexed-future-event 38 '#s(future-event 2 2 sync 1334779296836.396 #f #f))
            (indexed-future-event 39 '#s(future-event 2 0 sync 1334779296843.481 [allocate memory] #f))
            (indexed-future-event 40 '#s(future-event 2 0 result 1334779296843.49 #f #f))
            (indexed-future-event 41 '#s(future-event 2 2 result 1334779296843.501 #f #f))
            (indexed-future-event 42 '#s(future-event 2 2 sync 1334779296843.807 #f #f))
            (indexed-future-event 43 '#s(future-event 2 0 sync 1334779296847.291 [allocate memory] #f))
            (indexed-future-event 44 '#s(future-event 2 0 result 1334779296847.3 #f #f))
            (indexed-future-event 45 '#s(future-event 2 2 result 1334779296847.312 #f #f))
            (indexed-future-event 46 '#s(future-event 2 2 sync 1334779296847.375 #f #f))
            (indexed-future-event 47 '#s(future-event 2 0 sync 1334779296854.487 [allocate memory] #f))
            (indexed-future-event 48 '#s(future-event 2 0 result 1334779296854.495 #f #f))
            (indexed-future-event 49 '#s(future-event 2 2 result 1334779296854.507 #f #f))
            (indexed-future-event 50 '#s(future-event 2 2 sync 1334779296854.656 #f #f))
            (indexed-future-event 51 '#s(future-event 2 0 sync 1334779296857.374 [allocate memory] #f))
            (indexed-future-event 52 '#s(future-event 2 0 result 1334779296857.383 #f #f))
            (indexed-future-event 53 '#s(future-event 2 2 result 1334779296857.421 #f #f))
            (indexed-future-event 54 '#s(future-event 2 2 sync 1334779296857.488 #f #f))
            (indexed-future-event 55 '#s(future-event 2 0 sync 1334779296869.919 [allocate memory] #f))
            (indexed-future-event 56 '#s(future-event 2 0 result 1334779296869.947 #f #f))
            (indexed-future-event 57 '#s(future-event 2 2 result 1334779296869.981 #f #f))
            (indexed-future-event 58 '#s(future-event 2 2 sync 1334779296870.32 #f #f))
            (indexed-future-event 59 '#s(future-event 2 0 sync 1334779296879.438 [allocate memory] #f))
            (indexed-future-event 60 '#s(future-event 2 0 result 1334779296879.446 #f #f))
            (indexed-future-event 61 '#s(future-event 2 2 result 1334779296879.463 #f #f))
            (indexed-future-event 62 '#s(future-event 2 2 sync 1334779296879.526 #f #f))
            (indexed-future-event 63 '#s(future-event 2 0 sync 1334779296882.928 [allocate memory] #f))
            (indexed-future-event 64 '#s(future-event 2 0 result 1334779296882.935 #f #f))
            (indexed-future-event 65 '#s(future-event 2 2 result 1334779296882.944 #f #f))
            (indexed-future-event 66 '#s(future-event 2 2 sync 1334779296883.311 #f #f))
            (indexed-future-event 67 '#s(future-event 2 0 sync 1334779296890.471 [allocate memory] #f))
            (indexed-future-event 68 '#s(future-event 2 0 result 1334779296890.479 #f #f))
            (indexed-future-event 69 '#s(future-event 2 2 result 1334779296890.517 #f #f))
            (indexed-future-event 70 '#s(future-event 2 2 sync 1334779296890.581 #f #f))
            (indexed-future-event 71 '#s(future-event 2 0 sync 1334779296894.362 [allocate memory] #f))
            (indexed-future-event 72 '#s(future-event 2 0 result 1334779296894.369 #f #f))
            (indexed-future-event 73 '#s(future-event 2 2 result 1334779296894.382 #f #f))
            (indexed-future-event 74 '#s(future-event 2 2 sync 1334779296894.769 #f #f))
            (indexed-future-event 75 '#s(future-event 2 0 sync 1334779296901.501 [allocate memory] #f))
            (indexed-future-event 76 '#s(future-event 2 0 result 1334779296901.51 #f #f))
            (indexed-future-event 77 '#s(future-event 2 2 result 1334779296901.556 #f #f))
            (indexed-future-event 78 '#s(future-event 2 2 sync 1334779296901.62 #f #f))
            (indexed-future-event 79 '#s(future-event 2 0 sync 1334779296905.428 [allocate memory] #f))
            (indexed-future-event 80 '#s(future-event 2 0 result 1334779296905.434 #f #f))
            (indexed-future-event 81 '#s(future-event 2 2 result 1334779296905.447 #f #f))
            (indexed-future-event 82 '#s(future-event 2 2 sync 1334779296905.743 #f #f))
            (indexed-future-event 83 '#s(future-event 2 0 sync 1334779296912.538 [allocate memory] #f))
            (indexed-future-event 84 '#s(future-event 2 0 result 1334779296912.547 #f #f))
            (indexed-future-event 85 '#s(future-event 2 2 result 1334779296912.564 #f #f))
            (indexed-future-event 86 '#s(future-event 2 2 sync 1334779296912.625 #f #f))
            (indexed-future-event 87 '#s(future-event 2 0 sync 1334779296916.094 [allocate memory] #f))
            (indexed-future-event 88 '#s(future-event 2 0 result 1334779296916.1 #f #f))
            (indexed-future-event 89 '#s(future-event 2 2 result 1334779296916.108 #f #f))
            (indexed-future-event 90 '#s(future-event 2 2 sync 1334779296916.243 #f #f))
            (indexed-future-event 91 '#s(future-event 2 0 sync 1334779296927.233 [allocate memory] #f))
            (indexed-future-event 92 '#s(future-event 2 0 result 1334779296927.242 #f #f))
            (indexed-future-event 93 '#s(future-event 2 2 result 1334779296927.262 #f #f))
            (indexed-future-event 94 '#s(future-event 2 2 sync 1334779296927.59 #f #f))
            (indexed-future-event 95 '#s(future-event 2 0 sync 1334779296934.603 [allocate memory] #f))
            (indexed-future-event 96 '#s(future-event 2 0 result 1334779296934.612 #f #f))
            (indexed-future-event 97 '#s(future-event 2 2 result 1334779296934.655 #f #f))
            (indexed-future-event 98 '#s(future-event 2 2 sync 1334779296934.72 #f #f))
            (indexed-future-event 99 '#s(future-event 2 0 sync 1334779296938.773 [allocate memory] #f))
            ))
]

@interaction-eval-show[
    #:eval future-eval 
           (timeline-pict better-log 
                          #:x 0 
                          #:y 0 
                          #:width 600 
                          #:height 300)
]

The problem is that most every arithmetic operation in this example 
produces an inexact number whose storage must be allocated.  While some allocation 
can safely be performed exclusively without the aid of the runtime thread, especially 
frequent allocation requires synchronized operations which defeat any performance 
improvement.

By using @tech{flonum}-specific operations (see
@secref["fixnums+flonums"]), we can re-write @racket[mandelbrot] to use
much less allocation:

@interaction-eval[
    #:eval future-eval 
    (define good-log 
      (list (indexed-future-event 0 '#s(future-event #f 0 create 1334778395768.733 #f 3))
            (indexed-future-event 1 '#s(future-event 3 2 start-work 1334778395768.771 #f #f))
            (indexed-future-event 2 '#s(future-event 3 2 complete 1334778395864.648 #f #f))
            (indexed-future-event 3 '#s(future-event 3 2 end-work 1334778395864.652 #f #f))
            ))
]

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
Executing this program yields the following in the visualizer: 

@interaction-eval-show[
    #:eval future-eval 
           (timeline-pict good-log 
                          #:x 0 
                          #:y 0 
                          #:width 600 
                          #:height 300)
]

Notice that only one green bar is shown here because one of the 
mandelbrot computations is not being evaluated by a future (on 
the runtime thread).

As a general guideline, any operation that is inlined by the
@tech{JIT} compiler runs safely in parallel, while other operations
that are not inlined (including all operations if the JIT compiler is
disabled) are considered unsafe. The @exec{raco decompile} tool
annotates operations that can be inlined by the compiler (see
@secref[#:doc '(lib "scribblings/raco/raco.scrbl") "decompile"]), so the
decompiler can be used to help predict parallel performance.


@close-eval[future-eval]
