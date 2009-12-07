#lang scribble/doc

@title{@bold{Futures}: Fine-grained Parallelism}

@; ----------------------------------------------------------------------

@(require scribble/manual
          scribble/urls
          scribble/struct
          (for-label scheme
                     scheme/base
                     scheme/contract
                     scheme/future))

@; ----------------------------------------------------------------------

The PLT futures API enables the development of parallel programs which
take advantage of machines with multiple processors, cores, or
hardware threads.

@defmodule[scheme/future]{}

@defproc[(future [thunk (-> any)]) future?]{
  Starts running @scheme[thunk] in parallel.  The @scheme[future]
  procedure returns immediately with a future descriptor value.
}

@defproc[(touch [f future?]) any]{
  Returns the value computed in the future @scheme[f], blocking until
  the future completes (if it has not already completed).
}

@defproc[(future? [x any/c]) boolean?]{
  Returns @scheme[#t] if @scheme[x] is a future.
}

@defproc[(processor-count) exact-positive-integer?]{
  Returns the number of processors/cores/hardware threads available on
  the current system.
}

@section[#:tag "besteffortpar"]{Best-Effort Parallelism}

The @scheme[future] API represents a best-effort attempt to execute an
arbitrary segment of code in parallel.  When designing programs and
algorithms which leverage @scheme[future] for parallel speedup, there
are a number of performance considerations to be aware of.

Futures are designed to accommodate the fact that many low-level
functions provided by the MzScheme virtual machine are not reentrant.
Thus, a future will execute its work in parallel until it detects an
attempt to perform an ``unsafe'' operation (e.g. invoking a
non-reentrant function).  When such an operation is detected, the
future will block until @scheme[touch]ed, upon which the remainder of
its work will be done sequentially with respect to the touching
thread (in this case, ``thread'' refers to an OS thread).

To guarantee that unsafe operations never execute simultaneously, only
the initial OS thread used to start the MzScheme virtual machine (the
``runtime thread'') is allowed to execute them.  If a parallel future
detects an attempted unsafe operation, it will signal the runtime
thread that pending unsafe work is available, then block, waiting for
the runtime thread to complete it.  Note that as mentioned above, the
runtime thread will not attempt to do this work until the future is
explicitly touched.  Also note that calls to @scheme[future] and
@scheme[touch] are themselves considered unsafe operations.

Consider the following contrived example:

@schemeblock[
  (define (add-in-parallel a b)
    (let ([f (future (lambda () (+ a b)))])
      (touch f)))

  (add-in-parallel 4 8)
]

The output of this program is, as expected:

@verbatim|{
  12
}|

Now suppose we add a print message to our function for debugging purposes:

@schemeblock[
  (define (add-in-parallel a b)
    (let ([f (future
              (lambda ()
                (begin
                  (printf "Adding ~a and ~a together!~n" a b)
                  (+ a b))))])
      (printf "About to touch my future...~n")
      (touch f)))

  (add-in-parallel 4 8)
]

Though this program still produces the same output, no work is being
done in parallel.  Because @scheme[printf] is considered an unsafe
operation, f will block, and the print invocation (along with the
subsequent add) will not be performed until the @scheme[touch] call.

@section[#:tag "logging"]{How Do I Keep Those Cores Busy?}

It is not always obvious when or where unsafe operations may
be causing unacceptable performance degradation in parallel programs.
A a general guideline, any primitive that is inlined will run in parallel.
For example, fixnum and flonum addition do run in parallel,
but not bignum or rational addition. Similarly, vector operations are
generally safe, but not continuation operations. Also, allocation can run
in parallel, as long as only a little bit of allocation happens. Once a significant
amount of allocation happens, a parallel thread has to rendez-vous with the
runtime thread to get new, local memory.

To help tell what is happening in your program, the parallel threads
logs all of the points at which it has to synchronize
with the runtime thread.
For example, running the code in the previous
example in the debug log level produces the following output:

@verbatim|{
  About to touch my future...
  future: 0 waiting for runtime at 1259702453747.720947: printf
  Adding 4 and 8 together!
  12
}|

The message indicates which future blocked, the time it blocked and
the primitive operation that caused it to block.

To be sure we are not merely seeing the effects of a race condition in
this example, we can force the main thread to @scheme[sleep] for an
unreasonable amount of time:

@schemeblock[
  (define (add-in-parallel a b)
    (let ([f (future
              (lambda ()
                (begin
                  (printf "Adding ~a and ~a together!~n" a b)
                  (+ a b))))])
      (sleep 10.0)
      (printf "About to touch my future...~n")
      (touch f)))

  (add-in-parallel 4 8)
]

@verbatim|{
  About to touch my future...
  future: 0 waiting for runtime at 1259702453747.720947: printf
  Adding 4 and 8 together!
  12
}|

@section[#:tag "compiling"]{Enabling Futures in MzScheme Builds}

PLT's parallel-future support is only enabled if you pass
@DFlag{enable-futures} to @exec{configure} when you build PLT (and
that build currently only works with @exec{mzscheme}, not with
@exec{mred}). When parallel-future support is not enabled,
@scheme[future] just remembers the given thunk to call sequentially on
a later @scheme[touch].
