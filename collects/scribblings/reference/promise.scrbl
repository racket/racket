#lang scribble/doc
@(require "mz.ss"
          (for-label racket/promise))

@title{Delayed Evaluation}

@note-lib[racket/promise]

A @deftech{promise} encapsulates an expression to be evaluated on
demand via @scheme[force]. After a promise has been @scheme[force]d,
every later @scheme[force] of the promise produces the same result.

This module provides this functionality, and extends it to additional
kinds of promises with various evaluation strategies.


@defproc[(promise? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a promise, @scheme[#f]
otherwise.}


@defform[(delay body ...+)]{

Creates a promise that, when @scheme[force]d, evaluates the
@scheme[body]s to produce its value.  The result is then cached, so
further uses of @scheme[force] produce the cached value immediately.
This includes multiple values and exceptions.}


@defform[(lazy body ...+)]{

Like @scheme[delay], if the last @scheme[body] produces a promise when
forced, then this promise is @scheme[force]d too to obtain a value.
In other words, this form creates a composable promise, where the
computation of its body is ``attached'' to the computation of the
following promise and a single @scheme[force] iterates through the
whole chain, tail-calling each step.

Note that the last @scheme[body] of this form must produce a single
value --- but this value can itself be a @scheme[delay] promise that
returns multiple values.

This form useful for implementing lazy libraries and languages, where
tail-calls can be wrapped in a promise.}


@defproc[(force [v any/c]) any]{

If @scheme[v] is a promise, then the promise is forced to obtain a
value. If the promise has not been forced before, then the result is
recorded in the promise so that future @scheme[force]s on the promise
produce the same value (or values). If forcing the promise raises an
exception, then the exception is similarly recorded so that forcing
the promise will raise the same exception every time.

If @scheme[v] is @scheme[force]d again before the original call to
@scheme[force] returns, then the @exnraise[exn:fail].

Additional kinds of promises are also forced via @scheme[force].  See
below for further details.

If @scheme[v] is not a promise, then it is returned as the result.}


@defproc[(promise-forced? [promise promise?]) boolean?]{

Returns @scheme[#t] if @scheme[promise] has been forced.}


@defproc[(promise-running? [promise promise?]) boolean?]{

Returns @scheme[#t] if @scheme[promise] is currently being forced.
(Note that a promise can be either running or forced but not both.)}


@section{Additional Promise Kinds}

@defform[(delay/name body ...+)]{

Creates a ``call by name'' promise, that is similar to
@scheme[delay]-promises, except that the resulting value is not
cached.  It is essentially a thunk, wrapped in a way that
@scheme[force] recognizes.  Note that if a @scheme[delay/name] promise
forces itself, no exception is raised.
@; TODO: clarify that the point is that code that is written using
@; `force', can be used with these promises too.

Note that this promise is never considered ``running'' or ``forced''
in the sense of @scheme[promise-running?] and
@scheme[promise-forced?].}

@defform[(delay/sync body ...+)]{

Conventional promises are not useful when multiple threads attempt to
force them: when a promise is running, any additional threads that
@scheme[force] it will get an exception.  @scheme[delay/sync] is
useful for such cases: if a second thread attempts to @scheme[force]
such a promise, it will get blocked until the computation is done and
an answer is available.  If @scheme[force] is used with the promise as
it is forced from the same thread, an exception is raised.

In addition, these promises can be used with @scheme[sync], which
blocks until it has been forced.  Note that using @scheme[sync] this
way is passive in the sense that it does not trigger evaluation of the
promise.}

@defform[(delay/thread body ...+)]{
@; TODO: document #:group keyword

This kind of promise begins the computation immediately, but this
happens on a separate thread.  When the computation is done, the result
is cached as usual.  Note that exceptions are caught as usual, and will
only be raised when @scheme[force]d.  If such a promise is
@scheme[force]d before a value is ready, the calling thread will be
blocked until the computation terminates.  These promises can also be
used with @scheme[sync].}

@defform[(delay/idle body ...+)]{
@; TODO: document #:wait-for, #:work-while, #:tick, #:use keywords

Similar to @scheme[delay/thread], but the computation thread gets to
work only when the process is otherwise idle, as determined by
@scheme[system-idle-evt], and the work is done in small runtime
fragements, making it overall not raise total CPU use or hurt
responsiveness.  If the promise is @scheme[forced] before the
computation is done, it will run the rest of the computation immediately
without slicing the runtime.  Using @scheme[sync] on these promises
blocks as is the case with @scheme[delay/sync], and this happens in a
passive way too, so the computation continues to work in low-priority.

@;{
TODO: Say something on:
* `use' = 0 --> similar to a plain `delay' which is evaluated only when
  forced (or delay/sync, since it's still sync-able), except that the
  evaluation is still happening on a new thread.
* `use' = 1 --> given cpu time as usual, but still polls the idle event
  every `tick' seconds
* `use' = 1 and both `wait-for' and `work-while' are `always-evt' -->
  similar to `delay/thread'.
* can use `wait-for' to delay evaluation start until some event is
  ready.  Specifically, this can be done to chain a few of these
  promises sequentially.
* same goes for `work-while'.  For example, you can use that with a
  `semaphore-peek-evt' to be able to pause/resume the computation on
  demand.
;}
}
