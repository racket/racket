#lang scribble/doc
@(require "mz.rkt" (for-label racket/promise))

@title{Delayed Evaluation}

@note-lib[racket/promise]

A @deftech{promise} encapsulates an expression to be evaluated on
demand via @racket[force]. After a promise has been @racket[force]d,
every later @racket[force] of the promise produces the same result.


@defproc[(promise? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a promise, @racket[#f]
otherwise.}


@defform[(delay body ...+)]{

Creates a promise that, when @racket[force]d, evaluates the
@racket[body]s to produce its value.  The result is then cached, so
further uses of @racket[force] produce the cached value immediately.
This includes multiple values and exceptions.}


@defform[(lazy body ...+)]{

Like @racket[delay], if the last @racket[body] produces a promise when
forced, then this promise is @racket[force]d, too, to obtain a value.
In other words, this form creates a composable promise, where the
computation of its body is ``attached'' to the computation of the
following promise, and a single @racket[force] iterates through the
whole chain, tail-calling each step.

Note that the last @racket[body] of this form must produce a single
value, but the value can itself be a @racket[delay] promise that
returns multiple values.

The @racket[lazy] form is useful for implementing lazy libraries and
languages, where tail calls can be wrapped in a promise.}


@defproc[(force [v any/c]) any]{

If @racket[v] is a promise, then the promise is forced to obtain a
value. If the promise has not been forced before, then the result is
recorded in the promise so that future @racket[force]s on the promise
produce the same value (or values). If forcing the promise raises an
exception, then the exception is similarly recorded so that forcing
the promise will raise the same exception every time.

If @racket[v] is @racket[force]d again before the original call to
@racket[force] returns, then the @exnraise[exn:fail].

If @racket[v] is not a promise, then it is returned as the result.}


@defproc[(promise-forced? [promise promise?]) boolean?]{

Returns @racket[#t] if @racket[promise] has been forced.}


@defproc[(promise-running? [promise promise?]) boolean?]{

Returns @racket[#t] if @racket[promise] is currently being forced.
(Note that a promise can be either running or forced but not both.)}


@section{Additional Promise Kinds}

@defform[(delay/name body ...+)]{

Creates a ``call-by-name'' promise that is similar to
@racket[delay]-promises, except that the resulting value is not
cached.  This kind of promise is essentially a thunk that is wrapped
in a way that @racket[force] recognizes.

If a @racket[delay/name] promise forces itself, no exception is
raised, the promise is never considered ``running'' or ``forced'' in
the sense of @racket[promise-running?] and @racket[promise-forced?].}

@defform[(delay/strict body ...+)]{

Creates a ``strict'' promise: it is evaluated immediately, and the
result is wrapped in a promise value.  Note that the body can evaluate
to multiple values, and forcing the resulting promise will return these
values.}

@defform[(delay/sync body ...+)]{

Produces a promise where an attempt to @racket[force] the promise by a
thread other than one currently running the promise causes the
@racket[force] to block until a result is available. This kind of
promise is also a @tech{synchronizable event} for use with
@racket[sync]; @racket[sync]ing on the promise does not @racket[force]
it, but merely waits until a value is forced by another thread.

If a promise created by @racket[delay/sync] is forced on a thread that
is already running the promise, an exception is raised in the same way
as for promises created with @racket[delay].}

@defform/subs[(delay/thread body/option ...+)
              ([body/option body
                            (code:line #:group thread-group-expr)])]{

Like @racket[delay/sync], but begins the computation immediately on a
newly created thread. The thread is created under the @tech{thread
group} specified by @racket[thread-group-expr], which defaults to
@racket[(make-thread-group)]. A @racket[#:group] specification can
appear at most once.

Exceptions raised by the @racket[body]s are caught as usual and raised
only when the promise is @racket[force]d. Unlike @racket[delay/sync],
if the thread running @racket[body] terminates without producing a result
or exception, @racket[force] of the promise raises an exception (instead
of blocking).}

@defform/subs[(delay/idle body/option ...+)
              ([body/option body
                            (code:line #:wait-for wait-evt-expr)
                            (code:line #:work-while while-evt-expr)
                            (code:line #:tick tick-secs-expr)
                            (code:line #:use use-ratio-expr)])]{

Like @racket[delay/thread], but with the following differences:

@itemlist[

 @item{the computation does not start until the event produced by
       @racket[wait-evt-expr] is ready, where the default is
       @racket[(system-idle-evt)];}

 @item{the computation thread gets to work only when the process is
       otherwise idle as determined by @racket[while-evt-expr], which
       also defaults to @racket[(system-idle-evt)];}

 @item{the thread is allowed to run only periodically: out of every
       @racket[tick-secs-expr] (defaults to @racket[0.2]) seconds, the
       thread is allowed to run @racket[use-ratio-expr] (defaults to
       @racket[0.12]) of the time proportionally; i.e., the thread
       runs for @racket[(* tick-secs-expr use-ratio-expr)] seconds.}

]

If the promise is @racket[forced] before the computation is done, it
runs the rest of the computation immediately without waiting on events
or periodically restricting evaluation.

A @racket[#:wait-for], @racket[#:work-while], @racket[#:tick], or
@racket[#:use] specification can appear at most once.

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
