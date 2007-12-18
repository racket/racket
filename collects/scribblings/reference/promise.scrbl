#lang scribble/doc
@(require "mz.ss"
          (for-label scheme/promise))

@title{Delayed Evaluation}

@note-lib[scheme/promise]

A @deftech{promise} encapsulates an expression to be evaluated on
demand via @scheme[force]. After a promise has been @scheme[force]d,
every later @scheme[force] of the promise produces the same result.


@defproc[(promise? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a promise, @scheme[#f]
otherwise.}


@defform[(delay expr)]{

Creates a promise that, when @scheme[force]d, evaluates @scheme[expr]
to produce its value.}

@defform[(lazy expr)]{

Like @scheme[delay], except that if @scheme[expr] produces a promise,
then the promise is @scheme[force]d to obtain a value.}

@defproc[(force [v any/c]) any]{

If @scheme[v] is a promise, then the promise is forced to obtain a
value. If the promise has not been forced before, then the result is
recorded in the promise so that future @scheme[force]s on the promise
produce the same value (or values). If forcing the promise raises an
exception, then the exception is similarly recorded so that forcing
the promise will raise the same exception every time.

If @scheme[v] is @scheme[force]d again before the original call to
@scheme[force] returns, then the @exnraise[exn:fail].

If @scheme[v] is not a promise, then it is returned as the result.}
