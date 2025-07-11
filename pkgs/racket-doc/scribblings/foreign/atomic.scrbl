#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/atomic
                     racket/future))

@title{Atomic Execution}

@defmodule[ffi/unsafe/atomic]

@deftech{Atomic mode} evaluates a Racket expression without switching
among Racket threads and with limited support for events. An atomic
computation in this sense is @emph{not} atomic with respect to other
@tech[#:doc reference.scrbl]{places}, but only to other @tech[#:doc
reference.scrbl]{threads} within a place.

@elemtag["atomic-unsafe"]{Atomic mode is @bold{unsafe}}, because the
Racket scheduler is not able to operate while execution is in atomic
mode; the scheduler cannot switch threads or poll certain kinds of
events, which can lead to deadlock or starvation of other threads.
Beware that many operations can involve such synchronization, such as
writing to an output port. Even if an output target is known to be
free of synchronization, beware that values can have arbitrary
printing procedures attached through @racket[prop:custom-write].
Successful use of atomic mode requires a detailed knowledge of any
implementation that might be reached during atomic mode to ensure that
it terminates and does not involve synchronization.

@deftogether[(
@defproc[(start-atomic) void?]
@defproc[(end-atomic) void?]
)]{

Disables/re-enables context switches at the level of Racket threads,
and also suspends/resumes delivery of break exceptions (independent of the
result of @racket[break-enabled]). Calls to @racket[start-atomic] and
@racket[end-atomic] can be nested.

Note that pairing @racket[start-atomic] and @racket[end-atomic] with
@racket[dynamic-wind] is useful only when

@itemlist[

 @item{the current @tech[#:doc reference.scrbl]{exception handler} is
       known to safely escape atomic mode, or else all possible
       escapes are through known continuation jumps or aborts (because
       breaks are disabled and no other exceptions are possible) that
       escape safely; and}

 @item{exception constructions, if any, avoid printing values in the
       exception message, or else the @tech[#:doc
       reference.scrbl]{error value conversion handler} is always used
       and known to be safe for atomic mode.}

]

Using @racket[call-as-atomic] is somewhat safer than using
@racket[start-atomic] and @racket[end-atomic], because
@racket[call-as-atomic] catches exceptions and re-raises them after
exiting atomic mode, and it wraps any call to the error value
conversion handler with @racket[call-as-nonatomic]. The latter is safe
for a particular atomic region, however, only if the region can be
safely interrupted by a non-atomic exception construction.

Unlike @racket[call-as-atomic], @racket[start-atomic] and
@racket[end-atomic] can be called from any OS thread as supported by
@racketmodname[ffi/unsafe/os-thread], although the calls have no
effect in that case. Using @racket[start-atomic] in a @tech[#:doc
reference.scrbl]{future} that is not running in a Racket thread
blocks the future until it is resumed by @racket[touch] in a Racket
thread.

See also the caveat that @elemref["atomic-unsafe"]{atomic mode is unsafe}.}


@deftogether[(
@defproc[(start-breakable-atomic) void?]
@defproc[(end-breakable-atomic) void?]
)]{

Like @racket[start-atomic] and @racket[end-atomic], but the delivery
of break exceptions is not suspended.

These functions are not significantly faster than
@racket[start-atomic] and @racket[end-atomic], so they provide no
benefit in a context where breaks are disabled.}


@defproc[(call-as-atomic [thunk (-> any)]) any]{

Calls @racket[thunk] in atomic mode, where @racket[call-as-nonatomic]
can be used during the dynamic extent of the call to revert to
non-atomic mode for a nested computation.

When @racket[call-as-atomic] is used in the dynamic extent of
@racket[call-as-atomic], then @racket[thunk] is called directly as a
non-tail call.

If @racket[thunk] raises an exception, the exception is caught and
re-raised after exiting atomic mode. Any call to the current
@tech[#:doc reference.scrbl]{error value conversion handler} is
effectively wrapped with @racket[call-as-nonatomic].

See also the caveat that @elemref["atomic-unsafe"]{atomic mode is unsafe}.}


@defproc[(call-as-nonatomic [thunk (-> any)]) any]{

Within the dynamic extent of a call to @racket[call-as-atomic], calls
@racket[thunk] in non-atomic mode. Beware that the current thread may
be suspended or terminated by other threads during the execution of
@racket[thunk].

When used not in the dynamic extent of a call to
@racket[call-as-atomic], @racket[call-as-nonatomic] raises
@racket[exn:fail:contract].}


@defproc[(in-atomic-mode?) boolean?]{

Returns @racket[#t] when in @tech{atomic mode} or @tech{uninterruptable mode} (within the current
@tech[#:doc reference.scrbl]{place}), @racket[#f] otherwise.}


@deftogether[(
@defproc[(start-uninterruptable) void?]
@defproc[(end-uninterruptable) void?]
)]{

Like @racket[start-atomic] and @racket[end-atomic], but with the
intent that the continuation after @racket[start-uninterruptable] and
before @racket[end-uninterruptable] may run concurrently with other Racket
threads, but in @deftech{uninterruptable mode}: it should reach
@racket[end-uninterruptable] without interruption from other threads.
Uninterruptable mode is unsafe, just like
@elemref["atomic-unsafe"]{atomic mode is unsafe}.

Uninterruptable mode is currently implemented using
@racket[start-atomic] and @racket[end-atomic], except when used within
a @tech[#:doc reference.scrbl]{future}. Unlike @racket[start-atomic],
@racket[start-uninterruptable] in the @CS[] implementation does not block a future that is
running concurrently with Racket threads. At the same time, it is the
responsibility of such a future to not perform any action that
blocks the future, otherwise the ``uninterruptable'' sequence is
interrupted, and it will be completed only when a Racket thread
@racket[touch]es the future (at which point it will run uninterrupted
in the Racket thread until @racket[end-uninterruptable]).

Calls to @racket[start-uninterruptable] and
@racket[end-uninterruptable] can be nested, and they can be mutually
nested with calls to @racket[start-atomic] and @racket[end-atomic]. A
call to @racket[start-atomic] nested in uninterruptable mode will
still block a future that is running concurrently to Racket threads.

@history[#:added "8.17.0.7"]}


@defproc[(call-as-uninterruptable [thunk (-> any)]) any]{

Similar to @racket[call-as-atomic], but for
@tech{uninterruptable mode}.

@history[#:added "8.17.0.7"]}
