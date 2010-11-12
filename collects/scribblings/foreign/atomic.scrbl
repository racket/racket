#lang scribble/doc
@(require "utils.ss"
          (for-label ffi/unsafe/atomic))

@title{Atomic Execution}

@defmodule[ffi/unsafe/atomic]

@deftogether[(
@defproc[(start-atomic) void?]
@defproc[(end-atomic) void?]
)]{

Disables and enables context switches and delivery of break exceptions
at the level of Racket threads. Calls to @scheme[start-atomic] and
@scheme[end-atomic] can be nested.

Using @scheme[call-as-atomic] is somewhat safer, in that
@scheme[call-as-atomic] correctly catches exceptions and re-raises
them after exiting atomic mode. For simple uses where exceptions need
not be handled, however, @scheme[start-atomic] and @scheme[end-atomic]
are faster.}

@deftogether[(
@defproc[(start-breakable-atomic) void?]
@defproc[(end-breakable-atomic) void?]
)]{

Like @racket[start-atomic] and @racket[end-atomic], but the delivery
of break exceptions is not suspended. To ensure that a call to
@racket[start-atomic] is reliably paired with a call to
@racket[end-atomic], use @racket[dynamic-wind] pre- and post thunks or
some other context where breaks are disabled. These variants are not
faster than plan @racket[start-atomic] and @racket[end-atomic].}


@defproc[(call-as-atomic [thunk (-> any)]) any]{

Calls @scheme[thunk] in atomic mode. If @scheme[thunk] raises and
exception, the exception is caught and re-raised after exiting atomic
mode.

When @scheme[call-as-atomic] is used in the dynamic extent of
@scheme[call-as-atomic], then @scheme[thunk] is simply called directly
(as a tail call).}


@defproc[(call-as-nonatomic [thunk (-> any)]) any]{

Within the dynamic extent of a @scheme[call-as-atomic], calls
@scheme[thunk] in non-atomic mode. Beware that the current thread
maybe suspended or terminated by other threads during @scheme[thunk],
in which case the call never returns.

When used not in the dynamic extent of @scheme[call-as-atomic],
@scheme[call-as-nonatomic] raises @scheme[exn:fail:contract].}

