#lang scribble/doc
@(require "utils.rkt" (for-label ffi/unsafe/atomic))

@title{Atomic Execution}

@defmodule[ffi/unsafe/atomic]

@deftogether[(
@defproc[(start-atomic) void?]
@defproc[(end-atomic) void?]
)]{

Disables and enables context switches and delivery of break exceptions
at the level of Racket threads. Calls to @racket[start-atomic] and
@racket[end-atomic] can be nested.

Using @racket[call-as-atomic] is somewhat safer, in that
@racket[call-as-atomic] correctly catches exceptions and re-raises
them after exiting atomic mode. For simple uses where exceptions need
not be handled, however, @racket[start-atomic] and @racket[end-atomic]
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
faster than plain @racket[start-atomic] and @racket[end-atomic].}


@defproc[(call-as-atomic [thunk (-> any)]) any]{

Calls @racket[thunk] in atomic mode. If @racket[thunk] raises an
exception, the exception is caught and re-raised after exiting atomic
mode.

When @racket[call-as-atomic] is used in the dynamic extent of
@racket[call-as-atomic], then @racket[thunk] is simply called directly
(as a non-tail call).}


@defproc[(call-as-nonatomic [thunk (-> any)]) any]{

Within the dynamic extent of a @racket[call-as-atomic], calls
@racket[thunk] in non-atomic mode. Beware that the current thread
may be suspended or terminated by other threads during the
execution of @racket[thunk], in which case the call never returns.

When used not in the dynamic extent of @racket[call-as-atomic],
@racket[call-as-nonatomic] raises @racket[exn:fail:contract].}


@defproc[(in-atomic-mode?) boolean?]{

Returns @racket[#t] if Racket context switches are disabled,
@racket[#f] otherwise.}
