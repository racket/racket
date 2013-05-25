#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/try-atomic ffi/unsafe/atomic))

@title{Speculatively Atomic Execution}

@defmodule[ffi/unsafe/try-atomic]{The
@racketmodname[ffi/unsafe/try-atomic] supports atomic execution that
can be suspended and resumed in non-atomic mode if it takes too long
or if some external event causes the attempt to be abandoned.}

@defproc[(call-as-nonatomic-retry-point [thunk (-> any)]) any]{

Calls @racket[thunk] in atomic mode (see @racket[start-atomic] and
@racket[end-atomic]) while allowing @racket[thunk] to use
@racket[try-atomic]. Any incomplete computations started with
@racket[try-atomic] are run non-atomically after @racket[thunk]
returns. The result of @racket[thunk] is used as the result of
@racket[call-as-nonatomic-retry-point].}


@defproc[(try-atomic
          [thunk (-> any)]
          [default-val any/c]
          [#:should-give-up? give-up-proc (-> any/c) _run-200-milliseconds]
          [#:keep-in-order? keep-in-order? any/c #t])
         any]{

Within the dynamic extent of a @racket[call-as-nonatomic-retry-point]
call, attempts to run @racket[thunk] in the existing atomic mode. The
@racket[give-up-proc] procedure is called periodically to determine
whether atomic mode should be abandoned; the default
@racket[give-up-proc] returns true after 200 milliseconds. If atomic
mode is abandoned, the computation is suspended, and
@racket[default-val] is returned, instead. The computation is resumed
later by the enclosing @racket[call-as-nonatomic-retry-point] call.

If @racket[keep-in-order?] is true, then if @racket[try-atomic] is
called after an earlier computation was suspended for the same
@racket[call-as-nonatomic-retry-point] call, then
@racket[thunk] is immediately enqueued for completion by
@racket[call-as-nonatomic-retry-point] and @racket[default-val] is
returned.

The @racket[give-up-proc] callback is polled only at points where the
level of atomic-mode nesting (see @racket[start-atomic],
@racket[start-breakable-atomic], and @racket[call-as-atomic]) is the
same as at the point of calling @racket[try-atomic].

If @racket[thunk] aborts the current continuation using
@racket[(default-continuation-prompt-tag)], the abort is suspended the
resumed by the enclosing
@racket[call-as-nonatomic-retry-point]. Escapes to the context of the
call to @racket[thunk] using any other prompt tag or continuation are
blocked (using @racket[dynamic-wind]) and simply return
@racket[(void)] from @racket[thunk].}
