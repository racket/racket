#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/custodian))

@title{Custodian Shutdown Registration}

@defmodule[ffi/unsafe/custodian]{The
@racketmodname[ffi/unsafe/custodian] library provides utilities for
registering shutdown callbacks with custodians.}

@defproc[(register-custodian-shutdown [v any/c]
                                      [callback (any/c . -> . any)]
                                      [custodian custodian? (current-custodian)]
                                      [#:at-exit? at-exit? any/c #f]
                                      [#:weak? weak? any/c #f])
          cpointer?]{

Registers @racket[callback] to be applied (in atomic mode and an
unspecified Racket thread) to @racket[v] when @racket[custodian] is
shutdown. If @racket[custodian] is already shut down, the result is
@racket[#f] and @racket[v] is not registered. Otherwise, the result is
a pointer that can be supplied to
@racket[unregister-custodian-shutdown] to remove the registration.

If @racket[at-exit?] is true, then @racket[callback] is applied when
Racket exits, even if the custodian is not explicitly shut down.

If @racket[weak?]  is true, then @racket[callback] may not be called
if @racket[v] is determined to be unreachable during garbage
collection.  The value @racket[v] is always weakly held by the
custodian, even if @racket[weak?] is @racket[#f]; see
@cpp{scheme_add_managed} for more information.

Normally, @racket[weak?] should be false. To trigger actions based on
finalization or custodian shutdown---whichever happens first---leave
@racket[weak?] as @racket[#f] and have a finalizer run in atomic mode
and cancel the shutdown action via
@racket[unregister-custodian-shutdown]. If @racket[weak?] is true or
if the finalizer is not run in atomic mode, then there's no guarantee
that either of the custodian or finalizer callbacks has completed by
the time that the custodian shutdown has completed; @racket[v] might
be no longer registered to the custodian, while the finalizer for
@racket[v] might be still running or merely queued to run. See also
@racket[register-finalizer-and-custodian-shutdown].}


@defproc[(unregister-custodian-shutdown [v any/c]
                                        [registration cpointer?])
         void?]{

Cancels a custodian-shutdown registration, where @racket[registration]
is a previous result from @racket[register-custodian-shutdown] applied
to @racket[v]. If @racket[registration] is @racket[#f], then no action
is taken.}

@defproc[(register-finalizer-and-custodian-shutdown
                 [v any/c]
                 [callback (any/c . -> . any)]
                 [custodian custodian? (current-custodian)]
                 [#:at-exit? at-exit? any/c #f]
                 [#:weak? weak? any/c #f]
                 [#:custodian-unavailable unavailable-callback ((-> void?) -> any) (lambda (reg-fnl) (reg-fnl))])
         any]{

Registers @racket[callback] to be applied (in atomic mode) to
@racket[v] when @racket[custodian] is shutdown or when @racket[v] is
about to be collected by the garbage collector, whichever happens
first. The @racket[callback] is only applied to @racket[v] once.

If @racket[custodian] is already shut down, then
@racket[unavailable-callback] is applied in tail position to a
function that registers a finalizer. By default, a finalizer is
registered anyway, but usually a better choice is to report an error.
If @racket[custodian] is not already shut down, then the result
from @racket[register-finalizer-and-custodian-shutdown] is @|void-const|.

@history[#:added "6.1.1.6"]}


@defproc[(make-custodian-at-root) custodian?]{

Creates a custodian that is a child of the root custodian, bypassing
the @racket[current-custodian] setting.

Creating a child of the root custodian is useful for registering a
shutdown function that will be triggered only when the current place
terminates.

@history[#:added "6.9.0.5"]}
