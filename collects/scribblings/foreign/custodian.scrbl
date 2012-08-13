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
shutdown. The result is a pointer that can be supplied to
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
@racket[weak?] as @racket[#f] and have a finalizer cancel the shutdown
action via @racket[unregister-custodian-shutdown]. Otherwise, a
not-yet-run finalizer may remain pending after the custodian is
shutdown.}


@defproc[(unregister-custodian-shutdown [v any/c]
                                        [registration _cpointer])
         void?]{

Cancels a custodian-shutdown registration, where @racket[registration]
is a previous result from @racket[register-custodian-shutdown] applied
to @racket[v].}
