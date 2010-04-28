#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "timer"]{Timers}
@(require (for-label web-server/private/timer))

@defmodule[web-server/private/timer]{

This module provides a functionality for running
procedures after a given amount of time, that may be extended.

@defstruct[timer ([evt evt?]
                  [expire-seconds number?]
                  [action (-> void)])]{
 @racket[evt] is an @racket[alarm-evt] that is ready at @racket[expire-seconds].
 @racket[action] should be called when this @racket[evt] is ready.
}

@defproc[(start-timer-manager)
         void]{
 Handles the execution and management of timers.
}

@defproc[(start-timer [s number?]
                      [action (-> void)])
         timer?]{
 Registers a timer that runs @racket[action] after @racket[s] seconds.
}

@defproc[(reset-timer! [t timer?]
                       [s number?])
         void]{
 Changes @racket[t] so that it will fire after @racket[s] seconds.
}

@defproc[(increment-timer! [t timer?]
                           [s number?])
         void]{
 Changes @racket[t] so that it will fire after @racket[s] seconds from when
 it does now.
}

@defproc[(cancel-timer! [t timer?])
         void]{
 Cancels the firing of @racket[t] ever and frees resources used by @racket[t].
}

}
