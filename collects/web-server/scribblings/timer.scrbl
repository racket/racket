#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "timer.ss"]{Timers}
@(require (for-label web-server/private/timer))

@defmodule[web-server/private/timer]{

This module provides a functionality for running
procedures after a given amount of time, that may be extended.

@defstruct[timer ([evt evt?]
                  [expire-seconds number?]
                  [action (-> void)])]{
 @scheme[evt] is an @scheme[alarm-evt] that is ready at @scheme[expire-seconds].
 @scheme[action] should be called when this @scheme[evt] is ready.
}

@defproc[(start-timer-manager)
         void]{
 Handles the execution and management of timers.
}

@defproc[(start-timer [s number?]
                      [action (-> void)])
         timer?]{
 Registers a timer that runs @scheme[action] after @scheme[s] seconds.
}

@defproc[(reset-timer! [t timer?]
                       [s number?])
         void]{
 Changes @scheme[t] so that it will fire after @scheme[s] seconds.
}

@defproc[(increment-timer! [t timer?]
                           [s number?])
         void]{
 Changes @scheme[t] so that it will fire after @scheme[s] seconds from when
 it does now.
}

@defproc[(cancel-timer! [t timer?])
         void]{
 Cancels the firing of @scheme[t] ever and frees resources used by @scheme[t].
}

}
