#lang scribble/doc

@title{@bold{Places}: Coarse-grained Parallelism}

@; ----------------------------------------------------------------------

@(require scribble/manual
          scribble/urls
          scribble/struct
          (for-label scheme
                     scheme/base
                     scheme/contract
                     scheme/place))

@; ----------------------------------------------------------------------

The PLT futures API enables the development of parallel programs which
take advantage of machines with multiple processors, cores, or
hardware threads.

@defmodule[scheme/place]{}

@defproc[(place [module-path module-path?] [start-proc proc?] [place-channel place-ch?]) place?]{
  Starts running @scheme[start-proc] in parallel. scheme[start-proc] must
  be a function defined in @scheme[module-path].  The @scheme[place]
  procedure returns immediately with a place descriptor value.
}

@defproc[(place-wait [p place?]) exact-integer?]{
  Returns the return value of a completed place @scheme[p], blocking until
  the place completes (if it has not already completed).
}

@defproc[(place? [x any/c]) boolean?]{
  Returns @scheme[#t] if @scheme[x] is a place.
}

@defproc[(place-ch-send [ch place-ch?] [x any/c]) void]{
  Sends an immutable message @scheme[x] on channel @scheme[ch].
}

@defproc[(place-ch-recv [p place-ch?]) any/c]{
  Returns an immutable message received on channel @scheme[ch].
}

@defproc[(place-ch? [x any/c]) boolean?]{
  Returns @scheme[#t] if @scheme[x] is a place-ch.
}

@section[#:tag "example"]{How Do I Keep Those Cores Busy?}

This code launches two places passing 1 and 2 as the initial channels 
and then waits for the places to complete and return.

@schemeblock[
    (let ((pls (map (lambda (x) (place "place_worker.ss" 'place-main x))
                  (list 1 2))))
       (map place-wait pls))
]

This is the code for the place_worker.ss module that each place will execute.

@schemeblock[
(module place_worker scheme
  (provide place-main)

  (define (place-main x)
   (printf "IN PLACE ~a~n" x)))
]


@section[#:tag "messagepassingparallelism"]{Message Passing Parallelism}

Places can only communicate by passing immutable messages on place-channels.

@section[#:tag "logging"]{Architecture and Garbage Collection}

Immutable messages communicated on place-channels are first copied to a shared
garbage collector called the master.  The master waits on a barrier until all places garbage
collectors have collected.  Once the master is released it collects and resets
the barrier.

@section[#:tag "compiling"]{Enabling Places in MzScheme Builds}

PLT's parallel-places support is only enabled if you pass
@DFlag{enable-places} to @exec{configure} when you build PLT (and
that build currently only works with @exec{mzscheme}, not with
@exec{mred}). When parallel-future support is not enabled,
@scheme[place] usage is a syntax error.
@; @FIXME{use threads to emulate places maybe?}
