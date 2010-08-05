#lang scribble/doc

@title{@bold{Places}: Coarse-grained Parallelism}

@; ----------------------------------------------------------------------

@(require scribble/manual
          scribble/urls
          scribble/struct
          (for-label racket
                     racket/base
                     racket/contract
                     racket/place))

@; ----------------------------------------------------------------------

The PLT futures API enables the development of parallel programs which
take advantage of machines with multiple processors, cores, or
hardware threads.

@defmodule[racket/place]{}

@defproc[(place [module-path module-path?] [start-proc proc?] [place-channel place-channel?]) place?]{
  Starts running @racket[start-proc] in parallel. @racket[start-proc] must
  be a function defined in @racket[module-path].  Each place is created with a racket[place-channel]
  that permits communication with the place originator.  This initial channel can be overridden with
  an optional @racket[place-channel] argument.  The @racket[place]
  procedure returns immediately with a place descriptor value representing the newly constructed place.
}

@defproc[(place-wait [p place?]) exact-integer?]{
  Returns the return value of a completed place @racket[p], blocking until
  the place completes (if it has not already completed).
}

@defproc[(place? [x any/c]) boolean?]{
  Returns @racket[#t] if @racket[x] is a place object.
}

@defproc[(place-channel) (values place-channel? place-channel?)]{
  Returns two @racket[place-channel] objects.
  
  One @racket[place-channel] should be used by the current @racket[place] to send 
  messages to a destination @racket[place].

  The other @racket[place-channel] should be sent to a destination @racket[place] over
  an existing @racket[place-channel].
}

@defproc[(place-channel-send [ch place-channel?] [x any/c]) void]{
  Sends an immutable message @racket[x] on channel @racket[ch].
}

@defproc[(place-channel-recv [p place-channel?]) any/c]{
  Returns an immutable message received on channel @racket[ch].
}

@defproc[(place-channel? [x any/c]) boolean?]{
  Returns @racket[#t] if @racket[x] is a place-channel object.
}

@defproc[(place-channel-send/recv [ch place-channel?] [x any/c]) void]{
  Sends an immutable message @racket[x] on channel @racket[ch] and then 
  waits for a repy message.
  Returns an immutable message received on channel @racket[ch].
}

@section[#:tag "example"]{How Do I Keep Those Cores Busy?}

This code launches two places passing 1 and 2 as the initial channels 
and then waits for the places to complete and return.

@racketblock[
    (let ((pls (map (lambda (x) (place "place-worker.ss" 'place-main x))
                  (list 1 2))))
       (map place-wait pls))
]

This is the code for the place-worker.ss module that each place will execute.

@racketblock[
(module place-worker racket
  (provide place-main)

  (define (place-main x)
   (printf "IN PLACE ~a~n" x)))
]

@section[#:tag "place-channels"]{Place Channels}
Place channels can be used with @racket[place-channel-recv], or as a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{synchronizable event}
 (see @secref[#:doc '(lib "scribblings/reference/reference.scrbl") "sync"]) to receive a value
through the channel. The channel can be used with @racket[place-channel-send]
to send a value through the channel.

@section[#:tag "messagepassingparallelism"]{Message Passing Parallelism}

Places can only communicate by passing immutable messages on place-channels.
Only immutable pairs, vectors, and structs can be communicated across places channels.

@section[#:tag "places-architecture"]{Architecture and Garbage Collection}

Immutable messages communicated on place-channels are first copied to a shared
garbage collector called the master.  Places are allowed to garbage collect
independently of one another.  The master collector, however, has to pause all
mutators before it can collect garbage.

@section[#:tag "enabling-places"]{Enabling Places in Racket Builds}

PLT's parallel-places support is only enabled if you pass
@DFlag{enable-places} to @exec{configure} when you build PLT (and
that build currently only works with @exec{racket}, not with
@exec{gracket}). When parallel-places support is not enabled,
@racket[place] usage is a syntax error.
@; @FIXME{use threads to emulate places maybe?}
