#lang scribble/doc

@title[#:tag "places"]{@bold{Places}: Coarse-grained Parallelism}

@; ----------------------------------------------------------------------

@(require scribble/manual
          scribble/urls
          scribble/struct
          "mz.ss"
          (for-label racket
                     racket/base
                     racket/contract
                     racket/place
                     racket/flonum))

@; ----------------------------------------------------------------------

@deftech{Places} enable the development of parallel programs that
take advantage of machines with multiple processors, cores, or
hardware threads.

@note-lib[racket/place]

Note: currently, parallel support for @racket[place] is disabled by
default, and using it will raise an exception.  Support can only be
enabled if you build Racket yourself, and pass @DFlag{enable-places} to
@exec{configure}.  This works only for @exec{racket} (not
@exec{gracket}), and it is supported only on Linux x86/x86_64, and Mac
OS X x86/x86_64 platforms.

@defproc[(place [module-path module-path?] [start-proc symbol?]) place?]{
  Starts running @racket[start-proc] in parallel. @racket[start-proc] must
  be a function defined in @racket[module-path].  The @racket[place]
  procedure returns immediately with a place descriptor value representing the newly constructed place.
  Each place descriptor value is also a @racket[place-channel] that permits communication with the place.  
}

@defproc[(place-wait [p place?]) exact-integer?]{
  Returns the return value of a completed place @racket[p], blocking until
  the place completes (if it has not already completed).
}

@defproc[(place? [x any/c]) boolean?]{
  Returns @racket[#t] if @racket[x] is a place-descriptor value, @racket[#f] otherwise.
}

@defproc[(place-channel) (values place-channel? place-channel?)]{
  Returns two @racket[place-channel] endpoint objects.
  
  One @racket[place-channel] endpoint should be used by the current @racket[place] to send 
  messages to a destination @racket[place].

  The other @racket[place-channel] endpoint should be sent to a destination @racket[place] over
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

@section[#:tag "example"]{Basic Example}

This code launches two places, echos a message to them and then waits for the places to complete and return.

@racketblock[
(let ([pls (for/list ([i (in-range 2)])
              (place "place-worker.rkt" 'place-main))])
   (for ([i (in-range 2)]
         [p pls])
      (place-channel-send p i)
      (printf "~a\n" (place-channel-recv p)))
   (map place-wait pls))
]

This is the code for the place-worker.ss module that each place will execute.

@racketblock[
(module place-worker racket
  (provide place-main)

  (define (place-main ch)
    (place-channel-send ch (format "Hello from place ~a" (place-channel-recv ch)))))
]

@section[#:tag "place-channels"]{Place Channels}
Place channels can be used with @racket[place-channel-recv], or as a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{synchronizable event}
 (see @secref[#:doc '(lib "scribblings/reference/reference.scrbl") "sync"]) to receive a value
through the channel. The channel can be used with @racket[place-channel-send]
to send a value through the channel.

@section[#:tag "messagepassingparallelism"]{Message Passing Parallelism}

Places communicate by passing messages on place-channels.
Only atomic values, immutable pairs, vectors, and structs can be 
communicated across places channels.

@section[#:tag "places-architecture"]{Architecture and Garbage Collection}

Places enables a @deftech{shared memory space} between all places.
References from the @tech{shared memory space} back into a places memory space.
The invariant of allowing no backpointers is enforced by only allowing immutable
datastructures to be deep copied into the @tech{shared memory space}.

However, mutation of atomic values in
the @tech{shared memory space} is permitted to improve performace of 
shared-memory parallel programs. 

Special functions such as @racket[shared-flvector] and @racket[shared-bytes] allocate 
vectors of mutable atomic values into the @tech{shared memory space}.  

Parallel mutation of these atomic values
can possibly lead to data races, but will not cause @exec{racket} to
crash.  In practice however, parallel tasks usually write to disjoint 
partitions of a shared vector.

Places are allowed to garbage collect independently of one another.
The shared-memory collector, however, has to pause all
places before it can collect garbage.
