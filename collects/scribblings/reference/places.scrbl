#lang scribble/doc

@title[#:tag "places"]{Places}

@; ----------------------------------------------------------------------

@(require scribble/manual
          scribble/urls
          scribble/struct
          "mz.ss"
          (for-label racket
                     racket/base
                     racket/contract
                     racket/place
                     racket/future
                     racket/flonum
                     racket/fixnum))

@; ----------------------------------------------------------------------

@margin-note{Currently, parallel support for @racket[place] is enabled
  only for Racket 3m (which is the main variant of Racket), and only
  by default for Windows, Linux x86/x86_64, and Mac OS X x86/x86_64. To
  enable support for other platforms, use @DFlag{enable-places} with
  @exec{configure} when building Racket.}

@note-lib[racket/place]

@tech{Places} enable the development of parallel programs that
take advantage of machines with multiple processors, cores, or
hardware threads.

A @deftech{place} is a parallel task that is effectively a separate
instance of the Racket virtual machine. Places communicate through
@deftech{place channels}, which are endpoints for a two-way buffered
communication.

To a first approximation, place channels allow only immutable values
as messages over the channel: numbers, characters, booleans, immutable
pairs, immutable vectors, and immutable structures. In addition, place
channels themselves can be sent across channels to establish new
(possibly more direct) lines of communication in addition to any
existing lines. Finally, mutable values produced by
@racket[shared-flvector], @racket[make-shared-flvector],
@racket[shared-fxvector], @racket[make-shared-fxvector],
@racket[shared-bytes], and @racket[make-shared-bytes] can be sent
across place channels; mutation of such values is visible to all
places that share the value, because they are allowed in a
@deftech{shared memory space}.

A @tech{place channel} can be used as a @tech{synchronizable event}
(see @secref["sync"]) to receive a value through the channel. A place
can also receive messages with @racket[place-channel-get], and
messages can be sent with @racket[place-channel-put].

Constraints on messages across a place channel---and therefore on the
kinds of data that places share---enable greater parallelism than
@racket[future], even including separate @tech{garbage collection} of
separate places. At the same time, the setup and communication costs
for places can be higher than for futures.

For example, the following expression lanches two places, echoes a
message to each, and then waits for the places to complete and return:

@racketblock[
(let ([pls (for/list ([i (in-range 2)])
              (place "place-worker.rkt" 'place-main))])
   (for ([i (in-range 2)]
         [p pls])
      (place-channel-put p i)
      (printf "~a\n" (place-channel-get p)))
   (map place-wait pls))
]

The @filepath{place-worker.rkt} module must export the
@racket[place-main] function that each place executes, where
@racket[place-main] must accept a single @tech{place channel}
argument:

@racketmod[
racket
(provide place-main)

(define (place-main pch)
  (place-channel-put pch (format "Hello from place ~a" 
                                  (place-channel-get pch))))
]


@defproc[(place? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @deftech{place descriptor}
  value, @racket[#f] otherwise. Every @tech{place descriptor}
  is also a @tech{place channel}.
}

@defproc[(place-channel? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is @tech{place channel}, 
  @racket[#f] otherwise.
}

@defproc[(place [module-path module-path?] [start-proc symbol?]) place?]{

 Creates a @tech{place} to run the procedure that is identified by
 @racket[module-path] and @racket[start-proc]. The result is a
 @tech{place descriptor} value that represents the new parallel task;
 the place descriptor is returned immediately. The place descriptor
 value is also a @tech{place channel} that permits communication with
 the place.

 The module indicated by @racket[module-path] must export a function
 with the name @racket[start-proc]. The function must accept a single
 argument, which is a @tech{place channel} that corresponds to the
 other end of communication for the @tech{place descriptor} returned
 by @racket[place].}


@defproc[(place-wait [p place?]) exact-integer?]{
  Returns the completion value of the place indicated by @racket[p],
  blocking until the place completes if it has not already completed.
}

@defproc[(place-break [p place?]) void?]{
  Sends place @racket[p] a break signal; see @secref["breakhandler"].
}

@defproc[(place-kill [p place?]) void?]{
  Terminates the place indicated by @racket[p],
}


@defform[(place/thunk place-channel? body ...)]{
  In-line definition of a place worker body, which is lifted up to module scope.
  @racket[place/thunk] closes over only module scope variables.
  Returns a thunk for creating the described place.
}

@defform[(place/anon place-channel? body ...)]{
  In-line definition of a place worker body, which is lifted up to module scope.
  @racket[place/anon] closes over only module scope variables.
  Returns the place descriptor for the newly constructed place.
}

@defform[(define-place (place-name-id place-channel-id) body ...)]{
  In-line definition of a place worker body, which is lifted up to module scope.
  @racket[define-place] closes over only module scope variables.
  Defines a procedure for creating the described place.
}

@defproc[(place-channel) (values place-channel? place-channel?)]{

  Returns two @tech{place channels}. Data sent through the first
  channel can be received through the second channel, and data sent
  through the second channel can be received from the first.

  Typically, one place channel is used by the current @tech{place} to
  send messages to a destination @tech{place}; the other place channel
  is sent to the destination @tech{place} (via an existing @tech{place
  channel}).
}

@defproc[(place-channel-put [pch place-channel?] [v any/c]) void]{
  Sends an immutable message @racket[v] on channel @racket[pch].
}

@defproc[(place-channel-get [pch place-channel?]) any/c]{
  Returns an immutable message received on channel @racket[pch].
}

@defproc[(place-channel-put/get [pch place-channel?] [v any/c]) void]{
  Sends an immutable message @racket[v] on channel @racket[pch] and then 
  waits for a reply message on the same channel.
}
