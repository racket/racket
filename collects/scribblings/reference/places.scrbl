#lang scribble/doc

@title[#:tag "places"]{Places}

@; ----------------------------------------------------------------------

@(require scribble/manual scribble/urls scribble/struct "mz.rkt"
          (for-label racket
                     racket/base
                     racket/contract
                     racket/place
                     racket/future
                     racket/flonum
                     racket/fixnum))

@; ----------------------------------------------------------------------

@margin-note{Currently, parallel support for places is enabled
  only for Racket 3m (which is the main variant of Racket), and only
  by default for Windows, Linux x86/x86_64, and Mac OS X x86/x86_64. To
  enable support for other platforms, use @DFlag{enable-places} with
  @exec{configure} when building Racket. The @racket[place-enabled?]
  function reports whether places run in parallel.}

@note-lib[racket/place]

@tech{Places} enable the development of parallel programs that
take advantage of machines with multiple processors, cores, or
hardware threads.

A @deftech{place} is a parallel task that is effectively a separate
instance of the Racket virtual machine. Places communicate through
@deftech{place channels}, which are endpoints for a two-way buffered
communication.

To a first approximation, place channels support only immutable,
transparent values as messages. In addition, place
channels themselves can be sent across channels to establish new
(possibly more direct) lines of communication in addition to any
existing lines. Finally, mutable values produced by
@racket[shared-flvector], @racket[make-shared-flvector],
@racket[shared-fxvector], @racket[make-shared-fxvector],
@racket[shared-bytes], and @racket[make-shared-bytes] can be sent
across place channels; mutation of such values is visible to all
places that share the value, because they are allowed in a
@deftech{shared memory space}. See @racket[place-message-allowed?].

A @tech{place channel} can be used as a @tech{synchronizable event}
(see @secref["sync"]) to receive a value through the channel. A place
can also receive messages with @racket[place-channel-get], and
messages can be sent with @racket[place-channel-put].

Constraints on messages across a place channel---and therefore on the
kinds of data that places share---enable greater parallelism than
@racket[future], even including separate @tech{garbage collection} of
separate places. At the same time, the setup and communication costs
for places can be higher than for futures.

For example, the following expression launches two places, echoes a
message to each, and then waits for the places to terminate:

@racketblock[
(let ([pls (for/list ([i (in-range 2)])
              (dynamic-place "place-worker.rkt" 'place-main))])
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


@defproc[(place-enabled?) boolean?]{

Returns @racket[#t] if Racket is configured so that
@racket[dynamic-place] and @racket[place] create places that can run
in parallel, @racket[#f] if @racket[dynamic-place] and @racket[place]
are simulated using @racket[thread].}


@defproc[(place? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @deftech{place descriptor}
  value, @racket[#f] otherwise. Every @tech{place descriptor}
  is also a @tech{place channel}.
}

@defproc[(place-channel? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is @tech{place channel}, 
  @racket[#f] otherwise.
}


@defproc[(dynamic-place [module-path module-path?] [start-proc symbol?]) place?]{

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
 by @racket[place].

 When the @tech{place} is created, the initial @tech{exit handler}
 terminates the place, using the argument to the exit handler as the
 place's @deftech{completion value}. Use @racket[(exit _v)] to
 immediately terminate a place with the completion value
 @racket[_v]. Since a completion value is limited to an exact integer
 between @racket[0] and @racket[255], any other value for @racket[v]
 is converted to @racket[0].

 If the function indicated by @racket[module-path] and
 @racket[start-proc] returns, then the place terminates with the
 @tech{completion value} @racket[0].}


@defform[(place id body ...+)]{
  Creates a place that evaluates @racket[body]
  expressions with @racket[id] bound to a place channel.  The
  @racket[body]s close only over @racket[id] plus the top-level
  bindings of the enclosing module, because the
  @racket[body]s are lifted to a function that is exported by
  the module. The result of @racket[place] is a place descriptor,
  like the result of @racket[dynamic-place].
}


@defproc[(place-wait [p place?]) exact-integer?]{
  Returns the @tech{completion value} of the place indicated by @racket[p],
  blocking until the place has terminated.
}


@defproc[(place-dead-evt [p place?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that is
ready if and only if @racket[p] has terminated.}


@defproc[(place-kill [p place?]) void?]{
  Immediately terminates the place, setting the place's
  @tech{completion value} to @racket[1] if the place does not have a
  completion value already.}


@defproc[(place-break [p place?]) void?]{
  Sends place @racket[p] a break signal; see @secref["breakhandler"].
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

@defproc[(place-channel-put [pch place-channel?] [v place-message-allowed?]) void]{
  Sends a message @racket[v] on channel @racket[pch].

 See @racket[place-message-allowed?] form information on automatic
 coercions in @racket[v], such as converting a mutable string to an
 immutable string.

}

@defproc[(place-channel-get [pch place-channel?]) place-message-allowed?]{
  Returns a message received on channel @racket[pch].
}

@defproc[(place-channel-put/get [pch place-channel?] [v any/c]) void]{
  Sends an immutable message @racket[v] on channel @racket[pch] and then 
  waits for a reply message on the same channel.
}

@defproc[(place-message-allowed? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is allowed as a message on a place channel,
@racket[#f] otherwise.

If @racket[(place-enabled?)] returns @racket[#f], then the result is
always @racket[#t] and no conversions are performed on @racket[v] as a
message. Otherwise, the following kinds of data are allowed as
messages:

@itemlist[

 @item{@tech{numbers}, @tech{characters}, @tech{booleans}, and
       @|void-const|;}

 @item{@tech{symbols} that are @tech{interned};}
 
 @item{@tech{strings} and @tech{byte strings}, where mutable strings
       and byte strings are automatically replaced by immutable
       variants;}

 @item{@tech{pairs}, @tech{lists}, @tech{vectors}, and immutable
       @tech{prefab} structures containing message-allowed values,
       where a mutable vector is automatically replaced by an
       immutable vector;}

 @item{@tech{place channels}, where a @tech{place descriptor} is
       automatically replaced by a plain place channel; and}

 @item{values produced by @racket[shared-flvector],
       @racket[make-shared-flvector], @racket[shared-fxvector],
       @racket[make-shared-fxvector], @racket[shared-bytes], and
       @racket[make-shared-bytes].}

]}

@;------------------------------------------------------------------------
