#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "channel"]{Channels}

A @deftech{channel} both synchronizes a pair of threads and passes a
value from one to the other. Channels are synchronous; both the sender
and the receiver must block until the (atomic) transaction is
complete. Multiple senders and receivers can access a channel at once,
but a single sender and receiver is selected for each transaction.

Channel synchronization is @defterm{fair}: if a thread is blocked on a
channel and transaction opportunities for the channel occur infinitely
often, then the thread eventually participates in a transaction.

In addition to its use with channel-specific procedures, a channel can
be used as a @tech{synchronizable event} (see @secref["sync"]).  A
channel is @tech{ready for synchronization} when @racket[channel-get]
would not block; the channel's @tech{synchronization result} is the
same as the @racket[channel-get] result.

For buffered asynchronous channels, see @secref["async-channel"].

@defproc[(channel? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{channel}, 
@racket[#f] otherwise.}

@defproc[(make-channel) channel?]{

Creates and returns a new channel. The channel can be used with
@racket[channel-get], with @racket[channel-try-get], or as a
@tech{synchronizable event} (see @secref["sync"]) to receive a value
through the channel. The channel can be used with @racket[channel-put]
or through the result of @racket[channel-put-evt] to send a value
through the channel.}

@defproc[(channel-get [ch channel?]) any]{

Blocks until a sender is ready to provide a value through
@racket[ch]. The result is the sent value.}

@defproc[(channel-try-get [ch channel?]) any]{

Receives and returns a value from @racket[ch] if a sender is
immediately ready, otherwise returns @racket[#f].}

@defproc[(channel-put [ch channel?] [v any/c]) void?]{

Blocks until a receiver is ready to accept the value @racket[v]
through @racket[ch].}


@defproc[(channel-put-evt [ch channel?] [v any/c]) channel-put-evt?]{

Returns a fresh @tech{synchronizable event} for use with
@racket[sync]. The event is @tech{ready for synchronization} when
@racket[(channel-put ch v)] would not block, and the event's
@tech{synchronization result} is the event itself.}


@defproc[(channel-put-evt? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a channel-put event produced by
@racket[channel-put-evt], @racket[#f] otherwise.}
