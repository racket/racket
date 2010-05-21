#lang scribble/doc
@(require "mz.ss")


@title[#:tag "channel"]{Channels}

A @deftech{channel} both synchronizes a pair of threads and passes a
value from one to the other. Channels are synchronous; both the sender
and the receiver must block until the (atomic) transaction is
complete. Multiple senders and receivers can access a channel at once,
but a single sender and receiver is selected for each transaction.

Channel synchronization is @defterm{fair}: if a thread is blocked on a
channel and transaction opportunities for the channel occur infinitely
often, then the thread eventually participates in a transaction.

For buffered asynchronous channels, see @secref["async-channel"].

@defproc[(make-channel) channel?]{

Creates and returns a new channel. The channel can be used with
@scheme[channel-get], with @scheme[channel-try-get], or as a
@tech{synchronizable event} (see @secref["sync"]) to receive a value
through the channel. The channel can be used with @scheme[channel-put]
or through the result of @scheme[channel-put-evt] to send a value
through the channel.}

@defproc[(channel? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a channel created by
@scheme[make-channel], @scheme[#f] otherwise.}


@defproc[(channel-get [ch channel?]) any]{

Blocks until a sender is ready to provide a value through
@scheme[ch]. The result is the sent value.}

@defproc[(channel-try-get [ch channel?]) any]{

Receives and returns a value from @scheme[ch] if a sender is
immediately ready, otherwise returns @scheme[#f].}

@defproc[(channel-put [ch channel?] [v any/c]) void?]{

Blocks until a receiver is ready to accept the value @scheme[v]
through @scheme[ch].}


@defproc[(channel-put-evt [ch channel?] [v any/c]) evt?]{

Returns a fresh @tech{synchronizable event} for use with
@scheme[sync]. The event is ready when @scheme[(channel-put ch v)]
would not block, and the event's synchronization result is the event
itself.}
