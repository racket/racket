#lang scribble/doc
@(require "mz.ss"
          (for-label scheme/async-channel))

@title[#:tag "async-channel"]{Buffered Asynchronous Channels}

@note-lib-only[scheme/async-channel]

@margin-note/ref{See also @secref["threadmbox"].}

@defproc[(async-channel? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an asynchronous channel,
@scheme[#f] otherwise.}


@defproc[(make-async-channel [limit (or/c exact-positive-integer? false/c) #f]) 
         async-channel?]{

Returns an asynchronous channel with a buffer limit of @scheme[limit]
items. A get operation blocks when the channel is empty, and a put
operation blocks when the channel has @scheme[limit] items already.
If @scheme[limit] is @scheme[#f], the channel buffer has no limit (so
a put never blocks).

The asynchronous channel value can be used directly with
@scheme[sync]. The channel blocks until @scheme[async-channel-get]
would return a value, and the unblock result is the received value.}


@defproc[(async-channel-get [ach async-channel?]) any/c]{

Blocks until at least one value is available in @scheme[ach], and then
returns the first of the values that were put into
@scheme[async-channel].}


@defproc[(async-channel-try-get [ach async-channel?]) any/c]{

If at least one value is immediately available in @scheme[ach],
returns the first of the values that were put into @scheme[ach]. If
@scheme[async-channel] is empty, the result is @scheme[#f].}


@defproc[(async-channel-put [ach async-channel?][v any/c]) void?]{

Puts @scheme[v] into @scheme[ach], blocking if @scheme[ach]'s buffer
is full until space is available.}


@defproc[(async-channel-put-evt [async-channel channel?][v any/c]) 
         evt?]{

Returns a @tech{synchronizable event} that is blocked while
@scheme[(async-channel-put ach v)] would block. The unblock result is
the event itself. See also @scheme[sync].}
