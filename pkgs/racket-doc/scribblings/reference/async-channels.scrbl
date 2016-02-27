#lang scribble/doc
@(require "mz.rkt" (for-label racket/async-channel))

@(define async-eval
   (lambda ()
     (let ([the-eval (make-base-eval)])
       (the-eval '(require racket/async-channel))
       the-eval)))

@title[#:tag "async-channel"]{Buffered Asynchronous Channels}

@note-lib-only[racket/async-channel]

@section{Creating and Using Asynchronous Channels}

@margin-note/ref{See also @secref["threadmbox"].}

An @deftech{asynchronous channel} is like a @tech{channel}, but it buffers
values so that a send operation does not wait on a receive operation.

In addition to its use with procedures that are specific to
asynchronous channels, an asynchronous channel can be used as a
@tech{synchronizable event} (see @secref["sync"]).  An asynchronous
channel is @tech{ready for synchronization} when
@racket[async-channel-get] would not block; the asynchronous channel's
@tech{synchronization result} is the same as the
@racket[async-channel-get] result.

@defproc[(async-channel? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an asynchronous channel,
@racket[#f] otherwise.}


@defproc[(make-async-channel [limit (or/c exact-positive-integer? #f) #f]) 
         async-channel?]{

Returns an asynchronous channel with a buffer limit of @racket[limit]
items. A get operation blocks when the channel is empty, and a put
operation blocks when the channel has @racket[limit] items already.
If @racket[limit] is @racket[#f], the channel buffer has no limit (so
a put never blocks).}


@defproc[(async-channel-get [ach async-channel?]) any/c]{

Blocks until at least one value is available in @racket[ach], and then
returns the first of the values that were put into
@racket[async-channel].}


@defproc[(async-channel-try-get [ach async-channel?]) any/c]{

If at least one value is immediately available in @racket[ach],
returns the first of the values that were put into @racket[ach]. If
@racket[async-channel] is empty, the result is @racket[#f].}


@defproc[(async-channel-put [ach async-channel?] [v any/c]) void?]{

Puts @racket[v] into @racket[ach], blocking if @racket[ach]'s buffer
is full until space is available.}


@defproc[(async-channel-put-evt [ach async-channel?] [v any/c]) 
         evt?]{

Returns a @tech{synchronizable event} that is @tech{ready for
synchronization} when @racket[(async-channel-put ach v)] would return
a value (i.e., when the channel holds fewer values already than its
limit); @resultItself{asychronous channel-put event}.}

@examples[#:eval (async-eval) #:once
(eval:no-prompt
 (define (server input-channel output-channel)
   (thread (lambda ()
             (define (get)
               (async-channel-get input-channel))
             (define (put x)
               (async-channel-put output-channel x))
             (define (do-large-computation)
               (sqrt 9))
             (let loop ([data (get)])
               (case data
                 [(quit) (void)]
                 [(add) (begin
                          (put (+ 1 (get)))
                          (loop (get)))]
                 [(long) (begin
                           (put (do-large-computation))
                           (loop (get)))])))))
 (define to-server (make-async-channel))
 (define from-server (make-async-channel)))

(server to-server from-server)

(async-channel? to-server)
(printf "Adding 1 to 4\n")
(async-channel-put to-server 'add)
(async-channel-put to-server 4)
(printf "Result is ~a\n" (async-channel-get from-server))
(printf "Ask server to do a long computation\n")
(async-channel-put to-server 'long)
(printf "I can do other stuff\n")
(printf "Ok, computation from server is ~a\n" 
        (async-channel-get from-server))
(async-channel-put to-server 'quit)
]

@section{Contracts and Impersonators on Asynchronous Channels}

@defproc[(async-channel/c [c contract?]) contract?]{

Returns a contract that recognizes asynchronous channels. Values put into or
retrieved from the channel must match @racket[c].

If the @racket[c] argument is a flat contract or a chaperone contract, then the
result will be a chaperone contract. Otherwise, the result will be an
impersonator contract.

When an @racket[async-channel/c] contract is applied to an asynchronous channel,
the result is not @racket[eq?] to the input. The result will be either a
@tech{chaperone} or @tech{impersonator} of the input depending on the type of
contract.}

@defproc[(impersonate-async-channel [channel async-channel?]
                                    [get-proc (any/c . -> . any/c)]
                                    [put-proc (any/c . -> . any/c)]
                                    [prop impersonator-property?]
                                    [prop-val any] ...
                                    ...)
         (and/c async-channel? impersonator?)]{

Returns an impersonator of @racket[channel], which redirects the
@racket[async-channel-get] and @racket[async-channel-put] operations.

The @racket[get-proc] must accept the value that @racket[async-channel-get]
produces on @racket[channel]; it must produce a replacement value, which is the
result of the get operation on the impersonator.

The @racket[put-proc] must accept the value passed to @racket[async-channel-put]
called on @racket[channel]; it must produce a replacement value, which is the
value passed to the put procedure called on the original channel.

The @racket[get-proc] and @racket[put-proc] procedures are called for all
operations that get or put values from the channel, not just
@racket[async-channel-get] and @racket[async-channel-put].

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments to
@racket[impersonate-async-channel] must be odd) add
@tech{impersonator properties} or override @tech{impersonator property} values
of @racket[channel].}

@defproc[(chaperone-async-channel [channel async-channel?]
                                  [get-proc (any/c . -> . any/c)]
                                  [put-proc (any/c . -> . any/c)]
                                  [prop impersonator-property?]
                                  [prop-val any] ...
                                  ...)
         (and/c async-channel? chaperone?)]{

Like @racket[impersonate-async-channel], but the @racket[get-proc] procedure
must produce the same value or a chaperone of the original value, and
@racket[put-proc] must produce the same value or a chaperone of the original
value.}
