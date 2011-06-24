#lang scribble/doc
@(require "mz.rkt" (for-label racket/async-channel))

@(define async-eval
   (lambda ()
     (let ([the-eval (make-base-eval)])
       (the-eval '(require racket/async-channel))
       the-eval)))

@title[#:tag "async-channel"]{Buffered Asynchronous Channels}

@note-lib-only[racket/async-channel]

@margin-note/ref{See also @secref["threadmbox"].}

@defproc[(async-channel? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an asynchronous channel,
@racket[#f] otherwise.}


@defproc[(make-async-channel [limit (or/c exact-positive-integer? #f) #f]) 
         async-channel?]{

Returns an asynchronous channel with a buffer limit of @racket[limit]
items. A get operation blocks when the channel is empty, and a put
operation blocks when the channel has @racket[limit] items already.
If @racket[limit] is @racket[#f], the channel buffer has no limit (so
a put never blocks).

The asynchronous channel value can be used directly with
@racket[sync]. The channel blocks until @racket[async-channel-get]
would return a value, and the unblock result is the received value.}


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

Returns a @tech{synchronizable event} that is blocked while
@racket[(async-channel-put ach v)] would block. The unblock result is
the event itself. See also @racket[sync].}

@defexamples[#:eval (async-eval)
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
(define from-server (make-async-channel))

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
