#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/os-thread
                     ffi/unsafe/os-async-channel))

@title{Operating System Threads}

@defmodule[ffi/unsafe/os-thread]{The
@racketmodname[ffi/unsafe/os-thread] library provides functions for
running constrained Racket code in a separate thread at the
operating-system level. Except for @racket[os-thread-enabled?], the
functions of @racketmodname[ffi/unsafe/os-thread] are currently
supported only when @racket[(system-type 'vm)] returns
@racket['chez-scheme], and even then only in certain build modes. The
functions raise @racket[exn:fail:unsupported] when not supported.}

@history[#:added "6.90.0.9"]


@defproc[(os-thread-enabled?) boolean?]{

Returns @racket[#t] if the other functions of
@racketmodname[ffi/unsafe/os-thread] work without raising
@racket[exn:fail:unsupported], @racket[#f] otherwise.}


@defproc[(call-in-os-thread [thunk (-> any)]) void?]{

Runs @racket[thunk] in a separate operating-system thread, which runs
concurrently to all Racket threads.

The @racket[thunk] is run in @tech{atomic mode}, and it must not
inspect its continuation or use any Racket thread functions (such as
@racket[thread] or @racket[current-thread]), any Racket
synchronization functions (such as @racket[semaphore-post] or
@racket[sync]), or any parameters (such as
@racket[current-output-port]). Variables may be safely mutated with
@racket[set!], and vectors, mutable pairs, boxes, mutable structure
fields, and @racket[eq?]- and @racket[eqv?]-based hash tables can be
mutated, but the visibility of mutations to other threads is
unspecified except as synchronized through @racket[os-semaphore-wait]
and @racket[os-semaphore-post].}


@defproc[(make-os-semaphore) any]{

Creates a semaphore that can be used with @racket[os-semaphore-wait]
and @racket[os-semaphore-post] to synchronize an operating-system
thread with Racket threads and other operating-system threads.}


@defproc[(os-semaphore-post [sema any/c]) void?]{

Analogous to @racket[semaphore-post], but posts to a semaphore created
by @racket[make-os-semaphore].}


@defproc[(os-semaphore-wait [sema any/c]) void?]{

Analogous to @racket[semaphore-wait], but waits on a semaphore created
by @racket[make-os-semaphore]. Waiting blocks the current thread; if
the current thread is a Racket thread, then waiting also blocks all
Racket threads.}

@; ----------------------------------------

@section{Operating System Asynchronous Channels}

@defmodule[ffi/unsafe/os-async-channel]{The
@racketmodname[ffi/unsafe/os-async-channel] library provides an
asynchronous channels that work with operating-system threads, where
normal racket channels or place channels are not allowed. These
channels are typically used in combination with
@racketmodname[ffi/unsafe/os-thread].}

An asynchronous operating-system channel is a @tech[#:doc
reference.scrbl]{synchronizable event}, so can it can be used with
@racket[sync] to receive a value in a Racket thread. Other threads
must use @racket[os-async-channel-try-get] or
@racket[os-async-channel-get].

When a thread is blocked on an otherwise inaccessible asynchronous
channel that was produced by @racket[make-os-async-channel], the
thread is @emph{not} available for garbage collection. That's
different from a thread is blocked on a regular Racket channel or a
place channel.

@history[#:added "8.0.0.4"]

@defproc[(make-os-async-channel) os-async-channel?]{

Creates a new, empty asynchronous channel for use with
operating-system threads.}


@defproc[(os-async-channel? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an asynchronous channel produced
by @racket[make-os-async-channel], @racket[#f] otherwise.}

@defproc[(os-async-channel-put [ch os-async-channel?] [v any/c]) void?]{

Enqueues @racket[v] in the asynchronous channel @racket[ch]. This
function can be called from a Racket thread or any operating-system
thread.}

@defproc[(os-async-channel-try-get [ch os-async-channel?] [default-v any/c #f]) any/c]{

Dequeues a value from the the asynchronous channel @racket[ch] and
returns it, if a value is available. If no value is immediately
available in the channel, @racket[default-v] is returned. This
function can be called from a Racket thread or any operating-system
thread.}

@defproc[(os-async-channel-get [ch os-async-channel?]) any/c]{

Dequeues a value from the the asynchronous channel @racket[ch] and
returns it, blocking until a value is available. This function can be
called from any non-Racket operating-system thread. This function
should @emph{not} be called from a Racket thread, since it blocks in a
way that will block all Racket threads within a place; in a Racket
thread, use @racket[sync], instead. }
