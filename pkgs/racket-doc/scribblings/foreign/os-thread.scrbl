#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/os-thread))

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

