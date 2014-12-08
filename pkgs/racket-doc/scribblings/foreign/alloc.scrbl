#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/alloc ffi/unsafe/define ffi/unsafe/atomic))

@title{Allocation and Finalization}

@defmodule[ffi/unsafe/alloc]{The
@racketmodname[ffi/unsafe/alloc] library provides utilities for
ensuring that values allocated through foreign functions are reliably
deallocated.}

@defproc[((allocator [dealloc (any/c . -> . any)]) [alloc procedure?]) procedure?]{

Produces a procedure that behaves like @racket[alloc], but the result
of @racket[alloc] is given a finalizer that calls @racket[dealloc] on
the result if it is not otherwise freed through a deallocator (as
designated with @racket[deallocator]). In addition, @racket[alloc] is
called in @tech{atomic mode} (see @racket[call-as-atomic]); its result is
received and registered in atomic mode, so that the result is reliably
freed as long as no exception is raised.

The @racket[dealloc] procedure itself need not be specifically
designated a deallocator (via @racket[deallocator]). If a deallocator
is called explicitly, it need not be the same as @racket[dealloc].}

@deftogether[(
@defproc[((deallocator [get-arg (list? . -> . any/c) car]) [dealloc procedure?]) 
         procedure?]
@defproc[((releaser [get-arg (list? . -> . any/c) car]) [dealloc procedure?]) 
         procedure?]
)]{

Produces a procedure that behaves like @racket[dealloc]. The
@racket[dealloc] procedure is called in @tech{atomic mode} (see
@racket[call-as-atomic]), and the reference count on one of its
arguments is decremented; if the reference count reaches zero, no
finalizer associated by an @racket[allocator]- or
@racket[retainer]-wrapped procedure is invoked when the value
becomes inaccessible.

The optional @racket[get-arg] procedure determines which of
@racket[dealloc]'s arguments correspond to the released object;
@racket[get-arg] receives a list of arguments passed to
@racket[dealloc], so the default @racket[car] selects the first one.

The @racket[releaser] procedure is a synonym for
@racket[deallocator].}


@defproc[((retainer [release (any/c . -> . any)]
                    [get-arg (list? . -> . any/c) car]) 
          [retain procedure?]) 
         procedure?]{

Produces a procedure that behaves like @racket[retain]. The procedure
is called in @tech{atomic mode} (see @racket[call-as-atomic]), and the
reference count on one of its arguments is incremented, with
@racket[release] recorded as the corresponding release procedure to be
called by the finalizer on the retained object (unless some
deallocator, as wrapped by @racket[deallocator], is explicitly called
first).

The optional @racket[get-arg] procedure determines which of
@racket[retain]'s arguments correspond to the retained object;
@racket[get-arg] receives a list of arguments passed to
@racket[retain], so the default @racket[car] selects the first one.

The @racket[release] procedure itself need not be specifically
designated a deallocator (via @racket[deallocator]). If a deallocator
is called explicitly, it need not be the same as @racket[release].}
