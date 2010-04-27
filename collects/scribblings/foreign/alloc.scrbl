#lang scribble/doc
@(require "utils.ss"
          (for-label ffi/unsafe/alloc
                     ffi/unsafe/define
                     ffi/unsafe/atomic))

@title{Allocation and Finalization}

@defmodule[ffi/unsafe/alloc]{The
@schememodname[ffi/unsafe/alloc] library provides utilities for
ensuring that values allocated through foreign functions are reliably
deallocated.}

@defproc[((allocator [dealloc (any/c . -> . any)]) [alloc procedure?]) procedure?]{

Produces a procedure that behaves like @scheme[alloc], but the result
of @scheme[alloc] is given a finalizer that calls @scheme[dealloc] on
the result if it is not otherwise freed through a deallocator (as
designated with @scheme[deallocator]). In addition, @scheme[alloc] is
called in atomic mode (see @scheme[start-atomic]); its result is
received and registered in atomic mode, so that the result is reliably
freed.

The @scheme[dealloc] procedure itself need not be specifically
designated a deallocator (via @scheme[deallocator]). If a deallocator
is called explicitly, it need not be the same as @scheme[dealloc].}

@deftogether[(
@defproc[((deallocator [get-arg (list? . -> . any/c) car]) [dealloc procedure?]) 
         procedure?]
@defproc[((releaser [get-arg (list? . -> . any/c) car]) [dealloc procedure?]) 
         procedure?]
)]{

Produces a procedure that behaves like @scheme[dealloc]. The
@scheme[dealloc] procedure is called in atomic mode (see
@scheme[start-atomic]), and the reference count on one of its
arguments is decremented; if the reference count reaches zero, no
finalizer associated by an @scheme[allocator]- or
@scheme[referencer]-wrapped procedure is invoked when the value
becomes inaccessible.

The optional @scheme[get-arg] procedure determines which of
@scheme[dealloc]'s arguments correspond to the released object;
@scheme[get-arg] receives a list of arguments passed to
@scheme[dealloc], so the default @scheme[car] selects the first one.

The @scheme[releaser] procedure is a synonym for
@scheme[deallocator].}


@defproc[((retainer [release (any/c . -> . any)]
                    [get-arg (list? . -> . any/c) car]) 
          [retain procedure?]) 
         procedure?]{

Produces a procedure that behaves like @scheme[retain]. The procedure
is called in atomic mode (see @scheme[start-atomic]), and the
reference count on one of its arguments is incremented, with
@scheme[release] recorded as the corresponding release procedure to be
called by the finalizer on the retained object (unless some
deallocator, as wrapped by @scheme[deallocate], is explicitly called
first).

The optional @scheme[get-arg] procedure determines which of
@scheme[retain]'s arguments correspond to the retained object;
@scheme[get-arg] receives a list of arguments passed to
@scheme[retain], so the default @scheme[car] selects the first one.

The @scheme[release] procedure itself need not be specifically
designated a deallocator (via @scheme[deallocator]). If a deallocator
is called explicitly, it need not be the same as @scheme[release].}
