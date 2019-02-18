#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/alloc ffi/unsafe/define ffi/unsafe/atomic))

@title{Allocation and Finalization}

@defmodule[ffi/unsafe/alloc]{The
@racketmodname[ffi/unsafe/alloc] library provides utilities for
ensuring that values allocated through foreign functions are reliably
deallocated.}

@defproc[((allocator [dealloc (any/c . -> . any)]) [alloc procedure?]) procedure?]{

Produces an @deftech{allocator} procedure that behaves like
@racket[alloc], but each result @racket[_v] of the @tech{allocator},
if not @racket[#f], is given a finalizer that calls @racket[dealloc]
on @racket[_v] --- unless the call has been canceled by applying a
@tech{deallocator} (produced by @racket[deallocator]) to @racket[_v].
Any existing @racket[dealloc] registered for @racket[_v] is canceled.

The resulting @tech{allocator} calls @racket[alloc] in @tech{atomic
mode} (see @racket[call-as-atomic]). The result from @racket[alloc] is
received and registered in atomic mode, so that the result is reliably
deallocated as long as no exception is raised.

The @racket[dealloc] procedure will be called in atomic mode, and it
must obey the same constraints as a finalizer procedure provided to
@racket[register-finalizer]. The @racket[dealloc] procedure itself
need not be specifically a @tech{deallocator} produced by
@racket[deallocator]. If a @tech{deallocator} is called explicitly, it
need not be the same as @racket[dealloc].

When a non-main @tech[#:doc reference.scrbl]{place} exits, after all
@tech[#:doc reference.scrbl]{custodian}-shutdown actions, for every
@racket[dealloc] still registered via an @tech{allocator} or
@tech{retainer} (from @racket[allocator] or @racket[retainer]), the
value to deallocate is treated as immediately unreachable. At that
point, @racket[dealloc] functions are called in reverse order of their
registrations. Note that references in a @racket[dealloc] function's
closure do @emph{not} prevent running a @racket[dealloc] function for
any other value. If deallocation needs to proceed in an order
different than reverse of allocation, use a @tech{retainer} to insert
a new deallocation action that will run earlier.

@history[#:changed "7.0.0.4" @elem{Added atomic mode for @racket[dealloc]
                                   and changed non-main place exits to call
                                   all remaining @racket[dealloc]s.}]}

@deftogether[(
@defproc[((deallocator [get-arg (list? . -> . any/c) car]) [dealloc procedure?]) 
         procedure?]
@defproc[((releaser [get-arg (list? . -> . any/c) car]) [dealloc procedure?]) 
         procedure?]
)]{

Produces a @deftech{deallocator} procedure that behaves like
@racket[dealloc]. The @tech{deallocator} calls @racket[dealloc] in
@tech{atomic mode} (see @racket[call-as-atomic]), and for one of its
arguments, the it cancels the most recent remaining deallocator
registered by a @tech{allocator} or @tech{retainer}.

The optional @racket[get-arg] procedure determines which of
@racket[dealloc]'s arguments correspond to the released object;
@racket[get-arg] receives a list of arguments passed to
@racket[dealloc], so the default @racket[car] selects the first one.
Note that @racket[get-arg] can only choose one of the by-position
arguments to @racket[dealloc], though the @tech{deallocator} will
require and accept the same keyword arguments as @racket[dealloc], if any.

The @racket[releaser] procedure is a synonym for
@racket[deallocator].}


@defproc[((retainer [release (any/c . -> . any)]
                    [get-arg (list? . -> . any/c) car]) 
          [retain procedure?]) 
         procedure?]{

Produces a @deftech{retainer} procedure that behaves like
@racket[retain]. A @tech{retainer} acts the same as an
@tech{allocator} produced by @racket[allocator], except that

@itemlist[

 @item{a @tech{retainer} does not cancel any existing @racket[release]
       or @racket[_dealloc] registrations when registering
       @racket[release]; and}

 @item{@racket[release] is registered for a value @racket[_v] that is
       is an argument to the @tech{retainer}, instead of the result
       for an @tech{allocator}.}

]

The optional @racket[get-arg] procedure determines which of the
@tech{retainer}'s arguments (that is, which of @racket[retain]'s
arguments) correspond to the retained object @racket[_v];
@racket[get-arg] receives a list of arguments passed to
@racket[retain], so the default @racket[car] selects the first one.
Note that @racket[get-arg] can only choose one of the by-position
arguments to @racket[retain], though the @tech{retainer} will
require and accept the same keyword arguments as @racket[retain], if any.

@history[#:changed "7.0.0.4" @elem{Added atomic mode for @racket[release]
                                   and changed non-main place exits to call
                                   all remaining @racket[release]s.}]}
