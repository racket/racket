#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/collect-callback))

@title{Garbage Collection Callbacks}

@defmodule[ffi/unsafe/collect-callback]{The
@racketmodname[ffi/unsafe/collect-callback] library provides functions
to register constrained callbacks that are run just before and after a
garbage collection.}

@history[#:added "7.0.0.9"]


@defproc[(unsafe-add-collect-callbacks [pre (vectorof vector?)]
                                       [post (vectorof vector?)])
         any/c]{

Registers descriptions of foreign functions to be called just before
and just after a garbage collection. The foreign functions must not
allocate garbage-collected memory, and they are called in a way that
does not allocate, which is why @var{pre_desc} and @var{post_desc} are
function descriptions instead of thunks.

A description is a vector of vectors, where each of the inner vectors
describes a single call, and the calls are performed in sequence. Each
call vector starts with a symbol that indicates the protocol of the
foreign function to be called. The following protocols are supported:
@margin-note*{The apparently arbitrary and whimsical set of supported
protocols is enough to allow DrRacket to show a garbage-collection
icon.}

@itemlist[

 @item{@racket['int->void] corresponds to @cpp{void (*)(int)}.}

 @item{@racket['ptr_ptr_ptr->void] corresponds to @cpp{void
 (*)(void*, void*, void*)}.}

 @item{@racket['ptr_ptr->save] corresponds to @cpp{void* (*)(void*,
 void*, void*)}, but the result is recored as the current ``save''
 value. The current ``save'' value starts as @cpp{NULL}.}

 @item{@racket['save!_ptr->void] corresponds to @cpp{void (*)(void*,
 void*)}, but only if the current ``save'' value is not a @cpp{NULL}
 pointer, and passing that pointer as the function's first argument
 (so that only one additional argument is us from the description
 vector).}

 @item{@racket['ptr_ptr_ptr_int->void] corresponds to @cpp{void
 (*)(void*, void*, void*, int)}.}

 @item{@racket['ptr_ptr_float->void] corresponds to @cpp{void
 (*)(void*, void*, float)}.}

 @item{@racket['ptr_ptr_double->void] corresponds to @cpp{void
 (*)(void*, void*, double)}.}

 @item{@racket['ptr_ptr_ptr_int_int_int_int_int_int_int_int_int->void]
 corresponds to @cpp{void (*)(void*, void*, void*, int, int, int, int,
 int, int, int, int, int)}.}

 @item{@racket['osapi_ptr_int->void] corresponds to @cpp{void
 (*)(void*, int)}, but using the stdcall calling convention
 on Windows.}

 @item{@racket['osapi_ptr_ptr->void] corresponds to @cpp{void
 (*)(void*, void*)}, but using the stdcall calling convention
 on Windows.}

 @item{@racket['osapi_ptr_int_int_int_int_ptr_int_int_long->void]
 corresponds to @cpp{void (*)(void*, int, int, int, int, void*,
 int, int, long)}, but using the stdcall calling convention
 on Windows.}

]

After the protocol symbol, the vector should contain a pointer to a
foreign function and then an element for each of the function's
arguments. Pointer values are represented as for the @racket[_pointer]
representation defined by @racketmodname[ffi/unsafe].

The result is a key for use with @racket[unsafe-remove-collect-callbacks]. If
the key becomes inaccessible, then the callback will be removed
automatically (but beware that the pre-callback will have executed and
the post-callback will not have executed)

}

@defproc[(unsafe-remove-collect-callbacks [key any/c]) void?]{

Unregisters pre- and post-collection callbacks that were previously
registered by a call to @racket[unsafe-add-collect-callbacks] that
returned @racket[v].}
