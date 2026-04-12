#lang scribble/manual
@(require "common.rkt"
          (for-label
           (only-in ffi/unsafe
                    register-finalizer)))

@title{Interoperability with @racketmodname[ffi/unsafe]}

The @racketmodname[ffi2] and @racketmodname[ffi/unsafe] libraries can
coexist within a single Racket application. Libraries like
@racketmodname[ffi/unsafe/alloc] and @racketmodname[ffi/unsafe/atomic],
and the procedure @racket[register-finalizer] form @racketmodname[ffi/unsafe]
remain particularly useful in an application that otherwise uses
@racketmodname[ffi2].

The main incompatibility between @racketmodname[ffi2] and
@racketmodname[ffi/unsafe] is that they have different pointer
representations, but @racket[ptr_t->cpointer] and
@racket[cpointer->ptr_t] support conversions between the
representations.

@deftogether[(
@defproc[(ptr_t->cpointer [ptr ptr_t?]) cpointer?]
@defproc[(cpointer->ptr_t [cptr cpointer?]) ptr_t?]
)]{

The @racket[ptr_t->cpointer] procedure converts a @tech{pointer}
object for @racketmodname[ffi2] to a ``C pointer'' for
@racketmodname[ffi/unsafe]. The @racket[cpointer->ptr_t] function
converts in the opposite direction.

}
