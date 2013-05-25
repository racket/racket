#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "foreign:cvector"]{Safe C Vectors}

@defmodule*[(ffi/cvector ffi/unsafe/cvector) 
            #:use-sources (ffi/unsafe/cvector)]{The
@racketmodname[ffi/unsafe/cvector] library exports the bindings of
this section. The @racketmodname[ffi/cvector] library exports the same
bindings, except for the unsafe @racket[make-cvector*] operation.}

The @racket[cvector] form can be used as a type C vectors (i.e., a
pointer to a memory block).

@defform*[[(_cvector mode type maybe-len)
           _cvector]]{

Like @racket[_bytes], @racket[_cvector] can be used as a simple type
that corresponds to a pointer that is managed as a safe C vector on
the Racket side.  The longer form behaves similarly to the
@racket[_list] and @racket[_vector] custom types, except that
@racket[_cvector] is more efficient; no Racket list or vector is
needed.}

@defproc[(make-cvector [type ctype?] [length exact-nonnegative-integer?]) cvector?]{

Allocates a C vector using the given @racket[type] and
@racket[length]. The resulting vector is not guaranteed to 
contain any particular values.}


@defproc[(cvector [type ctype?] [val any/c] ...) cvector?]{

Creates a C vector of the given @racket[type], initialized to the
given list of @racket[val]s.}


@defproc[(cvector? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a C vector, @racket[#f] otherwise.}


@defproc[(cvector-length [cvec cvector?]) exact-nonnegative-integer?]{

Returns the length of a C vector.}


@defproc[(cvector-type [cvec cvector?]) ctype?]{

Returns the C type object of a C vector.}


@defproc[(cvector-ptr [cvec cvector?]) cpointer?]{

Returns the pointer that points at the beginning block of the given C vector.}


@defproc[(cvector-ref [cvec cvector?] [k exact-nonnegative-integer?]) any]{

References the @racket[k]th element of the @racket[cvec] C vector.
The result has the type that the C vector uses.}


@defproc[(cvector-set! [cvec cvector?] [k exact-nonnegative-integer?] [val any]) void?]{

Sets the @racket[k]th element of the @racket[cvec] C vector to
@racket[val].  The @racket[val] argument should be a value that can be
used with the type that the C vector uses.}


@defproc[(cvector->list [cvec cvector?]) list?]{

Converts the @racket[cvec] C vector object to a list of values.}


@defproc[(list->cvector [lst list?] [type ctype?]) cvector?]{

Converts the list @racket[lst] to a C vector of the given
@racket[type].}


@defproc[(make-cvector* [cptr any/c] [type ctype?]
                        [length exact-nonnegative-integer?])
                        cvector?]{

Constructs a C vector using an existing pointer object.  This
operation is not safe, so it is intended to be used in specific
situations where the @racket[type] and @racket[length] are known.}


