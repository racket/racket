#lang scribble/doc
@(require "utils.rkt")

@title{Miscellaneous Support}

@defproc[(list->cblock [lst list?] [type ctype?]) any]{

Allocates a memory block of an appropriate size, and initializes it
using values from @racket[lst] and the given @racket[type].  The
@racket[lst] must hold values that can all be converted to C values
according to the given @racket[type].}


@defproc[(vector->cblock [vec vector?] [type type?]) any]{

Like @racket[list->cblock], but for Racket vectors.}


@defproc[(vector->cpointer [vec vector?]) cpointer?]{

Returns a pointer to an array of @racket[_scheme] values, which is the
internal representation of @racket[vec].}

@defproc[(flvector->cpointer [flvec flvector?]) cpointer?]{

Returns a pointer to an array of @racket[_double] values, which is the
internal representation of @racket[flvec].}

@defproc*[([(saved-errno) exact-integer?]
           [(saved-errno [new-value exact-integer?]) void?])]{

Returns or sets the error code saved for the current Racket
thread. The saved error code is set after a foreign call with a
non-@racket[#f] @racket[#:save-errno] option (see @racket[_fun] and
@racket[_cprocedure]), but it can also be set explicitly (for example,
to create mock foreign functions for testing).

@history[#:changed "6.4.0.9"]{Added the one-argument variant.}}

@defproc[(lookup-errno [sym (or/c 'EINTR 'EEXIST 'EAGAIN)])
         exact-integer?]{

Returns a platform-specific value corresponding to a Posix @tt{errno}
symbol. The set of supported symbols is likely to expand in the
future.}


@defproc[(cast [v any/c] [from-type ctype?] [to-type ctype?]) any/c]{

Converts @racket[v] from a value matching @racket[from-type] to a
value matching @racket[to-type], where @racket[(ctype-sizeof from-type)]
matches @racket[(ctype-sizeof to-type)].

The conversion is roughly equivalent to

@racketblock[
  (let ([p (malloc from-type)])
    (ptr-set! p from-type v)
    (ptr-ref p to-type))
]

If @racket[v] is a cpointer, @racket[(cpointer-gcable?  v)] is true,
and @racket[from-type] and @racket[to-type] are both based on
@racket[_pointer] or @racket[_gcpointer], then @racket[from-type] is
implicitly converted with @racket[_gcable] to ensure that the result
cpointer is treated as referring to memory that is managed by the
garbage collector.

If @racket[v] is a pointer with an offset component (e.g., from
@racket[ptr-add]), @racket[(cpointer-gcable? v)] is true, and the
result is a cpointer, then the result pointer has the same offset
component as @racket[v]. If @racket[(cpointer-gcable? v)] is false,
then any offset is folded into the pointer base for the result.}


@defproc[(cblock->list [cblock any/c] [type ctype?] [length exact-nonnegative-integer?])
         list?]{

Converts C @racket[cblock], which is a vector of @racket[type]s, to a
Racket list.  The arguments are the same as in the
@racket[list->cblock]. The @racket[length] must be specified because
there is no way to know where the block ends.}


@defproc[(cblock->vector [cblock any/c] [type ctype?] [length exact-nonnegative-integer?])
         vector?]{

Like @racket[cblock->vector], but for Racket vectors.}
