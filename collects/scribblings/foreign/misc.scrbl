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

@defproc[(saved-errno) exact-integer?]{

Returns the value most recently saved (in the current thread) after a
foreign call with a non-@racket[#f] @racket[#:save-errno] option (see
@racket[_fun] and @racket[_cprocedure]).}

@defproc[(lookup-errno [sym (or/c 'EINTR 'EEXIST 'EAGAIN)])
         exact-integer?]{

Returns a platform-specific value corresponding to a Posix @tt{errno}
symbol. The set of supported symbols is likely to expand in the
future.}


@defproc[(cast [v any/c] [from-type ctype?] [to-type ctype?]) any/c]{

Converts @racket[v] from a value matching @racket[from-type] to a
value matching @racket[to-type], where @racket[(ctype-sizeof from-type)]
matches @racket[(ctype-sizeof to-type)].

The conversion is equivalent to

@racketblock[
  (let ([p (malloc from-type)])
    (ptr-set! p from-type v)
    (ptr-ref p to-type))
]

Beware of potential pitfalls with @racket[cast]:

@itemlist[

 @item{If @racket[v] is a pointer that refers to memory that is
       managed by the garbage collector, @racket[from-type] and
       @racket[to-type] normally should be based on
       @racket[_gcpointer], not @racket[_pointer]; see also
       @racket[_gcable].}

 @item{If @racket[v] is a pointer with an offset component (e.g., from
       @racket[ptr-add]), the offset is folded into the pointer base
       for the result. Consequently, @racket[cast] generally should
       not be used on a source pointer that refers to memory that is
       managed by the garbage collector and that has an offset, unless
       the memory is specially allocated to allow interior pointers.}

]}


@defproc[(cblock->list [cblock any/c] [type ctype?] [length exact-nonnegative-integer?])
         list?]{

Converts C @racket[cblock], which is a vector of @racket[type]s, to a
Racket list.  The arguments are the same as in the
@racket[list->cblock]. The @racket[length] must be specified because
there is no way to know where the block ends.}


@defproc[(cblock->vector [cblock any/c] [type ctype?] [length exact-nonnegative-integer?])
         vector?]{

Like @racket[cblock->vector], but for Racket vectors.}
