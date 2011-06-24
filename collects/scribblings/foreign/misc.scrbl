#lang scribble/doc
@(require "utils.rkt")

@title{Miscellaneous Support}


@defproc[(regexp-replaces [objname (or/c string? bytes? symbol?)]
                          [substs (listof (list regexp? string?))])
         string?]{

A function that is convenient for many interfaces where the foreign
library has some naming convention that you want to use in your
interface as well.  The @racket[objname] argument can be any value
that will be used to name the foreign object; it is first converted
into a string, and then modified according to the given
@racket[substs] list in sequence, where each element in this list is a
list of a regular expression and a substitution string.  Usually,
@racket[regexp-replace*] is used to perform the substitution, except
for cases where the regular expression begins with a @litchar{^} or
ends with a @litchar{$}, in which case @racket[regexp-replace] is
used.

For example, the following makes it convenient to define Racket
bindings such as @racket[foo-bar] for foreign names like
@racket[MyLib_foo_bar]:

@racketblock[
(define mylib (ffi-lib "mylib"))
(define-syntax defmyobj
  (syntax-rules (:)
    [(_ name : type ...)
     (define name
       (get-ffi-obj 
        (regexp-replaces 'name '((#rx"-" "_") 
                                 (#rx"^" "MyLib_")))
        mylib (_fun type ...)))]))
(defmyobj foo-bar : _int -> _int)
]}


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
