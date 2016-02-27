#lang scribble/doc
@(require "mz.rkt" racket/math scribble/extract
          (for-label racket/math
                     racket/flonum
                     racket/fixnum
                     racket/unsafe/ops
                     racket/require))

@(define flfx-eval (make-base-eval))
@examples[#:hidden #:eval flfx-eval (require racket/fixnum)]


@title[#:tag "fixnums"]{Fixnums}

@defmodule[racket/fixnum]

The @racketmodname[racket/fixnum] library provides operations like
@racket[fx+] that consume and produce only fixnums. The operations in
this library are meant to be safe versions of unsafe operations like
@racket[unsafe-fx+]. These safe operations are generally no faster
than using generic primitives like @racket[+].

The expected use of the @racketmodname[racket/fixnum] library is for
code where the @racket[require] of @racketmodname[racket/fixnum] is
replaced with

@racketblock[(require (filtered-in
                       (λ (name) (regexp-replace #rx"unsafe-" name ""))
                       racket/unsafe/ops))]

to drop in unsafe versions of the library. Alternately, when
encountering crashes with code that uses unsafe fixnum operations, use
the @racketmodname[racket/fixnum] library to help debug the problems.

@; ------------------------------------------------------------

@section{Fixnum Arithmetic}

@deftogether[(
@defproc[(fx+ [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fx- [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fx* [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxquotient  [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxremainder [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxmodulo    [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxabs       [a fixnum?]) fixnum?]
)]{

Safe versions of @racket[unsafe-fx+], @racket[unsafe-fx-],
@racket[unsafe-fx*], @racket[unsafe-fxquotient],
@racket[unsafe-fxremainder], @racket[unsafe-fxmodulo], and
@racket[unsafe-fxabs]. The
@exnraise[exn:fail:contract:non-fixnum-result] if the arithmetic
result would not be a fixnum.}


@deftogether[(
@defproc[(fxand [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxior [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxxor [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxnot [a fixnum?]) fixnum?]
@defproc[(fxlshift [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxrshift [a fixnum?] [b fixnum?]) fixnum?]
)]{

Safe versions of @racket[unsafe-fxand], @racket[unsafe-fxior],
@racket[unsafe-fxxor], @racket[unsafe-fxnot],
@racket[unsafe-fxlshift], and @racket[unsafe-fxrshift].  The
@exnraise[exn:fail:contract:non-fixnum-result] if the arithmetic
result would not be a fixnum.}


@deftogether[(
@defproc[(fx=   [a fixnum?] [b fixnum?]) boolean?]
@defproc[(fx<   [a fixnum?] [b fixnum?]) boolean?]
@defproc[(fx>   [a fixnum?] [b fixnum?]) boolean?]
@defproc[(fx<=  [a fixnum?] [b fixnum?]) boolean?]
@defproc[(fx>=  [a fixnum?] [b fixnum?]) boolean?]
@defproc[(fxmin [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxmax [a fixnum?] [b fixnum?]) fixnum?]
)]{

Safe versions of @racket[unsafe-fx=], @racket[unsafe-fx<],
 @racket[unsafe-fx>], @racket[unsafe-fx<=], @racket[unsafe-fx>=],
 @racket[unsafe-fxmin], and @racket[unsafe-fxmax].}

@deftogether[(
@defproc[(fx->fl [a fixnum?]) flonum?]
@defproc[(fl->fx [a flonum?]) fixnum?]
)]{

Safe versions of @racket[unsafe-fx->fl] and @racket[unsafe-fl->fx].}

@; ------------------------------------------------------------

@section[#:tag "fxvectors"]{Fixnum Vectors}

A @deftech{fxvector} is like a @tech{vector}, but it holds only
@tech{fixnums}. The only advantage of a @tech{fxvector} over a
@tech{vector} is that a shared version can be created with functions
like @racket[shared-fxvector].

Two @tech{fxvectors} are @racket[equal?] if they have the same length,
and if the values in corresponding slots of the @tech{fxvectors} are
@racket[equal?].

A printed @tech{fxvector} starts with @litchar{#fx(}, optionally with
a number between the @litchar{#fx} and
@litchar{(}. @see-read-print["vector" #:print "vectors"]{fxvectors}

@defproc[(fxvector? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{fxvector}, @racket[#f] otherwise.}

@defproc[(fxvector [x fixnum?] ...) fxvector?]{

Creates a @tech{fxvector} containing the given @tech{fixnums}.

@mz-examples[#:eval flfx-eval (fxvector 2 3 4 5)]}

@defproc[(make-fxvector [size exact-nonnegative-integer?]
                        [x fixnum? 0]) 
         fxvector?]{

Creates a @tech{fxvector} with @racket[size] elements, where every
slot in the @tech{fxvector} is filled with @racket[x].

@mz-examples[#:eval flfx-eval (make-fxvector 4 3)]}

@defproc[(fxvector-length [vec fxvector?]) exact-nonnegative-integer?]{

Returns the length of @racket[vec] (i.e., the number of slots in the
@tech{fxvector}).}


@defproc[(fxvector-ref [vec fxvector?] [pos exact-nonnegative-integer?])
         fixnum?]{

Returns the @tech{fixnum} in slot @racket[pos] of
@racket[vec]. The first slot is position @racket[0], and the last slot
is one less than @racket[(fxvector-length vec)].}

@defproc[(fxvector-set! [vec fxvector?] [pos exact-nonnegative-integer?]
                        [x fixnum?])
         fixnum?]{

Sets the @tech{fixnum} in slot @racket[pos] of @racket[vec]. The
first slot is position @racket[0], and the last slot is one less than
@racket[(fxvector-length vec)].}

@defproc[(fxvector-copy [vec fxvector?]
                        [start exact-nonnegative-integer? 0]
                        [end exact-nonnegative-integer? (vector-length v)]) 
         fxvector?]{

Creates a fresh @tech{fxvector} of size @racket[(- end start)], with all of the
elements of @racket[vec] from @racket[start] (inclusive) to
@racket[end] (exclusive).}

@defproc[(in-fxvector [vec fxvector?]
                    [start exact-nonnegative-integer? 0]
                    [stop (or/c exact-integer? #f) #f]
                    [step (and/c exact-integer? (not/c zero?)) 1])
         sequence?]{
  Returns a sequence equivalent to @racket[vec] when no optional
  arguments are supplied.

  The optional arguments @racket[start], @racket[stop], and
  @racket[step] are as in @racket[in-vector].

  An @racket[in-fxvector] application can provide better
  performance for @tech{fxvector} iteration when it appears directly in a @racket[for] clause.
}

@deftogether[(
@defform[(for/fxvector maybe-length (for-clause ...) body ...)]
@defform/subs[(for*/fxvector maybe-length (for-clause ...) body ...)
              ([maybe-length (code:line)
                             (code:line #:length length-expr)
                             (code:line #:length length-expr #:fill fill-expr)])
              #:contracts ([length-expr exact-nonnegative-integer?]
                           [fill-expr fixnum?])]
)]{

Like @racket[for/vector] or @racket[for*/vector], but for
@tech{fxvector}s. The default @racket[fill-expr] produces @racket[0].}

@defproc[(shared-fxvector [x fixnum?] ...) fxvector?]{

Creates a @tech{fxvector} containing the given @tech{fixnums}.
For communication among @tech{places}, the new @tech{fxvector} is 
allocated in the @tech{shared memory space}.

@mz-examples[#:eval flfx-eval (shared-fxvector 2 3 4 5)]}


@defproc[(make-shared-fxvector [size exact-nonnegative-integer?]
                               [x fixnum? 0]) 
         fxvector?]{

Creates a @tech{fxvector} with @racket[size] elements, where every
slot in the @tech{fxvector} is filled with @racket[x].
For communication among @tech{places}, the new @tech{fxvector} is 
allocated in the @tech{shared memory space}.

@mz-examples[#:eval flfx-eval (make-shared-fxvector 4 3)]}


@close-eval[flfx-eval]
