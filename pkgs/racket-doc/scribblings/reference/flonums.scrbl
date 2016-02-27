#lang scribble/doc
@(require "mz.rkt" (for-label racket/flonum))

@(define fl-eval (make-base-eval))
@examples[#:hidden #:eval fl-eval (require racket/flonum)]

@title[#:tag "flonums"]{Flonums}

@defmodule[racket/flonum]

The @racketmodname[racket/flonum] library provides operations like
@racket[fl+] that consume and produce only
@tech{flonums}. Flonum-specific operations can provide better
performance when used consistently, and they are as safe as generic
operations like @racket[+].

@guidealso["fixnums+flonums"]

@; ------------------------------------------------------------------------

@section{Flonum Arithmetic}

@deftogether[(
@defproc[(fl+ [a flonum?] [b flonum?]) flonum?]
@defproc[(fl- [a flonum?] [b flonum?]) flonum?]
@defproc[(fl* [a flonum?] [b flonum?]) flonum?]
@defproc[(fl/ [a flonum?] [b flonum?]) flonum?]
@defproc[(flabs [a flonum?]) flonum?]
)]{

Like @racket[+], @racket[-], @racket[*], @racket[/], and @racket[abs],
but constrained to consume @tech{flonums}. The result is always a
@tech{flonum}.}

@deftogether[(
@defproc[(fl=   [a flonum?] [b flonum?]) boolean?]
@defproc[(fl<   [a flonum?] [b flonum?]) boolean?]
@defproc[(fl>   [a flonum?] [b flonum?]) boolean?]
@defproc[(fl<=  [a flonum?] [b flonum?]) boolean?]
@defproc[(fl>=  [a flonum?] [b flonum?]) boolean?]
@defproc[(flmin [a flonum?] [b flonum?]) flonum?]
@defproc[(flmax [a flonum?] [b flonum?]) flonum?]
)]{

Like @racket[=], @racket[<], @racket[>], @racket[<=], @racket[>=],
@racket[min], and @racket[max], but constrained to consume
@tech{flonums}.}

@deftogether[(
@defproc[(flround    [a flonum?]) flonum?]
@defproc[(flfloor    [a flonum?]) flonum?]
@defproc[(flceiling  [a flonum?]) flonum?]
@defproc[(fltruncate [a flonum?]) flonum?]
)]{

Like @racket[round], @racket[floor], @racket[ceiling], and
@racket[truncate], but constrained to consume @tech{flonums}.}

@deftogether[(
@defproc[(flsin  [a flonum?]) flonum?]
@defproc[(flcos  [a flonum?]) flonum?]
@defproc[(fltan  [a flonum?]) flonum?]
@defproc[(flasin [a flonum?]) flonum?]
@defproc[(flacos [a flonum?]) flonum?]
@defproc[(flatan [a flonum?]) flonum?]
@defproc[(fllog  [a flonum?]) flonum?]
@defproc[(flexp  [a flonum?]) flonum?]
@defproc[(flsqrt [a flonum?]) flonum?]
)]{

Like @racket[sin], @racket[cos], @racket[tan], @racket[asin],
@racket[acos], @racket[atan], @racket[log], @racket[exp], and
@racket[sqrt], but constrained to consume and produce
@tech{flonums}. The result is @racket[+nan.0] when a number outside
the range @racket[-1.0] to @racket[1.0] is given to @racket[flasin] or
@racket[flacos], or when a negative number is given to @racket[fllog]
or @racket[flsqrt].}

@defproc[(flexpt  [a flonum?] [b flonum?])
         flonum?]{

Like @racket[expt], but constrained to consume and produce
@tech{flonums}.

Due to the result constraint, the results compared to @racket[expt]
differ in the following cases:
@margin-note*{These special cases correspond to @tt{pow} in C99 @cite["C99"].}
@;
@itemlist[#:style 'compact

 @item{@racket[(flexpt -1.0 +inf.0)] --- @racket[1.0]}

 @item{@racket[(flexpt a +inf.0)] where @racket[a] is
  negative --- @racket[(expt (abs a) +inf.0)]}

 @item{@racket[(flexpt a -inf.0)] where @racket[a] is
  negative --- @racket[(expt (abs a) -inf.0)]}

 @item{@racket[(expt -inf.0 b)] where @racket[b] is a non-integer:
       @itemlist[#:style 'compact
         @item{@racket[b] is negative --- @racket[+0.0]}
         @item{@racket[b] is positive --- @racket[+inf.0]}]}

 @item{@racket[(flexpt a b)] where @racket[a] is
  negative and @racket[b] is not an integer --- @racket[+nan.0]}

]}


@defproc[(->fl [a exact-integer?]) flonum?]{

Like @racket[exact->inexact], but constrained to consume exact
integers, so the result is always a @tech{flonum}.}


@defproc[(fl->exact-integer [a flonum?]) exact-integer?]{

Like @racket[inexact->exact], but constrained to consume an
@tech{integer} @tech{flonum}, so the result is always an exact
integer.}


@deftogether[(
@defproc[(make-flrectangular [a flonum?] [b flonum?])
         (and/c complex?
                (lambda (c) (flonum? (real-part c)))
                (lambda (c) (flonum? (imag-part c))))]
@defproc[(flreal-part [a (and/c complex?
                                (lambda (c) (flonum? (real-part c)))
                                (lambda (c) (flonum? (imag-part c))))])
         flonum?]
@defproc[(flimag-part [a (and/c complex?
                                (lambda (c) (flonum? (real-part c)))
                                (lambda (c) (flonum? (imag-part c))))])
         flonum?]
)]{

Like @racket[make-rectangular], @racket[real-part], and
@racket[imag-part], but both parts of the complex number must be
inexact.}

@defproc[(flrandom [rand-gen pseudo-random-generator?]) (and flonum? (>/c 0) (</c 1))]{

Equivalent to @racket[(random rand-gen)].}

@; ------------------------------------------------------------------------

@section[#:tag "flvectors"]{Flonum Vectors}

A @deftech{flvector} is like a @tech{vector}, but it holds only
inexact real numbers. This representation can be more compact, and
unsafe operations on @tech{flvector}s (see
@racketmodname[racket/unsafe/ops]) can execute more efficiently than
unsafe operations on @tech{vectors} of inexact reals.

An f64vector as provided by @racketmodname[ffi/vector] stores the
same kinds of values as a @tech{flvector}, but with extra
indirections that make f64vectors more convenient for working with
foreign libraries. The lack of indirections makes unsafe
@tech{flvector} access more efficient.

Two @tech{flvectors} are @racket[equal?] if they have the same length,
and if the values in corresponding slots of the @tech{flvectors} are
@racket[equal?].

A printed @tech{flvector} starts with @litchar{#fl(}, optionally with
a number between the @litchar{#fl} and
@litchar{(}. @see-read-print["vector" #:print "vectors"]{flvectors}

@defproc[(flvector? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{flvector}, @racket[#f] otherwise.}

@defproc[(flvector [x flonum?] ...) flvector?]{

Creates a @tech{flvector} containing the given inexact real numbers.

@mz-examples[#:eval fl-eval (flvector 2.0 3.0 4.0 5.0)]}

@defproc[(make-flvector [size exact-nonnegative-integer?]
                        [x flonum? 0.0]) 
         flvector?]{

Creates a @tech{flvector} with @racket[size] elements, where every
slot in the @tech{flvector} is filled with @racket[x].

@mz-examples[#:eval fl-eval (make-flvector 4 3.0)]}

@defproc[(flvector-length [vec flvector?]) exact-nonnegative-integer?]{

Returns the length of @racket[vec] (i.e., the number of slots in the
@tech{flvector}).}


@defproc[(flvector-ref [vec flvector?] [pos exact-nonnegative-integer?])
         flonum?]{

Returns the inexact real number in slot @racket[pos] of
@racket[vec]. The first slot is position @racket[0], and the last slot
is one less than @racket[(flvector-length vec)].}

@defproc[(flvector-set! [vec flvector?] [pos exact-nonnegative-integer?]
                        [x flonum?])
         flonum?]{

Sets the inexact real number in slot @racket[pos] of @racket[vec]. The
first slot is position @racket[0], and the last slot is one less than
@racket[(flvector-length vec)].}

@defproc[(flvector-copy [vec flvector?]
                        [start exact-nonnegative-integer? 0]
                        [end exact-nonnegative-integer? (vector-length v)]) 
         flvector?]{

Creates a fresh @tech{flvector} of size @racket[(- end start)], with all of the
elements of @racket[vec] from @racket[start] (inclusive) to
@racket[end] (exclusive).}


@defproc[(in-flvector [vec flvector?]
                      [start exact-nonnegative-integer? 0]
                      [stop (or/c exact-integer? #f) #f]
                      [step (and/c exact-integer? (not/c zero?)) 1])
         sequence?]{
  Returns a sequence equivalent to @racket[vec] when no optional
  arguments are supplied.

  The optional arguments @racket[start], @racket[stop], and
  @racket[step] are as in @racket[in-vector].

  A @racket[in-flvector] application can provide better
  performance for @tech{flvector} iteration when it appears directly in a @racket[for] clause.
}

@deftogether[(
@defform[(for/flvector maybe-length (for-clause ...) body ...)]
@defform/subs[(for*/flvector maybe-length (for-clause ...) body ...)
              ([maybe-length (code:line)
                             (code:line #:length length-expr)
                             (code:line #:length length-expr #:fill fill-expr)])
              #:contracts ([length-expr exact-nonnegative-integer?]
                           [fill-expr flonum?])]
)]{

Like @racket[for/vector] or @racket[for*/vector], but for
@tech{flvector}s. The default @racket[fill-expr] produces @racket[0.0].}

@defproc[(shared-flvector [x flonum?] ...) flvector?]{

Creates a @tech{flvector} containing the given inexact real numbers.
For communication among @tech{places}, the new @tech{flvector} is 
allocated in the @tech{shared memory space}.

@mz-examples[#:eval fl-eval (shared-flvector 2.0 3.0 4.0 5.0)]}


@defproc[(make-shared-flvector [size exact-nonnegative-integer?]
                        [x flonum? 0.0]) 
         flvector?]{

Creates a @tech{flvector} with @racket[size] elements, where every
slot in the @tech{flvector} is filled with @racket[x].
For communication among @tech{places}, the new @tech{flvector} is 
allocated in the @tech{shared memory space}.

@mz-examples[#:eval fl-eval (make-shared-flvector 4 3.0)]}

@; ------------------------------------------------------------

@close-eval[fl-eval]
