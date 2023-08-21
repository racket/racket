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
@racket[fx+] that consume and produce only @tech{fixnums}. The operations in
this library are meant to be safe versions of unsafe operations like
@racket[unsafe-fx+]. These safe operations are generally no faster
than using generic primitives like @racket[+].

The expected use of the @racketmodname[racket/fixnum] library is for
code where the @racket[require] of @racketmodname[racket/fixnum] is
replaced with

@racketblock[(require (filtered-in
                       (λ (name)
                         (and (regexp-match #rx"^unsafe-fx" name)
                              (regexp-replace #rx"unsafe-" name "")))
                       racket/unsafe/ops))]

to drop in unsafe versions of the library. Alternately, when
encountering crashes with code that uses unsafe fixnum operations, use
the @racketmodname[racket/fixnum] library to help debug the problems.

@; ------------------------------------------------------------

@section{Fixnum Arithmetic}

@deftogether[(
@defproc[(fx+ [a fixnum?] ...) fixnum?]
@defproc[(fx- [a fixnum?] [b fixnum?] ...) fixnum?]
@defproc[(fx* [a fixnum?] ...) fixnum?]
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
result would not be a fixnum.

@history[#:changed "7.0.0.13" @elem{Allow zero or more arguments for @racket[fx+] and @racket[fx*]
                                    and one or more arguments for @racket[fx-].}]}


@deftogether[(
@defproc[(fxand [a fixnum?] ...) fixnum?]
@defproc[(fxior [a fixnum?] ...) fixnum?]
@defproc[(fxxor [a fixnum?] ...) fixnum?]
@defproc[(fxnot [a fixnum?]) fixnum?]
@defproc[(fxlshift [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxrshift [a fixnum?] [b fixnum?]) fixnum?]
)]{

Like @racket[bitwise-and], @racket[bitwise-ior],
@racket[bitwise-xor], @racket[bitwise-not], and
@racket[arithmetic-shift], but constrained to consume @tech{fixnums};
the result is always a @tech{fixnum}. The @racket[unsafe-fxlshift] and
@racket[unsafe-fxrshift] operations correspond to
@racket[arithmetic-shift], but require non-negative arguments;
@racket[unsafe-fxlshift] is a positive (i.e., left) shift, and
@racket[unsafe-fxrshift] is a negative (i.e., right) shift, where the
number of bits to shift must be no more than the number of bits used to
represent a @tech{fixnum}. The
@exnraise[exn:fail:contract:non-fixnum-result] if the arithmetic
result would not be a fixnum.

@history[#:changed "7.0.0.13" @elem{Allow any number of arguments for @racket[fxand], @racket[fxior],
                                    and @racket[fxxor].}]}


@deftogether[(
@defproc[(fxpopcount [a (and/c fixnum? (not/c negative?))]) fixnum?]
@defproc[(fxpopcount32 [a (and/c fixnum? (integer-in 0 @#,racketvalfont{#xFFFFFFFF}))]) fixnum?]
@defproc[(fxpopcount16 [a (and/c fixnum? (integer-in 0 @#,racketvalfont{#xFFFF})) ]) fixnum?]
)]{

Counts the number of bits in the two's complement representation of
@racket[a]. Depending on the platform, the @racket[fxpopcount32] and
@racket[fxpopcount16] operations can be faster when the result is
known to be no more than 32 or 16, respectively.

@history[#:added "8.5.0.7"]}

@deftogether[(
@defproc[(fx+/wraparound [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fx-/wraparound [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fx*/wraparound [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(fxlshift/wraparound [a fixnum?] [b fixnum?]) fixnum?]
)]{

Like @racket[fx+], @racket[fx-], @racket[fx*], and @racket[fxlshift],
but a fixnum result is produced for any allowed arguments (i.e., for
any fixnum argument, except that the second
@racket[fxlshift/wraparound] argument must be between 0 and the number
of bits in a fixnum, inclusive). The result is produced by simply discarding bits
that do not fit in a fixnum representation. The result is negative if
the highest of the retained bits is set---even, for example, if the
value was produced by adding two positive fixnums.

@history[#:added "7.9.0.6"]}

@defproc[(fxrshift/logical [a fixnum?] [b fixnum?]) fixnum?]{

Shifts the bits in @racket[a] to the right by @racket[b], filling in with zeros.
With the sign bit treated as just another bit, a logical right-shift of a
negative-signed fixnum can produce a large positive fixnum.
For example, @racket[(fxrshift/logical -1 1)] produces @racket[(most-positive-fixnum)],
illustrating that logical right-shift results are platform-dependent.

@mz-examples[
  #:eval flfx-eval
  (fxrshift/logical 128 2)
  (fxrshift/logical 255 4)
  (= (fxrshift/logical -1 1) (most-positive-fixnum))
]

@history[#:added "8.8.0.5"]}


@deftogether[(
@defproc[(fx=   [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(fx<   [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(fx>   [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(fx<=  [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(fx>=  [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(fxmin [a fixnum?] [b fixnum?] ...) fixnum?]
@defproc[(fxmax [a fixnum?] [b fixnum?] ...) fixnum?]
)]{

Like @racket[=], @racket[<], @racket[>],
@racket[<=], @racket[>=], @racket[min], and @racket[max], but
constrained to consume @tech{fixnums}.

@history/arity[]}

@deftogether[(
@defproc[(fx->fl [a fixnum?]) flonum?]
@defproc[(fl->fx [fl flonum?]) fixnum?]
)]{

Conversion between @tech{fixnums} and @tech{flonums} with truncation
in the case of converting a @tech{flonum} to a @tech{fixnum}.

The @racket[fx->fl] function is the same as @racket[exact->inexact] or
@racket[->fl] constrained to a fixnum argument.

The @racket[fl->fx] function is the same as @racket[truncate] followed
by @racket[inexact->exact] or @racket[fl->exact-integer] constrained
to returning a fixnum. If the truncated flonum does not fit into a
fixnum, the @exnraise[exn:fail:contract].

@history[#:changed "7.7.0.8" @elem{Changed @racket[fl->fx] to truncate.}]}


@defproc[(fixnum-for-every-system? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{fixnum} and is
represented by fixnum by every Racket implementation, @racket[#f]
otherwise.

@history[#:added "7.3.0.11"]}


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

@; ------------------------------------------------------------

@section[#:tag "fxrange"]{Fixnum Range}

@deftogether[(
@defproc[(most-positive-fixnum) fixnum?]
@defproc[(most-negative-fixnum) fixnum?]
)]{

Returns the largest-magnitude positive and negative @tech{fixnums}.
The values of @racket[(most-positive-fixnum)] and
@racket[(most-negative-fixnum)] depend on the platform and virtual
machine, but all fixnums are in the range
@racket[(most-negative-fixnum)] to @racket[(most-positive-fixnum)]
inclusive, and all exact integers in that range are fixnums.

@history[#:added "8.1.0.7"]}


@close-eval[flfx-eval]
