#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/unsafe/ops
                     racket/flonum
                     (only-in ffi/vector
                              f64vector?
                              f64vector-ref
                              f64vector-set!
                              u16vector?
                              u16vector-ref
                              u16vector-set!
                              s16vector?
                              s16vector-ref
                              s16vector-set!)))

@title[#:tag "unsafe"]{Unsafe Operations}

@defmodule[racket/unsafe/ops]

All functions and forms provided by @racketmodname[racket/base] and
@racketmodname[racket] check their arguments to ensure that the
arguments conform to contracts and other constraints. For example,
@racket[vector-ref] checks its arguments to ensure that the first
argument is a vector, that the second argument is an exact integer,
and that the second argument is between @racket[0] and one less than
the vector's length, inclusive.

Functions provided by @racketmodname[racket/unsafe/ops] are
@deftech{unsafe}. They have certain constraints, but the constraints
are not checked, which allows the system to generate and execute
faster code. If arguments violate an unsafe function's constraints,
the function's behavior and result is unpredictable, and the entire
system can crash or become corrupted.

All of the exported bindings of @racketmodname[racket/unsafe/ops] are
protected in the sense of @racket[protect-out], so access to unsafe
operations can be prevented by adjusting the code inspector (see
@secref["modprotect"]).

@section{Unsafe Numeric Operations}

@deftogether[(
@defproc[(unsafe-fx+ [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fx- [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fx* [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxquotient  [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxremainder [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxmodulo    [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxabs       [a fixnum?]) fixnum?]
)]{

For @tech{fixnums}: Like @racket[+], @racket[-], @racket[*],
@racket[quotient], @racket[remainder], @racket[modulo], and
@racket[abs], but constrained to consume @tech{fixnums} and produce a
@tech{fixnum} result. The mathematical operation on @racket[a] and
@racket[b] must be representable as a @tech{fixnum}. In the case of
@racket[unsafe-fxquotient], @racket[unsafe-fxremainder], and
@racket[unsafe-fxmodulo], @racket[b] must not be @racket[0].}


@deftogether[(
@defproc[(unsafe-fxand [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxior [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxxor [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxnot [a fixnum?]) fixnum?]
@defproc[(unsafe-fxlshift [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxrshift [a fixnum?] [b fixnum?]) fixnum?]
)]{

For @tech{fixnums}: Like @racket[bitwise-and], @racket[bitwise-ior],
@racket[bitwise-xor], @racket[bitwise-not], and
@racket[arithmetic-shift], but constrained to consume @tech{fixnums};
the result is always a @tech{fixnum}. The @racket[unsafe-fxlshift] and
@racket[unsafe-fxrshift] operations correspond to
@racket[arithmetic-shift], but require non-negative arguments;
@racket[unsafe-fxlshift] is a positive (i.e., left) shift, and
@racket[unsafe-fxrshift] is a negative (i.e., right) shift, where the
number of bits to shift must be less than the number of bits used to
represent a @tech{fixnum}. In the case of @racket[unsafe-fxlshift],
bits in the result beyond the number of bits used to represent a
@tech{fixnum} are effectively replaced with a copy of the high bit.}


@deftogether[(
@defproc[(unsafe-fx=   [a fixnum?] [b fixnum?]) boolean?]
@defproc[(unsafe-fx<   [a fixnum?] [b fixnum?]) boolean?]
@defproc[(unsafe-fx>   [a fixnum?] [b fixnum?]) boolean?]
@defproc[(unsafe-fx<=  [a fixnum?] [b fixnum?]) boolean?]
@defproc[(unsafe-fx>=  [a fixnum?] [b fixnum?]) boolean?]
@defproc[(unsafe-fxmin [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxmax [a fixnum?] [b fixnum?]) fixnum?]
)]{

For @tech{fixnums}: Like @racket[=], @racket[<], @racket[>],
@racket[<=], @racket[>=], @racket[min], and @racket[max], but
constrained to consume @tech{fixnums}.}


@deftogether[(
@defproc[(unsafe-fl+   [a flonum?] [b flonum?]) flonum?]
@defproc[(unsafe-fl-   [a flonum?] [b flonum?]) flonum?]
@defproc[(unsafe-fl*   [a flonum?] [b flonum?]) flonum?]
@defproc[(unsafe-fl/   [a flonum?] [b flonum?]) flonum?]
@defproc[(unsafe-flabs [a flonum?]) flonum?]
)]{

For @tech{flonums}: Unchecked versions of @racket[fl+], @racket[fl-],
@racket[fl*], @racket[fl/], and @racket[flabs].}


@deftogether[(
@defproc[(unsafe-fl=   [a flonum?] [b flonum?]) boolean?]
@defproc[(unsafe-fl<   [a flonum?] [b flonum?]) boolean?]
@defproc[(unsafe-fl>   [a flonum?] [b flonum?]) boolean?]
@defproc[(unsafe-fl<=  [a flonum?] [b flonum?]) boolean?]
@defproc[(unsafe-fl>=  [a flonum?] [b flonum?]) boolean?]
@defproc[(unsafe-flmin [a flonum?] [b flonum?]) flonum?]
@defproc[(unsafe-flmax [a flonum?] [b flonum?]) flonum?]
)]{

For @tech{flonums}: Unchecked versions of @racket[fl=], @racket[fl<],
@racket[fl>], @racket[fl<=], @racket[fl>=], @racket[flmin], and
@racket[flmax].}


@deftogether[(
@defproc[(unsafe-flround [a flonum?]) flonum?]
@defproc[(unsafe-flfloor [a flonum?]) flonum?]
@defproc[(unsafe-flceiling [a flonum?]) flonum?]
@defproc[(unsafe-fltruncate [a flonum?]) flonum?]
)]{

For @tech{flonums}: Unchecked (potentially) versions of
@racket[flround], @racket[flfloor], @racket[flceiling], and
@racket[fltruncate]. Currently, these bindings are simply aliases for
the corresponding safe bindings.}


@deftogether[(
@defproc[(unsafe-flsin [a flonum?]) flonum?]
@defproc[(unsafe-flcos [a flonum?]) flonum?]
@defproc[(unsafe-fltan [a flonum?]) flonum?]
@defproc[(unsafe-flasin [a flonum?]) flonum?]
@defproc[(unsafe-flacos [a flonum?]) flonum?]
@defproc[(unsafe-flatan [a flonum?]) flonum?]
@defproc[(unsafe-fllog [a flonum?]) flonum?]
@defproc[(unsafe-flexp [a flonum?]) flonum?]
@defproc[(unsafe-flsqrt [a flonum?]) flonum?]
@defproc[(unsafe-flexpt [a flonum?] [b flonum?]) flonum?]
)]{

For @tech{flonums}: Unchecked (potentially) versions of
@racket[flsin], @racket[flcos], @racket[fltan], @racket[flasin],
@racket[flacos], @racket[flatan], @racket[fllog], @racket[flexp],
@racket[flsqrt], and @racket[flexpt]. Currently, some of these
bindings are simply aliases for the corresponding safe bindings.}


@deftogether[(
@defproc[(unsafe-make-flrectangular [a flonum?] [b flonum?]) 
         (and/c complex? inexact? (not/c real?))]
@defproc[(unsafe-flreal-part [a (and/c complex? inexact? (not/c real?))]) flonum?]
@defproc[(unsafe-flimag-part [a (and/c complex? inexact? (not/c real?))]) flonum?]
)]{

For @tech{flonums}: Unchecked versions of @racket[make-flrectangular],
@racket[flreal-part], and @racket[flimag-part].}


@deftogether[(
@defproc[(unsafe-fx->fl [a fixnum?]) flonum?]
@defproc[(unsafe-fl->fx [a flonum?]) fixnum?]
)]{
Unchecked conversion of a fixnum to an integer flonum and vice versa.
These are similar to the safe bindings @racket[->fl] and @racket[fl->exact-integer],
but further constrained to consume or produce a fixnum.
}


@section{Unsafe Data Extraction}

@deftogether[(
@defproc[(unsafe-car [p pair?]) any/c]
@defproc[(unsafe-cdr [p pair?]) any/c]
@defproc[(unsafe-mcar [p mpair?]) any/c]
@defproc[(unsafe-mcdr [p mpair?]) any/c]
@defproc[(unsafe-set-mcar! [p mpair?] [v any/c]) void?]
@defproc[(unsafe-set-mcdr! [p mpair?] [v any/c]) void?]
)]{

Unsafe variants of @racket[car], @racket[cdr], @racket[mcar],
@racket[mcdr], @racket[set-mcar!], and @racket[set-mcdr!].}


@defproc[(unsafe-cons-list [v any/c] [rest list?]) (and/c pair? list?)]{

Unsafe variant of @racket[cons] that produces a pair that claims to be
a list---without checking whether @racket[rest] is a list.}


@deftogether[(
@defproc[(unsafe-list-ref [lst pair?] [pos (and/c exact-nonnegative-integer? fixnum?)]) any/c]
@defproc[(unsafe-list-tail [lst any/c] [pos (and/c exact-nonnegative-integer? fixnum?)]) any/c]
)]{

Unsafe variants of @racket[list-ref] and @racket[list-tail], where
@racket[pos] must be a @tech{fixnum}, and @racket[lst] must start with
at least @racket[(add1 pos)] (for @racket[unsafe-list-ref]) or
@racket[pos] (for @racket[unsafe-list-tail]) pairs.}


@deftogether[(
@defproc[(unsafe-unbox [b box?]) fixnum?]
@defproc[(unsafe-set-box! [b box?] [k fixnum?]) void?]
@defproc[(unsafe-unbox* [v (and/c box? (not/c impersonator?))]) any/c]
@defproc[(unsafe-set-box*! [v (and/c box? (not/c impersonator?))] [val any/c]) void?]
)]{

Unsafe versions of @racket[unbox] and @racket[set-box!], where the
@schemeidfont{box*} variants can be faster but do not work on
@tech{impersonators}.}

@defproc[(unsafe-box*-cas! [loc box?] [old any/c] [new any/c]) boolean?]{
  Unsafe version of @racket[box-cas!].  Like @racket[unsafe-set-box*!], it does
  not work on @tech{impersonators}.
}

@deftogether[(
@defproc[(unsafe-vector-length [v vector?]) fixnum?]
@defproc[(unsafe-vector-ref [v vector?] [k fixnum?]) any/c]
@defproc[(unsafe-vector-set! [v vector?] [k fixnum?] [val any/c]) void?]
@defproc[(unsafe-vector*-length [v (and/c vector? (not/c impersonator?))]) fixnum?]
@defproc[(unsafe-vector*-ref [v (and/c vector? (not/c impersonator?))] [k fixnum?]) any/c]
@defproc[(unsafe-vector*-set! [v (and/c vector? (not/c impersonator?))] [k fixnum?] [val any/c]) void?]
)]{

Unsafe versions of @racket[vector-length], @racket[vector-ref], and
@racket[vector-set!], where the @schemeidfont{vector*} variants can be
faster but do not work on @tech{impersonators}.

A vector's size can never be larger than a @tech{fixnum}, so even
@racket[vector-length] always returns a fixnum.}


@deftogether[(
@defproc[(unsafe-string-length [str string?]) fixnum?]
@defproc[(unsafe-string-ref [str string?] [k fixnum?])
         (and/c char? (lambda (ch) (<= 0 (char->integer ch) 255)))]
@defproc[(unsafe-string-set! [str (and/c string? (not/c immutable?))] [k fixnum?] [ch char?]) void?]
)]{

Unsafe versions of @racket[string-length], @racket[string-ref], and
@racket[string-set!]. The @racket[unsafe-string-ref] procedure can be used
only when the result will be a Latin-1 character. A string's size can
never be larger than a @tech{fixnum} (so even @racket[string-length]
always returns a fixnum).}


@deftogether[(
@defproc[(unsafe-bytes-length [bstr bytes?]) fixnum?]
@defproc[(unsafe-bytes-ref [bstr bytes?] [k fixnum?]) byte?]
@defproc[(unsafe-bytes-set! [bstr (and/c bytes? (not/c immutable?))] [k fixnum?] [b byte?]) void?]
)]{

Unsafe versions of @racket[bytes-length], @racket[bytes-ref], and
@racket[bytes-set!]. A bytes's size can never be larger than a
@tech{fixnum} (so even @racket[bytes-length] always returns a
fixnum).}


@deftogether[(
@defproc[(unsafe-flvector-length [v flvector?]) fixnum?]
@defproc[(unsafe-flvector-ref [v flvector?] [k fixnum?]) any/c]
@defproc[(unsafe-flvector-set! [v flvector?] [k fixnum?] [x flonum?]) void?]
)]{

Unsafe versions of @racket[flvector-length], @racket[flvector-ref], and
@racket[flvector-set!]. A @tech{flvector}'s size can never be larger than a
@tech{fixnum} (so even @racket[flvector-length] always returns a
fixnum).}


@deftogether[(
@defproc[(unsafe-f64vector-ref [vec f64vector?] [k fixnum?]) flonum?]
@defproc[(unsafe-f64vector-set! [vec f64vector?] [k fixnum?] [n flonum?]) void?]
)]{

Unsafe versions of @racket[f64vector-ref] and
@racket[f64vector-set!].}


@deftogether[(
@defproc[(unsafe-s16vector-ref [vec s16vector?] [k fixnum?]) (integer-in -32768 32767)]
@defproc[(unsafe-s16vector-set! [vec s16vector?] [k fixnum?] [n (integer-in -32768 32767)]) void?]
)]{

Unsafe versions of @racket[s16vector-ref] and
@racket[s16vector-set!].}


@deftogether[(
@defproc[(unsafe-u16vector-ref [vec u16vector?] [k fixnum?]) (integer-in 0 65535)]
@defproc[(unsafe-u16vector-set! [vec u16vector?] [k fixnum?] [n (integer-in 0 65535)]) void?]
)]{

Unsafe versions of @racket[u16vector-ref] and
@racket[u16vector-set!].}


@deftogether[(
@defproc[(unsafe-struct-ref [v any/c] [k fixnum?]) any/c]
@defproc[(unsafe-struct-set! [v any/c] [k fixnum?] [val any/c]) void?]
@defproc[(unsafe-struct*-ref [v (not/c impersonator?)] [k fixnum?]) any/c]
@defproc[(unsafe-struct*-set! [v (not/c impersonator?)] [k fixnum?] [val any/c]) void?]
)]{

Unsafe field access and update for an instance of a structure
type, where the @schemeidfont{struct*} variants can be
faster but do not work on @tech{impersonators}.
The index @racket[k] must be between @racket[0] (inclusive) and
the number of fields in the structure (exclusive). In the case of
@racket[unsafe-struct-set!], the field must be mutable.}

