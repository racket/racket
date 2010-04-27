#lang scribble/doc
@(require "mz.ss"
          (for-label racket/unsafe/ops
                     racket/flonum
                     (only-in ffi/vector
                              f64vector?
                              f64vector-ref
                              f64vector-set!)))

@title[#:tag "unsafe"]{Unsafe Operations}

@defmodule[racket/unsafe/ops]

All fuctions and forms provided by @schememodname[racket/base] and
@schememodname[scheme] check their arguments to ensure that the
arguments conform to contracts and other constraints. For example,
@scheme[vector-ref] checks its arguments to ensure that the first
argument is a vector, that the second argument is an exact integer,
and that the second argument is between @scheme[0] and one less than
the vector's length, inclusive.

Functions provided by @schememodname[racket/unsafe/ops] are
@deftech{unsafe}. They have certain constraints, but the constraints
are not checked, which allows the system to generate and execute
faster code. If arguments violate an unsafe function's constraints,
the function's behavior and result is unpredictable, and the entire
system can crash or become corrupted.

All of the exported bindings of @schememodname[scheme] are protected
in the sense of @scheme[protect-out], so access to unsafe operations
can be prevented by adjusting the code inspector (see
@secref["modprotect"]).

@section{Unsafe Numeric Operations}

@deftogether[(
@defproc[(unsafe-fx+ [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fx- [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fx* [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fxquotient [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fxremainder [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fxmodulo [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fxabs [a fixnum?]) fixnum?]
)]{

For @tech{fixnums}: Like @scheme[+], @scheme[-], @scheme[*],
@scheme[quotient], @scheme[remainder], @scheme[modulo], and
@scheme[abs], but constrained to consume @tech{fixnums} and produce a
@tech{fixnum} result. The mathematical operation on @scheme[a] and
@scheme[b] must be representable as a @tech{fixnum}. In the case of
@scheme[unsafe-fxquotient], @scheme[unsafe-fxremainder], and
@scheme[unsafe-fxmodulo], @scheme[b] must not be @scheme[0].}


@deftogether[(
@defproc[(unsafe-fxand [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fxior [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fxxor [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fxnot [a fixnum?]) fixnum?]
@defproc[(unsafe-fxlshift [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fxrshift [a fixnum?][b fixnum?]) fixnum?]
)]{

For @tech{fixnums}: Like @scheme[bitwise-and], @scheme[bitwise-ior],
@scheme[bitwise-xor], @scheme[bitwise-not], and
@scheme[arithmetic-shift], but constrained to consume @tech{fixnums};
the result is always a @tech{fixnum}. The @scheme[unsafe-fxlshift] and
@scheme[unsafe-fxrshift] operations correspond to
@scheme[arithmetic-shift], but require non-negative arguments;
@scheme[unsafe-fxlshift] is a positive (i.e., left) shift, and
@scheme[unsafe-fxrshift] is a negative (i.e., right) shift, where the
number of bits to shift must be less than the number of bits used to
represent a @tech{fixnum}. In the case of @scheme[unsafe-fxlshift],
bits in the result beyond the the number of bits used to represent a
@tech{fixnum} are effectively replaced with a copy of the high bit.}


@deftogether[(
@defproc[(unsafe-fx= [a fixnum?][b fixnum?]) boolean?]
@defproc[(unsafe-fx< [a fixnum?][b fixnum?]) boolean?]
@defproc[(unsafe-fx> [a fixnum?][b fixnum?]) boolean?]
@defproc[(unsafe-fx<= [a fixnum?][b fixnum?]) boolean?]
@defproc[(unsafe-fx>= [a fixnum?][b fixnum?]) boolean?]
@defproc[(unsafe-fxmin [a fixnum?][b fixnum?]) fixnum?]
@defproc[(unsafe-fxmax [a fixnum?][b fixnum?]) fixnum?]
)]{

For @tech{fixnums}: Like @scheme[=], @scheme[<], @scheme[>],
@scheme[<=], @scheme[>=], @scheme[min], and @scheme[max], but
constrained to consume @tech{fixnums}.}


@defproc[(unsafe-fx->fl [a fixnum?]) inexact-real?]{
Unchecked version of @scheme[->fl].
}


@deftogether[(
@defproc[(unsafe-fl+ [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(unsafe-fl- [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(unsafe-fl* [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(unsafe-fl/ [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(unsafe-flabs [a inexact-real?]) inexact-real?]
)]{

For @tech{flonums}: Unchecked versions of @scheme[fl+], @scheme[fl-],
@scheme[fl*], @scheme[fl/], and @scheme[flabs].}


@deftogether[(
@defproc[(unsafe-fl= [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(unsafe-fl< [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(unsafe-fl> [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(unsafe-fl<= [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(unsafe-fl>= [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(unsafe-flmin [a inexact-real?]) inexact-real?]
@defproc[(unsafe-flmax [a inexact-real?]) inexact-real?]
)]{

For @tech{flonums}: Unchecked versions of @scheme[fl=], @scheme[fl<],
@scheme[fl>], @scheme[fl<=], @scheme[fl>=], @scheme[flmin], and
@scheme[flmax].}


@deftogether[(
@defproc[(unsafe-flround [a inexact-real?]) inexact-real?]
@defproc[(unsafe-flfloor [a inexact-real?]) inexact-real?]
@defproc[(unsafe-flceiling [a inexact-real?]) inexact-real?]
@defproc[(unsafe-fltruncate [a inexact-real?]) inexact-real?]
)]{

For @tech{flonums}: Unchecked (potentially) versions of
@scheme[flround], @scheme[flfloor], @scheme[flceiling], and
@scheme[fltruncate]. Currently, these bindings are simply aliases for
the corresponding safe bindings.}


@deftogether[(
@defproc[(unsafe-flsin [a inexact-real?]) inexact-real?]
@defproc[(unsafe-flcos [a inexact-real?]) inexact-real?]
@defproc[(unsafe-fltan [a inexact-real?]) inexact-real?]
@defproc[(unsafe-flasin [a inexact-real?]) inexact-real?]
@defproc[(unsafe-flacos [a inexact-real?]) inexact-real?]
@defproc[(unsafe-flatan [a inexact-real?]) inexact-real?]
@defproc[(unsafe-fllog [a inexact-real?]) inexact-real?]
@defproc[(unsafe-flexp [a inexact-real?]) inexact-real?]
@defproc[(unsafe-flsqrt [a inexact-real?]) inexact-real?]
)]{

For @tech{flonums}: Unchecked (potentially) versions of
@scheme[flsin], @scheme[flcos], @scheme[fltan], @scheme[flasin],
@scheme[flacos], @scheme[flatan], @scheme[fllog], @scheme[flexp], and
@scheme[flsqrt]. Currently, some of these bindings are simply aliases
for the corresponding safe bindings.}


@section{Unsafe Data Extraction}

@deftogether[(
@defproc[(unsafe-car [p pair?]) any/c]
@defproc[(unsafe-cdr [p pair?]) any/c]
@defproc[(unsafe-mcar [p mpair?]) any/c]
@defproc[(unsafe-mcdr [p mpair?]) any/c]
@defproc[(unsafe-set-mcar! [p mpair?] [v any/c]) void?]
@defproc[(unsafe-set-mcdr! [p mpair?] [v any/c]) void?]
)]{

Unsafe variants of @scheme[car], @scheme[cdr], @scheme[mcar],
@scheme[mcdr], @scheme[set-mcar!], and @scheme[set-mcdr!].}


@deftogether[(
@defproc[(unsafe-unbox [v (and/c box? (not/c chaperone?))]) any/c]
@defproc[(unsafe-set-box! [v (and/c box? (not/c chaperone?))][val any/c]) void?]
@defproc[(unsafe-unbox* [b box?]) fixnum?]
@defproc[(unsafe-set-box*! [b box?][k fixnum?]) void?]
)]{

Unsafe versions of @scheme[unbox] and @scheme[set-box!].}


@deftogether[(
@defproc[(unsafe-vector-length [v (and/c vector? (not/c chaperone?))]) fixnum?]
@defproc[(unsafe-vector-ref [v (and/c vector? (not/c chaperone?))][k fixnum?]) any/c]
@defproc[(unsafe-vector-set! [v (and/c vector? (not/c chaperone?))][k fixnum?][val any/c]) void?]
@defproc[(unsafe-vector*-length [v vector?]) fixnum?]
@defproc[(unsafe-vector*-ref [v vector?][k fixnum?]) any/c]
@defproc[(unsafe-vector*-set! [v vector?][k fixnum?][val any/c]) void?]
)]{

Unsafe versions of @scheme[vector-length], @scheme[vector-ref], and
@scheme[vector-set!]. A vector's size can never be larger than a
@tech{fixnum} (so even @scheme[vector-length] always returns a
fixnum).}


@deftogether[(
@defproc[(unsafe-string-length [str string?]) fixnum?]
@defproc[(unsafe-string-ref [str string?][k fixnum?])
         (and/c char? (lambda (ch) (<= 0 (char->integer ch) 255)))]
@defproc[(unsafe-string-set! [str (and/c string? (not/c immutable?))][k fixnum?][ch char?]) void?]
)]{

Unsafe versions of @scheme[string-length], @scheme[string-ref], and
@scheme[string-set!]. The @scheme[unsafe-string-ref] procedure can be used
only when the result will be a Latin-1 character. A string's size can
never be larger than a @tech{fixnum} (so even @scheme[string-length]
always returns a fixnum).}


@deftogether[(
@defproc[(unsafe-bytes-length [bstr bytes?]) fixnum?]
@defproc[(unsafe-bytes-ref [bstr bytes?][k fixnum?]) byte?]
@defproc[(unsafe-bytes-set! [bstr (and/c bytes? (not/c immutable?))][k fixnum?][b byte?]) void?]
)]{

Unsafe versions of @scheme[bytes-length], @scheme[bytes-ref], and
@scheme[bytes-set!]. A bytes's size can never be larger than a
@tech{fixnum} (so even @scheme[bytes-length] always returns a
fixnum).}


@deftogether[(
@defproc[(unsafe-flvector-length [v flvector?]) fixnum?]
@defproc[(unsafe-flvector-ref [v flvector?][k fixnum?]) any/c]
@defproc[(unsafe-flvector-set! [v flvector?][k fixnum?][x inexact-real?]) void?]
)]{

Unsafe versions of @scheme[flvector-length], @scheme[flvector-ref], and
@scheme[flvector-set!]. A @tech{flvector}'s size can never be larger than a
@tech{fixnum} (so even @scheme[flvector-length] always returns a
fixnum).}


@deftogether[(
@defproc[(unsafe-f64vector-ref [vec f64vector?][k fixnum?]) inexact-real?]
@defproc[(unsafe-f64vector-set! [vec f64vector?][k fixnum?][n inexact-real?]) void?]
)]{

Unsafe versions of @scheme[f64vector-ref] and
@scheme[f64vector-set!].}


@deftogether[(
@defproc[(unsafe-struct-ref [v (not/c chaperone?)][k fixnum?]) any/c]
@defproc[(unsafe-struct-set! [v (not/c chaperone?)][k fixnum?][val any/c]) void?]
@defproc[(unsafe-struct*-ref [v any/c][k fixnum?]) any/c]
@defproc[(unsafe-struct*-set! [v any/c][k fixnum?][val any/c]) void?]
)]{

Unsafe field access and update for an instance of a structure
type. The index @scheme[k] must be between @scheme[0] (inclusive) and
the number of fields in the struture (exclusive). In the case of
@scheme[unsafe-struct-set!], the field must be mutable.}

