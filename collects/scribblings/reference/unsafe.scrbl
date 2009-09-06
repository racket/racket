#lang scribble/doc
@(require "mz.ss"
          (for-label scheme/unsafe/ops))

@title[#:tag "unsafe"]{Unsafe Operations}

@defmodule[scheme/unsafe/ops]

All fuctions and forms provided by @schememodname[scheme/base] and
@schememodname[scheme] check their arguments to ensure that the
arguments conform to contracts and other constraints. For example,
@scheme[vector-ref] checks its arguments to ensure that the first
argument is a vector, that the second argument is an exact integer,
and that the second argument is between @scheme[0] and one less than
the vector's length, inclusive.

Functions provided by @schememodname[scheme/unsafe/ops] are
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
)]{

For @tech{fixnums}: Like @scheme[+], @scheme[-], @scheme[*],
@scheme[quotient], and @scheme[remainder], but constrained to consume
@tech{fixnums} and produce a @tech{fixnum} result. The mathematical
operation on @scheme[a] and @scheme[b] must be representable as a
@tech{fixnum}. In the case of @scheme[unsafe-fxquotient] and
@scheme[unsafe-fxremainder], @scheme[b] must not be @scheme[0].}


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
represent a @tech{fixnum}, and the result is effectively
@scheme[bitwise-and]ed with the most negative @tech{fixnum}.}


@deftogether[(
@defproc[(unsafe-fx= [a fixnum?][b fixnum?]) boolean?]
@defproc[(unsafe-fx< [a fixnum?][b fixnum?]) boolean?]
@defproc[(unsafe-fx> [a fixnum?][b fixnum?]) boolean?]
@defproc[(unsafe-fx<= [a fixnum?][b fixnum?]) boolean?]
@defproc[(unsafe-fx>= [a fixnum?][b fixnum?]) boolean?]
)]{

For @tech{fixnums}: Like @scheme[=], @scheme[<], @scheme[>],
@scheme[<=], and @scheme[>=], but constrained to consume
@tech{fixnums}.}


@deftogether[(
@defproc[(unsafe-fl+ [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(unsafe-fl- [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(unsafe-fl* [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(unsafe-fl/ [a inexact-real?][b inexact-real?]) inexact-real?]
)]{

For real @tech{inexact numbers}: Like @scheme[+], @scheme[-],
@scheme[*], and @scheme[/], but constrained to consume real @tech{inexact
numbers}. The result is always a real @tech{inexact number}.}


@deftogether[(
@defproc[(unsafe-fl= [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(unsafe-fl< [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(unsafe-fl> [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(unsafe-fl<= [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(unsafe-fl>= [a inexact-real?][b inexact-real?]) boolean?]
)]{

For real @tech{inexact numbers}: Like @scheme[=], @scheme[<],
@scheme[>], @scheme[<=], and @scheme[>=], but constrained to consume
real @tech{inexact numbers}.}


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
@defproc[(unsafe-vector-length [v vector?]) fixnum?]
@defproc[(unsafe-vector-ref [v vector?][k fixnum?]) any/c]
@defproc[(unsafe-vector-set! [v vector?][k fixnum?][val any/c]) any/c]
)]{

Unsafe versions of @scheme[vector-length], @scheme[vector-ref], and
@scheme[vector-set!]. A vector's size can never be larger than a
@tech{fixnum} (so even @scheme[vector-length] always returns a
fixnum).}



@deftogether[(
@defproc[(unsafe-struct-ref [v any/c][k fixnum?]) any/c]
@defproc[(unsafe-struct-set! [v any/c][k fixnum?][val any/c]) any/c]
)]{

Unsafe field access and update for an instance of a structure
type. The index @scheme[k] must be between @scheme[0] (inclusive) and
the number of fields in the struture (exclusive). In the case of
@scheme[unsafe-struct-set!], the field must be mutable.}

