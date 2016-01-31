#lang scribble/doc
@(require "mz.rkt" racket/math scribble/extract
          (for-label racket/math
                     racket/flonum
                     racket/fixnum
                     racket/unsafe/ops
                     racket/require
                     racket/random))

@(define math-eval (make-base-eval))
@examples[#:hidden #:eval math-eval (require racket/math)]

@title[#:tag "numbers" #:style '(toc)]{Numbers}

@guideintro["numbers"]{numbers}

All @deftech{numbers} are @deftech{complex numbers}. Some of them are
@deftech{real numbers}, and all of the real numbers that can be
represented are also @deftech{rational numbers}, except for
@as-index{@racket[+inf.0]} (positive @as-index{infinity}), @as-index{@racket[+inf.f]} (single-precision variant),
@as-index{@racket[-inf.0]} (negative infinity), @as-index{@racket[-inf.f]}  (single-precision variant),
@as-index{@racket[+nan.0]} (@as-index{not-a-number}), and @as-index{@racket[+nan.f]}  (single-precision variant). Among the
rational numbers, some are @deftech{integers}, because @racket[round]
applied to the number produces the same number.

@margin-note/ref{See @secref["parse-number"] for information on the
syntax of number literals.}

Orthogonal to those categories, each number is also either an
@deftech{exact number} or an @deftech{inexact number}. Unless
otherwise specified, computations that involve an inexact number
produce inexact results. Certain operations on inexact numbers,
however, produce an exact number, such as multiplying an inexact
number with an exact @racket[0]. Operations that mathematically produce
irrational numbers for some rational arguments (e.g., @racket[sqrt]) may
produce inexact results even for exact arguments.

In the case of complex numbers, either the real and imaginary parts
are both exact or inexact with the same precision, or the number has
an exact zero real part and an inexact imaginary part; a complex
number with an exact zero imaginary part is a real number.

Inexact real numbers are implemented as either single- or
double-precision @as-index{IEEE floating-point numbers}---the latter
by default, and the former only when a computation starts with
numerical constants specified as single-precision numbers. Inexact
real numbers that are represented as double-precision floating-point
numbers are @deftech{flonums}.

Inexact numbers can be coerced to exact form, except for the inexact
numbers @racket[+inf.0], @racket[+inf.f],
@racket[-inf.0], @racket[-inf.f], @racket[+nan.0], and @racket[+nan.f], which
have no exact form. @index["division by inexact zero"]{Dividing} a
number by exact zero raises an exception; dividing a non-zero number
other than @racket[+nan.0] or @racket[+nan.f] by an inexact zero returns @racket[+inf.0],
@racket[+inf.f], @racket[-inf.0]
or @racket[-inf.f], depending on the sign and precision of the dividend. The
@racket[+nan.0] value is not @racket[=] to itself, but @racket[+nan.0]
is @racket[eqv?] to itself, and @racket[+nan.f] is similarly @racket[eqv?] but 
not @racket[=] to itself. Conversely, @racket[(= 0.0 -0.0)] is
@racket[#t], but @racket[(eqv? 0.0 -0.0)] is @racket[#f], and the 
same for @racket[0.0f0] and @racket[-0.0f0] (which are single-precision variants). The datum
@racketvalfont{-nan.0} refers to the same constant as @racket[+nan.0],
and @racketvalfont{-nan.f} is the same as @racket[+nan.f].

Calculations with infinites produce results consistent with IEEE
double- or single-precision floating point where IEEE specifies the result; in
cases where IEEE provides no specification,
the result corresponds to the limit approaching
infinity, or @racket[+nan.0] or @racket[+nan.f] if no such limit exists.

The precision and size of exact numbers is limited only by available
memory (and the precision of operations that can produce irrational
numbers). In particular, adding, multiplying, subtracting, and
dividing exact numbers always produces an exact result.

A @deftech{fixnum} is an exact integer whose two's complement
representation fit into 31 bits on a 32-bit platform or 63 bits on a
64-bit platform; furthermore, no allocation is required when computing
with fixnums. See also the @racketmodname[racket/fixnum] module, below.

Two fixnums that are @racket[=] are also the same
according to @racket[eq?]. Otherwise, the result of @racket[eq?]
applied to two numbers is undefined, except that numbers produced
by the default reader in @racket[read-syntax] mode are @tech{interned} and therefore @racket[eq?]
when they are @racket[eqv?].

Two real numbers are @racket[eqv?] when they are both inexact with the same precision or both
exact, and when they are @racket[=] (except for @racket[+nan.0], @racket[+nan.f],
@racket[+0.0], @racket[+0.0f0], @racket[-0.0], and @racket[-0.0f0], as noted above). 
Two complex numbers are @racket[eqv?] when their real and imaginary parts are @racket[eqv?].
Two numbers are @racket[equal?] when they are @racket[eqv?].

@see-read-print["number"]{numbers}

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "number-types"]{Number Types}

@defproc[(number? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v]
 is a number, @racket[#f] otherwise.

@mz-examples[(number? 1) (number? 2+3i) (number? "hello")]}


@defproc[(complex? [v any/c]) boolean?]{ Returns @racket[(number? v)],
because all numbers are @tech{complex numbers}.}


@defproc[(real? [v any/c]) boolean?]{ Returns @racket[#t] if @racket[v] is
 a @techlink{real number}, @racket[#f] otherwise.

@mz-examples[(real? 1) (real? +inf.0) (real? 2+3i) 
             (real? 2+0.0i) (real? "hello")]}


@defproc[(rational? [v any/c]) boolean?]{ Returns @racket[#t] if
 @racket[v] is a @techlink{rational number}, @racket[#f] otherwise.

@mz-examples[(rational? 1) (rational? +inf.0) (rational? "hello")]}


@defproc[(integer? [v any/c]) boolean?]{ Returns @racket[#t] if @racket[v]
 is a number that is an @techlink{integer}, @racket[#f] otherwise.

@mz-examples[(integer? 1) (integer? 2.3) (integer? 4.0) (integer? +inf.0) 
             (integer? 2+3i) (integer? "hello")]}


@defproc[(exact-integer? [v any/c]) boolean?]{

Returns @racket[(and (integer? v) (exact? v))].

@mz-examples[(exact-integer? 1) (exact-integer? 4.0)]}


@defproc[(exact-nonnegative-integer? [v any/c]) boolean?]{

Returns @racket[(and (exact-integer? v) (not (negative? v)))].

@mz-examples[(exact-nonnegative-integer? 0) (exact-nonnegative-integer? -1)]}


@defproc[(exact-positive-integer? [v any/c]) boolean?]{

Returns @racket[(and (exact-integer? v) (positive? v))].

@mz-examples[(exact-positive-integer? 1) (exact-positive-integer? 0)]}


@defproc[(inexact-real? [v any/c]) boolean?]{

Returns @racket[(and (real? v) (inexact? v))].}


@defproc[(fixnum? [v any/c]) boolean?]{

Return @racket[#t] if @racket[v] is a @techlink{fixnum}, @racket[#f]
otherwise.

Note: the result of this function is platform-dependent, so using it in
syntax transformers can lead to platform-dependent bytecode files.}


@defproc[(flonum? [v any/c]) boolean?]{

Return @racket[#t] if @racket[v] is a @techlink{flonum}, @racket[#f]
otherwise.}

@defproc[(double-flonum? [v any/c]) boolean?]{
Identical to @racket[flonum?]}.

@defproc[(single-flonum? [v any/c]) boolean?]{
Return @racket[#t] if @racket[v] is a single-precision floating-point
number, @racket[#f] otherwise.}

@defproc[(zero? [z number?]) boolean?]{ Returns @racket[(= 0 z)].

@mz-examples[(zero? 0) (zero? -0.0)]}


@defproc[(positive? [x real?]) boolean?]{ Returns @racket[(> x 0)].

@mz-examples[(positive? 10) (positive? -10) (positive? 0.0)]}


@defproc[(negative? [x real?]) boolean?]{ Returns @racket[(< x 0)].

@mz-examples[(negative? 10) (negative? -10) (negative? -0.0)]}


@defproc[(even? [n integer?]) boolean?]{ Returns @racket[(zero? (modulo
 n 2))].

@mz-examples[(even? 10.0) (even? 11) (eval:error (even? +inf.0))]}


@defproc[(odd? [n integer?]) boolean?]{ Returns @racket[(not (even? n))].

@mz-examples[(odd? 10.0) (odd? 11) (eval:error (odd? +inf.0))]}


@defproc[(exact? [z number?]) boolean?]{ Returns @racket[#t] if @racket[z]
 is an exact number, @racket[#f] otherwise.

@mz-examples[(exact? 1) (exact? 1.0)]}


@defproc[(inexact? [z number?]) boolean?]{ Returns @racket[#t] if @racket[z]
 is an inexact number, @racket[#f] otherwise.

@mz-examples[(inexact? 1) (inexact? 1.0)]}


@defproc[(inexact->exact [z number?]) exact?]{ Coerces @racket[z] to an
 exact number. If @racket[z] is already exact, it is returned. If @racket[z]
 is @racket[+inf.0], @racket[-inf.0], @racket[+nan.0],
 @racket[+inf.f], @racket[-inf.f], or @racket[+nan.f], then the
 @exnraise[exn:fail:contract].

@mz-examples[(inexact->exact 1) (inexact->exact 1.0)]}


@defproc[(exact->inexact [z number?]) inexact?]{ Coerces @racket[z] to an
 inexact number. If @racket[z] is already inexact, it is returned.

@mz-examples[(exact->inexact 1) (exact->inexact 1.0)]}

@defproc[(real->single-flonum [x real?]) single-flonum?]{ Coerces @racket[x]
 to a single-precision floating-point number. If @racket[x] is already
 a single-precision floating-point number, it is returned.}

@defproc[(real->double-flonum [x real?]) flonum?]{ Coerces @racket[x]
 to a double-precision floating-point number. If @racket[x] is already
 a double-precision floating-point number, it is returned.}

@; ----------------------------------------
@section[#:tag "generic-numbers"]{Generic Numerics}

Most Racket numeric operations work on any kind of number. 

@; ----------------------------------------
@subsection{Arithmetic}

@defproc[(+ [z number?] ...) number?]{

Returns the sum of the @racket[z]s, adding pairwise from left to
 right. If no arguments are provided, the result is @racket[0].

@mz-examples[(+ 1 2) (+ 1.0 2+3i 5) (+)]}


@defproc*[([(- [z number?]) number?]
           [(- [z number?] [w number?] ...+) number?])]{

When no @racket[w]s are supplied, returns @racket[(- 0 z)].
 Otherwise, returns the subtraction of the @racket[w]s from @racket[z]
 working pairwise from left to right.}

@mz-examples[(- 5 3.0) (- 1) (- 2+7i 1 3)]


@defproc[(* [z number?] ...) number?]{

Returns the product of the @racket[z]s, multiplying pairwise from left
 to right. If no arguments are provided, the result is
 @racket[1]. Multiplying any number by exact @racket[0] produces exact
 @racket[0].

@mz-examples[(* 2 3) (* 8.0 9) (* 1+2i 3+4i)]}


@defproc*[([(/ [z number?]) number?]
           [(/ [z number?] [w number?] ...+) number?])]{

When no @racket[w]s are supplied, returns @racket[(/ 1 z)].
 Otherwise, returns the division of @racket[z] by the @racket[w]s working
 pairwise from left to right.

If @racket[z] is exact @racket[0] and no @racket[w] is exact
 @racket[0], then the result is exact @racket[0]. If any @racket[w] is
 exact @racket[0], the @exnraise[exn:fail:contract:divide-by-zero].

@mz-examples[(/ 3 4) (/ 81 3 3) (/ 10.0) (/ 1+2i 3+4i)]}


@defproc[(quotient [n integer?] [m integer?]) integer?]{

Returns @racket[(truncate (/ n m))].

@mz-examples[(quotient 10 3) (quotient -10.0 3) (eval:error (quotient +inf.0 3))]}


@defproc[(remainder [n integer?] [m integer?]) integer?]{

Returns @racket[_q] with the same sign as @racket[n] such that

@itemize[

 @item{@racket[(abs _q)] is between @racket[0] (inclusive) and @racket[(abs m)] (exclusive), and}

 @item{@racket[(+ _q (* m (quotient n m)))] equals @racket[n].}

]

If @racket[m] is exact @racket[0], the
 @exnraise[exn:fail:contract:divide-by-zero].

@mz-examples[(remainder 10 3) (remainder -10.0 3) (remainder 10.0 -3) (remainder -10 -3) (eval:error (remainder +inf.0 3))]}


@defproc[(quotient/remainder [n integer?] [m integer?]) (values integer? integer?)]{

Returns @racket[(values (quotient n m) (remainder n m))], but the
 combination may be computed more efficiently than separate calls to
 @racket[quotient] and @racket[remainder].

@mz-examples[
(quotient/remainder 10 3)
]}


@defproc[(modulo [n integer?] [m integer?]) integer?]{ 

Returns @racket[_q] with the same sign as @racket[m] where

@itemize[

 @item{@racket[(abs _q)] is between @racket[0] (inclusive) and @racket[(abs m)] (exclusive), and}

 @item{the difference between @racket[_q] and @racket[(- n (* m (quotient n m)))] is a multiple of @racket[m].}

]

If @racket[m] is exact @racket[0], the
 @exnraise[exn:fail:contract:divide-by-zero].

@mz-examples[(modulo 10 3) (modulo -10.0 3)  (modulo 10.0 -3) (modulo -10 -3) (eval:error (modulo +inf.0 3))]}


@defproc[(add1 [z number?]) number?]{ Returns @racket[(+ z 1)].}

@defproc[(sub1 [z number?]) number?]{ Returns @racket[(- z 1)].}

@defproc[(abs [x real?]) number?]{ Returns the absolute value of
 @racket[x].

@mz-examples[(abs 1.0) (abs -1)]}

@defproc[(max [x real?] ...+) real?]{

Returns the largest of the @racket[x]s, or @racket[+nan.0] if any
 @racket[x] is @racket[+nan.0].  If any @racket[x] is inexact, the
 result is coerced to inexact.

@mz-examples[(max 1 3 2) (max 1 3 2.0)]}


@defproc[(min [x real?] ...+) real?]{

Returns the smallest of the @racket[x]s, or @racket[+nan.0] if any
 @racket[x] is @racket[+nan.0].  If any @racket[x] is inexact, the
 result is coerced to inexact.

@mz-examples[(min 1 3 2) (min 1 3 2.0)]}


@defproc[(gcd [n rational?] ...) rational?]{

Returns the @as-index{greatest common divisor} (a non-negative
 number) of the @racket[n]s; for non-integer @racket[n]s, the result
 is the @racket[gcd] of the numerators divided
 by the @racket[lcm] of the denominators. 
 If no arguments are provided, the result
 is @racket[0]. If all arguments are zero, the result is zero.

@mz-examples[(gcd 10) (gcd 12 81.0) (gcd 1/2 1/3)]}


@defproc[(lcm [n rational?] ...) rational?]{

Returns the @as-index{least common multiple} (a non-negative number)
 of the @racket[n]s; non-integer @racket[n]s, the result is
 the absolute value of the product divided by the
 @racket[gcd]. If no arguments are provided, the result is
 @racket[1]. If any argument is zero, the result is zero; furthermore,
 if any argument is exact @racket[0], the result is exact @racket[0].

@mz-examples[(lcm 10) (lcm 3 4.0) (lcm 1/2 2/3)]}


@defproc[(round [x real?]) (or/c integer? +inf.0 -inf.0 +nan.0)]{

Returns the integer closest to @racket[x], resolving ties in favor of
 an even number, but @racket[+inf.0], @racket[-inf.0], and @racket[+nan.0]
 round to themselves.

@mz-examples[(round 17/4) (round -17/4) (round 2.5) (round -2.5) (round +inf.0)]}


@defproc[(floor [x real?]) (or/c integer? +inf.0 -inf.0 +nan.0)]{

Returns the largest integer that is no more than @racket[x], but
 @racket[+inf.0], @racket[-inf.0], and @racket[+nan.0] floor to
 themselves.

@mz-examples[(floor 17/4) (floor -17/4) (floor 2.5) (floor -2.5) (floor +inf.0)]}


@defproc[(ceiling [x real?]) (or/c integer? +inf.0 -inf.0 +nan.0)]{

Returns the smallest integer that is at least as large as @racket[x],
 but @racket[+inf.0], @racket[-inf.0], and @racket[+nan.0] ceiling to
 themselves.

@mz-examples[(ceiling 17/4) (ceiling -17/4) (ceiling 2.5) (ceiling -2.5) (ceiling +inf.0)]}


@defproc[(truncate [x real?]) (or/c integer? +inf.0 -inf.0 +nan.0)]{

Returns the integer farthest from @racket[0] that is not farther from
 @racket[0] than @racket[x], but @racket[+inf.0], @racket[-inf.0], and
 @racket[+nan.0] truncate to themselves.

@mz-examples[(truncate 17/4) (truncate -17/4) (truncate 2.5) (truncate -2.5) (truncate +inf.0)]}


@defproc[(numerator [q rational?]) integer?]{

Coerces @racket[q] to an exact number, finds the numerator of the
 number expressed in its simplest fractional form, and returns this
 number coerced to the exactness of @racket[q].

@mz-examples[(numerator 5) (numerator 34/8) (numerator 2.3)]}


@defproc[(denominator [q rational?]) integer?]{

Coerces @racket[q] to an exact number, finds the denominator of the
 number expressed in its simplest fractional form, and returns this
 number coerced to the exactness of @racket[q].

@mz-examples[(denominator 5) (denominator 34/8) (denominator 2.3)]}


@defproc[(rationalize [x real?] [tolerance real?]) real?]{

Among the real numbers within @racket[(abs tolerance)] of @racket[x],
 returns the one corresponding to an exact number whose
 @racket[denominator] is the smallest.  If multiple integers are within
 @racket[tolerance] of @racket[x], the one closest to @racket[0] is
 used.

@mz-examples[
(rationalize 1/4 1/10)
(rationalize -1/4 1/10)
(rationalize 1/4 1/4)
(rationalize 11/40 1/4)
]}

@; ----------------------------------------
@subsection{Number Comparison}

@defproc[(= [z number?] [w number?] ...+) boolean?]{ Returns
 @racket[#t] if all of the arguments are numerically equal,
 @racket[#f] otherwise.  An inexact number is numerically equal to an
 exact number when the exact coercion of the inexact number is the
 exact number. Also, @racket[0.0] and @racket[-0.0] are numerically
 equal, but @racket[+nan.0] is not numerically equal to itself.

@mz-examples[(= 1 1.0) (= 1 2) (= 2+3i 2+3i 2+3i)]}


@defproc[(< [x real?] [y real?] ...+) boolean?]{ Returns @racket[#t] if
 the arguments in the given order are strictly increasing,
 @racket[#f] otherwise.

@mz-examples[(< 1 1) (< 1 2 3) (< 1 +inf.0) (< 1 +nan.0)]}


@defproc[(<= [x real?] [y real?] ...+) boolean?]{ Returns @racket[#t]
 if the arguments in the given order are non-decreasing,
 @racket[#f] otherwise.

@mz-examples[(<= 1 1) (<= 1 2 1)]}


@defproc[(> [x real?] [y real?] ...+) boolean?]{ Returns @racket[#t] if
 the arguments in the given order are strictly decreasing,
 @racket[#f] otherwise.

@mz-examples[(> 1 1) (> 3 2 1) (> +inf.0 1) (> +nan.0 1)]}


@defproc[(>= [x real?] [y real?] ...+) boolean?]{ Returns @racket[#t]
 if the arguments in the given order are non-increasing,
 @racket[#f] otherwise.

@mz-examples[(>= 1 1) (>= 1 2 1)]}


@; ------------------------------------------------------------------------
@subsection{Powers and Roots}

@defproc[(sqrt [z number?]) number?]{

Returns the principal @as-index{square root} of @racket[z].  The
 result is exact if @racket[z] is exact and @racket[z]'s square root
 is rational. See also @racket[integer-sqrt].

@mz-examples[(sqrt 4/9) (sqrt 2) (sqrt -1)]}


@defproc[(integer-sqrt [n integer?]) complex?]{

Returns @racket[(floor (sqrt n))] for positive @racket[n].  The
 result is exact if @racket[n] is exact.  For
 negative @racket[n], the result is @racket[(* (integer-sqrt (- n))
 0+i)].

@mz-examples[(integer-sqrt 4.0) (integer-sqrt 5) (integer-sqrt -4.0) (integer-sqrt -4)]}


@defproc[(integer-sqrt/remainder [n integer?])
         (values complex? integer?)]{

Returns @racket[(integer-sqrt n)] and @racket[(- n (expt
 (integer-sqrt n) 2))].

@mz-examples[(integer-sqrt/remainder 4.0) (integer-sqrt/remainder 5)]}


@defproc[(expt [z number?] [w number?]) number?]{

Returns @racket[z] raised to the power of @racket[w].

If @racket[w] is
 exact @racket[0], the result is exact @racket[1].
 If @racket[w] is @racket[0.0] or @racket[-0.0]  and @racket[z] is a @tech{real number}, the
 result is @racket[1.0] (even if @racket[z] is @racket[+nan.0]).

If @racket[z] is exact @racket[1], the result is exact @racket[1].
 If @racket[z] is @racket[1.0] and @racket[w] is a @tech{real number}, the
 result is @racket[1.0] (even if @racket[w] is @racket[+nan.0]).

If @racket[z] is
 exact @racket[0] and @racket[w] is negative, the
 @exnraise[exn:fail:contract:divide-by-zero].

Further special cases when @racket[w] is a @tech{real number}:
@margin-note*{These special cases correspond to @tt{pow} in C99 @cite["C99"],
except when @racket[z] is negative and @racket[w] is a not an
integer.}
@;
@itemlist[#:style 'compact

 @item{@racket[(expt 0.0 w)]:
       @itemlist[#:style 'compact
         @item{@racket[w] is negative --- @racket[+inf.0]}
         @item{@racket[w] is positive --- @racket[0.0]}]}

 @item{@racket[(expt -0.0 w)]:
       @itemlist[#:style 'compact
         @item{@racket[w] is negative:
               @itemlist[#:style 'compact
                @item{@racket[w] is an odd integer --- @racket[-inf.0]}
                @item{@racket[w] otherwise rational --- @racket[+inf.0]}]}
         @item{@racket[w] is positive:
               @itemlist[#:style 'compact
                @item{@racket[w] is an odd integer --- @racket[-0.0]}
                @item{@racket[w] otherwise rational --- @racket[+0.0]}]}]}

 @item{@racket[(expt z -inf.0)] for positive @racket[z]:
       @itemlist[#:style 'compact
         @item{@racket[z] is less than @racket[1.0] --- @racket[+inf.0]}
         @item{@racket[z] is greater than @racket[1.0] --- @racket[+0.0]}]}

 @item{@racket[(expt z +inf.0)] for positive @racket[z]:
       @itemlist[#:style 'compact
         @item{@racket[z] is less than @racket[1.0] --- @racket[+0.0]}
         @item{@racket[z] is greater than @racket[1.0] --- @racket[+inf.0]}]}

 @item{@racket[(expt -inf.0 w)] for integer @racket[w]:
       @itemlist[#:style 'compact
         @item{@racket[w] is negative:
               @itemlist[#:style 'compact
                @item{@racket[w] is odd --- @racket[-0.0]}
                @item{@racket[w] is even --- @racket[+0.0]}]}
         @item{@racket[w] is positive:
               @itemlist[#:style 'compact
                @item{@racket[w] is odd --- @racket[-inf.0]}
                @item{@racket[w] is even --- @racket[+inf.0]}]}]}

 @item{@racket[(expt +inf.0 w)]:
       @itemlist[#:style 'compact
         @item{@racket[w] is negative --- @racket[+0.0]}
         @item{@racket[w] is positive --- @racket[+inf.0]}]}
]

@mz-examples[(expt 2 3) (expt 4 0.5) (expt +inf.0 0)]}


@defproc[(exp [z number?]) number?]{

Returns Euler's number raised to the power of @racket[z]. The result
 is normally inexact, but it is exact @racket[1] when @racket[z] is an
 exact @racket[0].

@mz-examples[(exp 1) (exp 2+3i) (exp 0)]}


@defproc[(log [z number?]) number?]{

Returns the natural logarithm of @racket[z].  The result is normally
 inexact, but it is exact @racket[0] when @racket[z] is an exact
 @racket[1]. When @racket[z] is exact @racket[0],
 @exnraise[exn:fail:contract:divide-by-zero].

@mz-examples[(log (exp 1)) (log 2+3i) (log 1)]}


@; ------------------------------------------------------------------------
@subsection{Trigonometric Functions}

@defproc[(sin [z number?]) number?]{

Returns the sine of @racket[z], where @racket[z] is in radians. The
 result is normally inexact, but it is exact @racket[0] if @racket[z]
 is exact @racket[0].

@mz-examples[(sin 3.14159) (sin 1+05.i)]}


@defproc[(cos [z number?]) number?]{

Returns the cosine of @racket[z], where @racket[z] is in radians.

@mz-examples[(cos 3.14159) (cos 1+05.i)]}


@defproc[(tan [z number?]) number?]{

Returns the tangent of @racket[z], where @racket[z] is in radians. The
 result is normally inexact, but it is exact @racket[0] if @racket[z]
 is exact @racket[0].

@mz-examples[(tan 0.7854) (tan 1+05.i)]}


@defproc[(asin [z number?]) number?]{

Returns the arcsine in radians of @racket[z]. The result is normally
 inexact, but it is exact @racket[0] if @racket[z] is exact @racket[0].

@mz-examples[(asin 0.25) (asin 1+05.i)]}


@defproc[(acos [z number?]) number?]{

Returns the arccosine in radians of @racket[z].

@mz-examples[(acos 0.25) (acos 1+05.i)]}


@defproc*[([(atan [z number?]) number?]
           [(atan [y real?] [x real?]) number?])]{

In the one-argument case, returns the arctangent of the inexact
 approximation of @racket[z], except that the result is an exact
 @racket[0] for an exact @racket[0] argument.

In the two-argument case, the result is roughly the same as @racket[
 (atan (/ (exact->inexact y)) (exact->inexact x))], but the signs of @racket[y]
 and @racket[x] determine the quadrant of the result. Moreover, a
 suitable angle is returned when @racket[y] divided by @racket[x]
 produces @racket[+nan.0] in the case that neither @racket[y] nor
 @racket[x] is @racket[+nan.0]. Finally, if @racket[y] is exact
 @racket[0] and @racket[x] is an exact positive number, the result is
 exact @racket[0]. If both @racket[x] and @racket[y] are exact
 @racket[0], the @exnraise[exn:fail:contract:divide-by-zero].

@mz-examples[(atan 0.5) (atan 2 1) (atan -2 -1) (atan 1+05.i) (atan +inf.0 -inf.0)]}

@; ------------------------------------------------------------------------
@subsection{Complex Numbers}

@defproc[(make-rectangular [x real?] [y real?]) number?]{

Returns @racket[(+ x (* y 0+1i))].

@mz-examples[(make-rectangular 3 4.0)]}


@defproc[(make-polar [magnitude real?] [angle real?]) number?]{

Returns @racket[(+ (* magnitude (cos angle)) (* magnitude (sin angle)
 0+1i))].

@mz-examples[#:eval math-eval
                    (make-polar 10 (* pi 1/2))
                    (make-polar 10 (* pi 1/4))]}


@defproc[(real-part [z number?]) real?]{

Returns the real part of the complex number @racket[z] in rectangle
 coordinates.

@mz-examples[(real-part 3+4i) (real-part 5.0)]}


@defproc[(imag-part [z number?]) real?]{

Returns the imaginary part of the complex number @racket[z] in
 rectangle coordinates.

@mz-examples[(imag-part 3+4i) (imag-part 5.0) (imag-part 5.0+0.0i)]}


@defproc[(magnitude [z number?]) (and/c real? (not/c negative?))]{

 Returns the magnitude of the complex number @racket[z] in polar
 coordinates.

@mz-examples[(magnitude -3) (magnitude 3.0) (magnitude 3+4i)]}


@defproc[(angle [z number?]) real?]{ Returns the angle of
 the complex number @racket[z] in polar coordinates.
 
 The result is guaranteed to be between @racket[(- pi)] and
 @racket[pi], possibly equal to @racket[pi] (but never equal
 to @racket[(- pi)]).

@mz-examples[(angle -3) (angle 3.0) (angle 3+4i) (angle +inf.0+inf.0i) (angle -1)]}

@; ------------------------------------------------------------------------
@subsection{Bitwise Operations}

@section-index{logical operators}

@defproc[(bitwise-ior [n exact-integer?] ...) exact-integer?]{ Returns
 the bitwise ``inclusive or'' of the @racket[n]s in their (semi-infinite)
 two's complement representation. If no arguments are provided, the
 result is @racket[0].

@mz-examples[(bitwise-ior 1 2) (bitwise-ior -32 1)]}


@defproc[(bitwise-and [n exact-integer?] ...) exact-integer?]{ Returns
 the bitwise ``and'' of the @racket[n]s in their (semi-infinite) two's
 complement representation. If no arguments are provided, the result
 is @racket[-1].

@mz-examples[(bitwise-and 1 2) (bitwise-and -32 -1)]}


@defproc[(bitwise-xor [n exact-integer?] ...) exact-integer?]{ Returns
 the bitwise ``exclusive or'' of the @racket[n]s in their (semi-infinite)
 two's complement representation. If no arguments are provided, the
 result is @racket[0].

@mz-examples[(bitwise-xor 1 5) (bitwise-xor -32 -1)]}


@defproc[(bitwise-not [n exact-integer?])  exact-integer?]{ Returns the
 bitwise ``not'' of @racket[n] in its (semi-infinite) two's complement
 representation.

@mz-examples[(bitwise-not 5) (bitwise-not -1)]}


@defproc[(bitwise-bit-set? [n exact-integer?] [m exact-nonnegative-integer?])
         boolean?]{

Returns @racket[#t] when the @racket[m]th bit of @racket[n] is set in @racket[n]'s
        (semi-infinite) two's complement representation.
                   
This operation is equivalent to
@racket[(not (zero? (bitwise-and n (arithmetic-shift 1 m))))],
but it is faster and runs in constant time when @racket[n] is positive.

@mz-examples[(bitwise-bit-set? 5 0) (bitwise-bit-set? 5 2) (bitwise-bit-set? -5 (expt 2 700))]}


@defproc[(bitwise-bit-field [n exact-integer?] 
                            [start exact-nonnegative-integer?] 
                            [end (and/c exact-nonnegative-integer?
                                        (start . <= . end))])
         exact-integer?]{

Extracts the bits between position @racket[start] and @racket[(- end 1)] (inclusive)
from @racket[n] and shifts them down to the least significant portion of the number.

This operation is equivalent to the computation

@racketblock[
(bitwise-and (sub1 (arithmetic-shift 1 (- end start)))
             (arithmetic-shift n (- start)))
]

but it runs in constant time when @racket[n] is positive, @racket[start] and
@racket[end] are fixnums, and @racket[(- end start)] is no more than
the maximum width of a fixnum.

Each pair of examples below uses the same numbers, showing the result
both in binary and as integers.

@mz-examples[(format "~b" (bitwise-bit-field (string->number "1101" 2) 1 1))
             (bitwise-bit-field 13 1 1)
             (format "~b" (bitwise-bit-field (string->number "1101" 2) 1 3))
             (bitwise-bit-field 13 1 3)
             (format "~b" (bitwise-bit-field (string->number "1101" 2) 1 4))
             (bitwise-bit-field 13 1 4)]
}


@defproc[(arithmetic-shift [n exact-integer?] [m exact-integer?])
 exact-integer?]{ Returns the bitwise ``shift'' of @racket[n] in its
 (semi-infinite) two's complement representation.  If @racket[m] is
 non-negative, the integer @racket[n] is shifted left by @racket[m] bits;
 i.e., @racket[m] new zeros are introduced as rightmost digits. If
 @racket[m] is negative, @racket[n] is shifted right by @racket[(- m)]
 bits; i.e., the rightmost @racket[m] digits are dropped.

@mz-examples[(arithmetic-shift 1 10) (arithmetic-shift 255 -3)]}

@defproc[(integer-length [n exact-integer?]) exact-integer?]{ Returns
 the number of bits in the (semi-infinite) two's complement
 representation of @racket[n] after removing all leading zeros (for
 non-negative @racket[n]) or ones (for negative @racket[n]).

@mz-examples[(integer-length 8) (integer-length -8)]}

@; ------------------------------------------------------------------------
@subsection{Random Numbers}

@margin-note{When security is a concern, use @racket[crypto-random-bytes] instead of @racket[random].}

@defproc*[([(random [k (integer-in 1 4294967087)]
                    [rand-gen pseudo-random-generator?
                               (current-pseudo-random-generator)])
            exact-nonnegative-integer?]
           [(random [min (integer-in 1 4294967087)]
                    [max (integer-in 1 4294967087)]
                    [rand-gen pseudo-random-generator?
                              (current-pseudo-random-generator)])
            exact-nonnegative-integer?]
           [(random [rand-gen pseudo-random-generator?
                              (current-pseudo-random-generator)]) 
            (and/c real? inexact? (>/c 0) (</c 1))])]{  

When called with an integer argument @racket[k], returns a random
exact integer in the range @racket[0] to @math{@racket[k]-1}.

When called with two integer arguments @racket[min] and @racket[max], returns a
random exact integer in the range @racket[min] to @math{@racket[max]-1}.

When called with zero arguments, returns a random inexact number between
@racket[0] and @racket[1], exclusive.

In each case, the number is provided by the given pseudo-random number
generator (which defaults to the current one, as produced by
@racket[current-pseudo-random-generator]). The generator maintains an
internal state for generating numbers. The random number generator
uses a 54-bit version of L'Ecuyer's MRG32k3a algorithm
@cite["L'Ecuyer02"].

@history[#:changed "6.4"]{Added support for ranges.}}


@defproc[(random-seed [k (integer-in 1 (sub1 (expt 2 31)))])
          void?]{

Seeds the current pseudo-random number generator with
@racket[k]. Seeding a generator sets its internal state
deterministically; that is, seeding a generator with a particular
number forces it to produce a sequence of pseudo-random numbers that
is the same across runs and across platforms.

The @racket[random-seed] function is convenient for some purposes, but
note that the space of states for a pseudo-random number generator is
much larger that the space of allowed values for @racket[k]. Use
@racket[vector->pseudo-random-generator!] to set a pseudo-random
number generator to any of its possible states.}


@defproc[(make-pseudo-random-generator) pseudo-random-generator?]{

Returns a new pseudo-random number generator. The new generator is
seeded with a number derived from @racket[(current-milliseconds)].}


@defproc[(pseudo-random-generator? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a pseudo-random number generator,
@racket[#f] otherwise.}


@defparam[current-pseudo-random-generator rand-gen pseudo-random-generator?]{

A @tech{parameter} that determines the pseudo-random number generator
used by @racket[random].}


@defproc[(pseudo-random-generator->vector [rand-gen pseudo-random-generator?])
         pseudo-random-generator-vector?]{

Produces a vector that represents the complete internal state of
@racket[rand-gen]. The vector is suitable as an argument to
@racket[vector->pseudo-random-generator] to recreate the generator in
its current state (across runs and across platforms).}


@defproc[(vector->pseudo-random-generator [vec pseudo-random-generator-vector?])
         pseudo-random-generator?]{

Produces a pseudo-random number generator whose internal state
corresponds to @racket[vec].}

@defproc[(vector->pseudo-random-generator! [rand-gen pseudo-random-generator?]
                                           [vec pseudo-random-generator-vector?])
         void?]{

Like @racket[vector->pseudo-random-generator], but changes
@racket[rand-gen] to the given state, instead of creating a new
generator.}


@defproc[(pseudo-random-generator-vector? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a vector of six exact integers,
where the first three integers are in the range @racket[0] to
@racket[4294967086], inclusive; the last three integers are in the
range @racket[0] to @racket[4294944442], inclusive; at least one of
the first three integers is non-zero; and at least one of the last
three integers is non-zero. Otherwise, the result is @racket[#f].}

@; ------------------------------------------------------------------------

@subsection{Other Randomness Utilities}

@defmodule[racket/random]{}

@defproc[(crypto-random-bytes [n exact-positive-integer?])
         bytes?]{

Provides an interface to randomness from the underlying operating system. Use
@racket[crypto-random-bytes] instead of @racket[random] wherever security is a
concern.

Returns @racket[n] random bytes. On Unix systems, the bytes are
obtained from @filepath{/dev/urandom}, while Windows uses
the @tt{RtlGenRand} system function.

@examples[
 (eval:alts (crypto-random-bytes 14) #"\0\1\1\2\3\5\b\r\25\"7Y\220\351")
]

@history[#:added "6.3"]}

@defproc[(random-ref [seq sequence?]
                     [rand-gen pseudo-random-generator?
                      (current-pseudo-random-generator)])
         any/c]{

Returns a random element of the sequence. Like @racket[sequence-length], does
not terminate on infinite sequences, and evaluates the entire sequence.

@history[#:added "6.4"]}

@defproc[(random-sample [seq sequence?]
                        [n exact-positive-integer?]
                        [rand-gen pseudo-random-generator?
                         (current-pseudo-random-generator)]
                        [#:replacement? replacement? any/c #t])
         (listof any/c)]{

Returns a list of @racket[n] elements of @racket[seq], picked at random, listed
in any order.
If @racket[replacement?] is non-false, elements are drawn with replacement,
which allows for duplicates.

Like @racket[sequence-length], does not terminate on infinite sequences, and
evaluates the entire sequence.

@history[#:added "6.4"]}


@; ------------------------------------------------------------------------
@subsection{Number--String Conversions}

@section-index["numbers" "machine representations"]
@section-index["numbers" "floating-point"]
@section-index["numbers" "big-endian"]
@section-index["numbers" "little-endian"]
@section-index["numbers" "converting"]

@defproc[(number->string [z number?] [radix (or/c 2 8 10 16) 10])
         string?]{
 Returns a string that is the printed form of @racket[z]
 in the base specified by @racket[radix]. If @racket[z] is inexact,
 @racket[radix] must be @racket[10], otherwise the
 @exnraise[exn:fail:contract].

@mz-examples[(number->string 3.0) (number->string 255 8)]}


@defproc[(string->number [s string?] [radix (integer-in 2 16) 10])
         (or/c number? #f)]{

Reads and returns a number datum from @racket[s] (see
@secref["parse-number"]), returning @racket[#f] if @racket[s] does not
parse exactly as a number datum (with no whitespace). The optional
@racket[radix] argument specifies the default base for the number,
which can be overridden by @litchar{#b}, @litchar{#o}, @litchar{#d}, or
@litchar{#x} in the string. The @racket[read-decimal-as-inexact]
parameter affects @racket[string->number] in the same as way as @racket[read].

@mz-examples[(string->number "3.0+2.5i") (string->number "hello")
          (string->number "111" 7)  (string->number "#b111" 7)]
}

@defproc[(real->decimal-string [n real?] [decimal-digits exact-nonnegative-integer? 2])
         string?]{

Prints @racket[n] into a string and returns the string. The printed
form of @racket[n] shows exactly @racket[decimal-digits] digits after
the decimal point. The printed form uses a minus sign if @racket[n] is
negative, and it does not use a plus sign if @racket[n] is positive.

Before printing, @racket[n] is converted to an exact number,
multiplied by @racket[(expt 10 decimal-digits)], rounded, and then
divided again by @racket[(expt 10 decimal-digits)].  The result of this
process is an exact number whose decimal representation has no more
than @racket[decimal-digits] digits after the decimal (and it is
padded with trailing zeros if necessary).

@mz-examples[
#:eval math-eval
(real->decimal-string pi)
(real->decimal-string pi 5)
]}

@defproc[(integer-bytes->integer [bstr bytes?]
                                 [signed? any/c]
                                 [big-endian? any/c (system-big-endian?)]
                                 [start exact-nonnegative-integer? 0]
                                 [end exact-nonnegative-integer? (bytes-length bstr)])
         exact-integer?]{

Converts the machine-format number encoded in @racket[bstr] to an
exact integer. The @racket[start] and @racket[end] arguments specify
the substring to decode, where @racket[(- end start)] must be
@racket[2], @racket[4], or @racket[8]. If @racket[signed?] is true,
then the bytes are decoded as a two's-complement number, otherwise it
is decoded as an unsigned integer. If @racket[big-endian?] is true,
then the first character's ASCII value provides the most significant
eight bits of the number, otherwise the first character provides the
least-significant eight bits, and so on.}


@defproc[(integer->integer-bytes [n exact-integer?]
                                 [size-n (or/c 2 4 8)]
                                 [signed? any/c]
                                 [big-endian? any/c (system-big-endian?)]
                                 [dest-bstr (and/c bytes? (not/c immutable?))
                                            (make-bytes size-n)]
                                 [start exact-nonnegative-integer? 0])
          bytes?]{

Converts the exact integer @racket[n] to a machine-format number
encoded in a byte string of length @racket[size-n], which must be
@racket[2], @racket[4], or @racket[8]. If @racket[signed?] is true,
then the number is encoded as two's complement, otherwise it is
encoded as an unsigned bit stream. If @racket[big-endian?] is true,
then the most significant eight bits of the number are encoded in the
first character of the resulting byte string, otherwise the
least-significant bits are encoded in the first byte, and so on.

The @racket[dest-bstr] argument must be a mutable byte string of
length @racket[size-n]. The encoding of @racket[n] is written into
@racket[dest-bstr] starting at offset @racket[start], and
@racket[dest-bstr] is returned as the result.

If @racket[n] cannot be encoded in a string of the requested size and
format, the @exnraise[exn:fail:contract]. If @racket[dest-bstr] is not
of length @racket[size-n], the @exnraise[exn:fail:contract].}


@defproc[(floating-point-bytes->real [bstr bytes?]
                                     [big-endian? any/c (system-big-endian?)]
                                     [start exact-nonnegative-integer? 0]
                                     [end exact-nonnegative-integer? (bytes-length bstr)])
         flonum?]{

Converts the IEEE floating-point number encoded in @racket[bstr] from
position @racket[start] (inclusive) to @racket[end] (exclusive) to an
inexact real number. The difference between @racket[start] an
@racket[end] must be either 4 or 8 bytes. If @racket[big-endian?] is
true, then the first byte's ASCII value provides the most significant
eight bits of the IEEE representation, otherwise the first byte
provides the least-significant eight bits, and so on.}


@defproc[(real->floating-point-bytes [x real?]
                                     [size-n (or/c 4 8)]
                                     [big-endian? any/c (system-big-endian?)]
                                     [dest-bstr (and/c bytes? (not/c immutable?))
                                                (make-bytes size-n)]
                                     [start exact-nonnegative-integer? 0])
          bytes?]{

Converts the real number @racket[x] to its IEEE representation in a
byte string of length @racket[size-n], which must be @racket[4] or
@racket[8]. If @racket[big-endian?] is true, then the most significant
eight bits of the number are encoded in the first byte of the
resulting byte string, otherwise the least-significant bits are
encoded in the first character, and so on.

The @racket[dest-bstr] argument must be a mutable byte string of
length @racket[size-n]. The encoding of @racket[n] is written into
@racket[dest-bstr] starting with byte @racket[start], and
@racket[dest-bstr] is returned as the result.

If @racket[dest-bstr] is provided and it has less than @racket[start]
plus @racket[size-n] bytes, the @exnraise[exn:fail:contract].}


@defproc[(system-big-endian?) boolean?]{

Returns @racket[#t] if the native encoding of numbers is big-endian
for the machine running Racket, @racket[#f] if the native encoding
is little-endian.}

@; ------------------------------------------------------------------------
@subsection{Extra Constants and Functions}

@note-lib[racket/math]

@defthing[pi flonum?]{

An approximation of π, the ratio of a circle's circumference to its
diameter.
@examples[
#:eval math-eval
pi
(cos pi)
]}

@defthing[pi.f single-flonum?]{

Like @racket[pi], but in single precision.
@examples[
#:eval math-eval
pi.f
(* 2.0f0 pi)
(* 2.0f0 pi.f)
]}

@defproc[(degrees->radians [x real?]) real?]{

Converts an @racket[x]-degree angle to radians.
@mz-examples[
#:eval math-eval
(degrees->radians 180)
(sin (degrees->radians 45))
]}

@defproc[(radians->degrees [x real?]) real?]{

Converts @racket[x] radians to degrees.
@mz-examples[
#:eval math-eval
(radians->degrees pi)
(radians->degrees (* 1/4 pi))
]}

@defproc[(sqr [z number?]) number?]{

Returns @racket[(* z z)].}

@defproc[(sgn [x real?]) (or/c (=/c -1) (=/c 0) (=/c 1) +nan.0 +nan.f)]{

Returns the sign of @racket[x] as either @math{-1}, @math{0},
@math{1}, or not-a-number.

@mz-examples[
#:eval math-eval
(sgn 10)
(sgn -10.0)
(sgn 0)
(sgn +nan.0)
]}

@defproc[(conjugate [z number?]) number?]{

Returns the complex conjugate of @racket[z].

@mz-examples[
#:eval math-eval
(conjugate 1)
(conjugate 3+4i)
]}

@defproc[(sinh [z number?]) number?]{

Returns the hyperbolic sine of @racket[z].}

@defproc[(cosh [z number?]) number?]{

Returns the hyperbolic cosine of @racket[z].}

@defproc[(tanh [z number?]) number?]{

Returns the hyperbolic tangent of @racket[z].}

@defproc[(exact-round [x rational?]) exact-integer?]{

Equivalent to @racket[(inexact->exact (round x))].
}

@defproc[(exact-floor [x rational?]) exact-integer?]{

Equivalent to @racket[(inexact->exact (floor x))].
}

@defproc[(exact-ceiling [x rational?]) exact-integer?]{

Equivalent to @racket[(inexact->exact (ceiling x))].
}

@defproc[(exact-truncate [x rational?]) exact-integer?]{

Equivalent to @racket[(inexact->exact (truncate x))].
}

@defproc[(order-of-magnitude [r (and/c real? positive?)]) (and/c exact? integer?)]{
Computes the greatest exact integer @racket[m] such that:
@racketblock[(<= (expt 10 m)
                 (inexact->exact r))]
Hence also:
@racketblock[(< (inexact->exact r)
                (expt 10 (add1 m)))]

@mz-examples[#:eval math-eval 
                    (order-of-magnitude 999)
                    (order-of-magnitude 1000)
                    (order-of-magnitude 1/100)
                    (order-of-magnitude 1/101)]
}

@defproc[(nan? [x real?]) boolean?]{

Returns @racket[#t] if @racket[x] is @racket[eqv?] to @racket[+nan.0] or @racket[+nan.f]; otherwise @racket[#f].}

@defproc[(infinite? [x real?]) boolean?]{

Returns @racket[#t] if @racket[z] is @racket[+inf.0], @racket[-inf.0], @racket[+inf.f], @racket[-inf.f]; otherwise @racket[#f].}

@; ----------------------------------------------------------------------
@close-eval[math-eval]
@; ----------------------------------------------------------------------

@include-section["flonums.scrbl"]
@include-section["fixnums.scrbl"]
@include-section["extflonums.scrbl"]

@; ----------------------------------------------------------------------

