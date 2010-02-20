#lang scribble/doc
@(require "mz.ss"
          scheme/math
          scribble/extract
          (for-label scheme/math
                     scheme/flonum
                     scheme/fixnum
                     scheme/unsafe/ops
                     scheme/require))

@(define math-eval (make-base-eval))
@(interaction-eval #:eval math-eval (require scheme/math))

@title[#:tag "numbers"]{Numbers}

@guideintro["numbers"]{numbers}

All @deftech{numbers} are @deftech{complex numbers}. Some of them are
@deftech{real numbers}, and all of the real numbers that can be
represented are also @deftech{rational numbers}, except for
@as-index{@scheme[+inf.0]} (positive @as-index{infinity}),
@as-index{@scheme[-inf.0]} (negative infinity), and
@as-index{@scheme[+nan.0]} (@as-index{not-a-number}). Among the
rational numbers, some are @deftech{integers}, because @scheme[round]
applied to the number produces the same number.

Orthogonal to those categories, each number is also either an
@deftech{exact number} or an @deftech{inexact number}. Unless
otherwise specified, computations that involve an inexact number
produce inexact results. Certain operations on inexact numbers,
however, produce an exact number, such as multiplying an inexact
number with an exact @scheme[0]. Some operations, which can produce an
irrational number for rational arguments (e.g., @scheme[sqrt]), may
produce inexact results even for exact arguments.

In the case of complex numbers, either the real and imaginary parts
are both exact or inexact, or the number has an exact zero real part
and an inexact imaginary part; a complex number with an exact zero
imaginary part is a real number.

Inexact real numbers are implemented as either single- or
double-precision @as-index{IEEE floating-point numbers}---the latter
by default, and the former only when support for 32-bit inexact
numbers is specifically enabled when the run-time system is built, and
when computation starts with numerical constants specified as
single-precision numbers.

The precision and size of exact numbers is limited only by available
memory (and the precision of operations that can produce irrational
numbers). In particular, adding, multiplying, subtracting, and
dividing exact numbers always produces an exact result.

Inexact numbers can be coerced to exact form, except for the inexact
numbers @scheme[+inf.0], @scheme[-inf.0], and @scheme[+nan.0], which
have no exact form. @index["division by inexact zero"]{Dividing} a
number by exact zero raises an exception; dividing a non-zero number
other than @scheme[+nan.0] by an inexact zero returns @scheme[+inf.0]
or @scheme[-inf.0], depending on the sign of the dividend. The
@scheme[+nan.0] value is not @scheme[=] to itself, but @scheme[+nan.0]
is @scheme[eqv?] to itself. Conversely, @scheme[(= 0.0 -0.0)] is
@scheme[#t], but @scheme[(eqv? 0.0 -0.0)] is @scheme[#f]. The datum
@schemevalfont{-nan.0} refers to the same constant as @scheme[+nan.0].

Calculations with infinites produce results consistent with IEEE
double-precision floating point where IEEE specifies the result; in
cases where IEEE provides no specification (e.g., @scheme[(angle
+inf.0+inf.0i)]), the result corresponds to the limit approaching
infinity, or @scheme[+nan.0] if no such limit exists.

A @deftech{fixnum} is an exact integer whose two's complement
representation fit into 31 bits on a 32-bit platform or 63 bits on a
64-bit platform; furthermore, no allocation is required when computing
with fixnums. See also the @schememodname[scheme/fixnum] module, below.

Two fixnums that are @scheme[=] are also the same
according to @scheme[eq?]. Otherwise, the result of @scheme[eq?]
applied to two numbers is undefined.

Two numbers are @scheme[eqv?] when they are both inexact or both
exact, and when they are @scheme[=] (except for @scheme[+nan.0],
@scheme[+0.0], and @scheme[-0.0], as noted above). Two numbers are
@scheme[equal?] when they are @scheme[eqv?].

@; ----------------------------------------
@section{Number Types}

@defproc[(number? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v]
 is a number, @scheme[#f] otherwise.

@mz-examples[(number? 1) (number? 2+3i) (number? "hello")]}


@defproc[(complex? [v any/c]) boolean?]{ Returns @scheme[(number? v)],
because all numbers are @tech{complex numbers}.}


@defproc[(real? [v any/c]) boolean?]{ Returns @scheme[#t] if @scheme[v] is
 a @techlink{real number}, @scheme[#f] otherwise.

@mz-examples[(real? 1) (real? +inf.0) (real? 2+3i) 
             (real? 2+0.0i) (real? "hello")]}


@defproc[(rational? [v any/c]) boolean?]{ Returns @scheme[#t] if
 @scheme[v] is a @techlink{rational number}, @scheme[#f] otherwise.

@mz-examples[(rational? 1) (rational? +inf.0) (real? "hello")]}


@defproc[(integer? [v any/c]) boolean?]{ Returns @scheme[#t] if @scheme[v]
 is a number that is an @techlink{integer}, @scheme[#f] otherwise.

@mz-examples[(integer? 1) (integer? 2.3) (integer? 4.0) (integer? +inf.0) 
             (integer? 2+3i) (integer? "hello")]}


@defproc[(exact-integer? [v any/c]) boolean?]{

Returns @scheme[(and (integer? v) (exact? v))].

@mz-examples[(exact-integer? 1) (exact-integer? 4.0)]}


@defproc[(exact-nonnegative-integer? [v any/c]) boolean?]{

Returns @scheme[(and (exact-integer? v) (not (negative? v)))].

@mz-examples[(exact-nonnegative-integer? 0) (exact-nonnegative-integer? -1)]}


@defproc[(exact-positive-integer? [v any/c]) boolean?]{

Returns @scheme[(and (exact-integer? v) (positive? v))].

@mz-examples[(exact-positive-integer? 1) (exact-positive-integer? 0)]}


@defproc[(inexact-real? [v any/c]) boolean?]{

Returns @scheme[(and (real? v) (inexact? v))].}


@defproc[(fixnum? [v any/c]) boolean?]{

Return @scheme[#t] if @scheme[v] is a @techlink{fixnum}, @scheme[#f]
otherwise.}


@defproc[(zero? [z number?]) boolean?]{ Returns @scheme[(= 0 z)].

@mz-examples[(zero? 0) (zero? -0.0)]}


@defproc[(positive? [x real?]) boolean?]{ Returns @scheme[(> x 0)].

@mz-examples[(positive? 10) (positive? -10) (positive? 0.0)]}


@defproc[(negative? [x real?]) boolean?]{ Returns @scheme[(< x 0)].

@mz-examples[(negative? 10) (negative? -10) (negative? -0.0)]}


@defproc[(even? [n integer?]) boolean?]{ Returns @scheme[(zero? (modulo
 n 2))].

@mz-examples[(even? 10.0) (even? 11) (even? +inf.0)]}


@defproc[(odd? [n integer?]) boolean?]{ Returns @scheme[(not (even? n))].

@mz-examples[(odd? 10.0) (odd? 11) (odd? +inf.0)]}


@defproc[(exact? [z number?]) boolean?]{ Returns @scheme[#t] if @scheme[z]
 is an exact number, @scheme[#f] otherwise.

@mz-examples[(exact? 1) (exact? 1.0)]}


@defproc[(inexact? [z number?]) boolean?]{ Returns @scheme[#t] if @scheme[z]
 is an inexact number, @scheme[#f] otherwise.

@mz-examples[(inexact? 1) (inexact? 1.0)]}


@defproc[(inexact->exact [z number?]) exact?]{ Coerces @scheme[z] to an
 exact number. If @scheme[z] is already exact, it is returned. If @scheme[z]
 is @scheme[+inf.0], @scheme[-inf.0], or @scheme[+nan.0], then the
 @exnraise[exn:fail:contract].

@mz-examples[(inexact->exact 1) (inexact->exact 1.0)]}


@defproc[(exact->inexact [z number?]) inexact?]{ Coerces @scheme[z] to an
 inexact number. If @scheme[z] is already inexact, it is returned.

@mz-examples[(exact->inexact 1) (exact->inexact 1.0)]}


@; ----------------------------------------
@section{Arithmetic}

@defproc[(+ [z number?] ...) number?]{ Returns the sum of the
 @scheme[z]s, adding pairwise from left to right. If no arguments are
 provided, the result is @scheme[0].

@mz-examples[(+ 1 2) (+ 1.0 2+3i 5) (+)]}


@defproc*[([(- [z number?]) number?]
           [(- [z number?] [w number?] ...+) number?])]{
 When no @scheme[w]s are supplied, returns @scheme[(- 0 z)].
 Otherwise, returns the subtraction of the @scheme[w]s from @scheme[z]
 working pairwise from left to right.}

@mz-examples[(- 5 3.0) (- 1) (- 2+7i 1 3)]


@defproc[(* [z number?] ...) number?]{ Returns the product of the
 @scheme[z]s, multiplying pairwise from left to right. If no arguments are
 provided, the result is @scheme[1].}

@mz-examples[(* 2 3) (* 8.0 9) (* 1+2i 3+4i)]


@defproc*[([(/ [z number?]) number?]
           [(/ [z number?] [w number?] ...+) number?])]{
 When no @scheme[w]s are supplied, returns @scheme[(/ 1 z)].
 Otherwise, returns the division @scheme[z] by the var[w]s
 working pairwise from left to right.}

@mz-examples[(/ 3 4) (/ 81 3 3) (/ 10.0) (/ 1+2i 3+4i)]


@defproc[(quotient [n integer?] [m integer?]) integer?]{ Returns
 @scheme[(truncate (/ n m))].}

@mz-examples[(quotient 10 3) (quotient -10.0 3) (quotient +inf.0 3)]


@defproc[(remainder [n integer?] [m integer?]) integer?]{ Returns
 @scheme[_q] with the same sign as @scheme[n] such that

@itemize[

 @item{@scheme[(abs _q)] is between @scheme[0] (inclusive) and @scheme[(abs m)] (exclusive), and}

 @item{@scheme[(+ _q (* m (quotient n m)))] equals @scheme[n].}

]

@mz-examples[(remainder 10 3) (remainder -10.0 3) (remainder 10.0 -3) (remainder -10 -3) (remainder +inf.0 3)]}


@defproc[(quotient/remainder [n integer?] [m integer?]) (values number? number?)]{ Returns
 @scheme[(values (quotient n m) (remainder n m))], but the combination is computed
 more efficiently than separate calls to @scheme[quotient] and @scheme[remainder].

@mz-examples[
(quotient/remainder 10 3)
]}


@defproc[(modulo [n integer?] [m integer?]) number?]{  Returns
 @scheme[_q] with the same sign as @scheme[m] where

@itemize[

 @item{@scheme[(abs _q)] is between @scheme[0] (inclusive) and @scheme[(abs m)] (exclusive), and}

 @item{the difference between @scheme[_q] and @scheme[(- n (* m (quotient n m)))] is a multiple of @scheme[m].}

]

@mz-examples[(modulo 10 3) (modulo -10.0 3)  (modulo 10.0 -3) (modulo -10 -3) (modulo +inf.0 3)]}


@defproc[(add1 [z number?]) number?]{ Returns @scheme[(+ z 1)].}

@defproc[(sub1 [z number?]) number?]{ Returns @scheme[(- z 1)].}

@defproc[(abs [x real?]) number?]{ Returns the absolute value of
 @scheme[x].

@mz-examples[(abs 1.0) (abs -1)]}

@defproc[(max [x real?] ...+) real?]{ Returns the largest of the
 @scheme[x]s, or @scheme[+nan.0] if any @scheme[x] is @scheme[+nan.0].
 If any @scheme[x] is inexact, the result is coerced to inexact.

@mz-examples[(max 1 3 2) (max 1 3 2.0)]}


@defproc[(min [x real?] ...+) real?]{ Returns the smallest of the
 @scheme[x]s, or @scheme[+nan.0] if any @scheme[x] is @scheme[+nan.0].
 If any @scheme[x] is inexact, the result is coerced to inexact.

@mz-examples[(min 1 3 2) (min 1 3 2.0)]}


@defproc[(gcd [n integer?] ...) integer?]{ Returns the
 @as-index{greatest common divisor} (a non-negative number) of the
 @scheme[n]s. If no arguments are provided, the result is
 @scheme[0]. If all arguments are zero, the result is zero.

@mz-examples[(gcd 10) (gcd 12 81.0)]}


@defproc[(lcm [n integer?] ...) integer?]{ Returns the @as-index{least
 common multiple} (a non-negative number) of the @scheme[n]s. If no
 arguments are provided, the result is @scheme[1]. If any argument is
 zero, the result is zero.

@mz-examples[(lcm 10) (lcm 3 4.0)]}


@defproc[(round [x real?]) integer?]{ Returns the integer closest to
 @scheme[x], resolving ties in favor of an even number.

@mz-examples[(round 17/4) (round -17/4) (round 2.5) (round -2.5)]}


@defproc[(floor [x real?]) integer?]{ Returns the largest integer is that
 is no more than @scheme[x].

@mz-examples[(floor 17/4) (floor -17/4) (floor 2.5) (floor -2.5)]}


@defproc[(ceiling [x real?]) integer?]{ Returns the smallest integer is
 that is at least as large as @scheme[x].

@mz-examples[(ceiling 17/4) (ceiling -17/4) (ceiling 2.5) (ceiling -2.5)]}


@defproc[(truncate [x real?]) integer?]{ Returns the integer farthest
 from @scheme[0] that is no closer to @scheme[0] than @scheme[x].

@mz-examples[(truncate 17/4) (truncate -17/4) (truncate 2.5) (truncate -2.5)]}


@defproc[(numerator [q rational?]) integer?]{
 Coreces @scheme[q] to an exact number, finds the numerator of the number
 expressed in its simplest fractional form, and returns this number
 coerced to the exactness of @scheme[q].

@mz-examples[(numerator 5) (numerator 34/8) (numerator 2.3)]}


@defproc[(denominator [q rational?]) integer?]{
 Coreces @scheme[q] to an exact number, finds the numerator of the number
 expressed in its simplest fractional form, and returns this number
 coerced to the exactness of @scheme[q].

@mz-examples[(denominator 5) (denominator 34/8) (denominator 2.3)]}


@defproc[(rationalize [x real?][tolerance real?]) real?]{

Among the real numbers within @scheme[(abs tolerance)] of @scheme[x],
returns the one corresponding to an exact number whose
@scheme[denominator] is smallest.  If multiple integers are within
@scheme[tolerance] of @scheme[x], the one closest to @scheme[0] is
used.

@mz-examples[
(rationalize 1/4 1/10)
(rationalize -1/4 1/10)
(rationalize 1/4 1/4)
(rationalize 11/40 1/4)
]}

@; ----------------------------------------
@section{Number Comparison}

@defproc[(= [z number?] [w number?] ...+) boolean?]{ Returns
 @scheme[#t] if all of the arguments are numerically equal,
 @scheme[#f] otherwise.  An inexact number is numerically equal to an
 exact number when the exact coercion of the inexact number is the
 exact number. Also, @scheme[0.0] and @scheme[-0.0] are numerically
 equal, but @scheme[+nan.0] is not numerically equal to itself.

@mz-examples[(= 1 1.0) (= 1 2) (= 2+3i 2+3i 2+3i)]}


@defproc[(< [x real?] [y real?] ...+) boolean?]{ Returns @scheme[#t] if
 the arguments in the given order are in strictly increasing,
 @scheme[#f] otherwise.

@mz-examples[(< 1 1) (< 1 2 3) (< 1 +inf.0) (< 1 +nan.0)]}


@defproc[(<= [x real?] [y real?] ...+) boolean?]{ Returns @scheme[#t]
 if the arguments in the given order are in non-decreasing,
 @scheme[#f] otherwise.

@mz-examples[(<= 1 1) (<= 1 2 1)]}


@defproc[(> [x real?] [y real?] ...+) boolean?]{ Returns @scheme[#t] if
 the arguments in the given order are in strictly decreasing,
 @scheme[#f] otherwise.

@mz-examples[(> 1 1) (> 3 2 1) (> +inf.0 1) (< +nan.0 1)]}


@defproc[(>= [x real?] [y real?] ...+) boolean?]{ Returns @scheme[#t]
 if the arguments in the given order are in non-increasing,
 @scheme[#f] otherwise.

@mz-examples[(>= 1 1) (>= 1 2 1)]}


@; ------------------------------------------------------------------------
@section{Powers and Roots}

@defproc[(sqrt [z number?]) number?]{ Returns the principal
 @as-index{square root} of @scheme[z].  The result is exact if
 @scheme[z] is exact and @scheme[z]'s square root is rational. See
 also @scheme[integer-sqrt].

@mz-examples[(sqrt 4/9) (sqrt 2) (sqrt -1)]}


@defproc[(integer-sqrt [n integer?]) complex?]{ Returns @scheme[(floor
 (sqrt n))] for positive @scheme[n]. For negative @scheme[n], the result is
 @scheme[(* (integer-sqrt (- n)) 0+i)].

@mz-examples[(integer-sqrt 4.0) (integer-sqrt 5)]}


@defproc[(integer-sqrt/remainder [n integer?])
         (values integer? integer?)]{
 Returns @scheme[(integer-sqrt n)] and @scheme[(- n (expt
 (integer-sqrt n) 2))].

@mz-examples[(integer-sqrt/remainder 4.0) (integer-sqrt/remainder 5)]}

@defproc[(expt [z number?] [w number?]) number?]{ Returns @scheme[z]
 raised to the power of @scheme[w]. If @scheme[w] is exact @scheme[0],
 the result is @scheme[1]. If @scheme[z] is exact @scheme[0] and
 @scheme[w] is negative, the @exnraise[exn:fail:contract].

@mz-examples[(expt 2 3) (expt 4 0.5) (expt +inf.0 0)]}

@defproc[(exp [z number?]) number?]{ Returns Euler's number raised to the
 power of @scheme[z]. The result is normally inexact, but it is
 @scheme[1] when @scheme[z] is an exact @scheme[0].

@mz-examples[(exp 1) (exp 2+3i) (exp 0)]}


@defproc[(log [z number?]) number?]{ Returns the natural logarithm of
 @scheme[z].  The result is normally inexact, but it is
 @scheme[0] when @scheme[z] is an exact @scheme[1]. When @scheme[z]
 is exact @scheme[0], @exnraise[exn:fail:contract:divide-by-zero].}

@mz-examples[(log (exp 1)) (log 2+3i) (log 1)]}


@; ------------------------------------------------------------------------
@section{Trignometric Functions}

@defproc[(sin [z number?]) number?]{ Returns the sine of @scheme[z], where
 @scheme[z] is in radians.

@mz-examples[(sin 3.14159) (sin 1+05.i)]}


@defproc[(cos [z number?]) number?]{ Returns the cosine of @scheme[z],
 where @scheme[z] is in radians.

@mz-examples[(cos 3.14159) (cos 1+05.i)]}


@defproc[(tan [z number?]) number?]{ Returns the tangent of @scheme[z],
 where @scheme[z] is in radians.

@mz-examples[(tan 0.7854) (tan 1+05.i)]}


@defproc[(asin [z number?]) number?]{ Returns the arcsin in radians of @scheme[z].

@mz-examples[(asin 0.25) (asin 1+05.i)]}


@defproc[(acos [z number?]) number?]{ Returns the arccosine in radians
 of @scheme[z].

@mz-examples[(acos 0.25) (acos 1+05.i)]}


@defproc*[([(atan [z number?]) number?]
           [(atan [y real?] [x real?]) number?])]{

In the one-argument case, returns the arctangent of the inexact
approximation of @scheme[z], except that the result is an exact
@scheme[0] for an exact @scheme[0] argument.

In the two-argument case, the result is roughly the same as @scheme[(/
(exact->inexact y) (exact->inexact x))], but the signs of @scheme[y]
and @scheme[x] determine the quadrant of the result. Moreover, a
suitable angle is returned when @scheme[y] divided by @scheme[x]
produces @scheme[+nan.0] in the case that neither @scheme[y] nor
@scheme[x] is @scheme[+nan.0].

@mz-examples[(atan 0.5) (atan 2 1) (atan -2 -1) (atan 1+05.i) (atan +inf.0 -inf.0)]}

@; ------------------------------------------------------------------------
@section{Complex Numbers}

@defproc[(make-rectangular [x real?] [y real?]) number?]{ Returns
 @scheme[(+ x (* y 0+1i))].

@mz-examples[(make-rectangular 3 4.0)]}


@defproc[(make-polar [magnitude real?] [angle real?]) number?]{ Returns
 @scheme[(+ (* magnitude (cos angle)) (* magnitude (sin angle) 0+1i))].

@mz-examples[#:eval math-eval
                    (make-polar 10 (* pi 1/2))
                    (make-polar 10 (* pi 1/4))]}


@defproc[(real-part [z number?]) real?]{ Returns the real part of
 the complex number @scheme[z] in rectangle coordinates.

@mz-examples[(real-part 3+4i) (real-part 5.0)]}


@defproc[(imag-part [z number?]) real?]{ Returns the imaginary part of
 the complex number @scheme[z] in rectangle coordinates.

@mz-examples[(imag-part 3+4i) (imag-part 5.0) (imag-part 5.0+0.0i)]}


@defproc[(magnitude [z number?]) (and/c real? (not/c negative?))]{
 Returns the magnitude of the complex number @scheme[z] in polar
 coordinates.

@mz-examples[(magnitude -3) (magnitude 3.0) (magnitude 3+4i)]}


@defproc[(angle [z number?]) real?]{ Returns the angle of
 the complex number @scheme[z] in polar coordinates.

@mz-examples[(angle -3) (angle 3.0) (angle 3+4i) (angle +inf.0+inf.0i)]}

@; ------------------------------------------------------------------------
@section{Bitwise Operations}

@section-index{logical operators}

@defproc[(bitwise-ior [n exact-integer?] ...) exact-integer?]{ Returns
 the bitwise ``inclusive or'' of the @scheme[n]s in their (semi-infinite)
 two's complement representation. If no arguments are provided, the
 result is @scheme[0].

@mz-examples[(bitwise-ior 1 2) (bitwise-ior -32 1)]}


@defproc[(bitwise-and [n exact-integer?] ...) exact-integer?]{ Returns
 the bitwise ``and'' of the @scheme[n]s in their (semi-infinite) two's
 complement representation. If no arguments are provided, the result
 is @scheme[-1].

@mz-examples[(bitwise-and 1 2) (bitwise-and -32 -1)]}


@defproc[(bitwise-xor [n exact-integer?] ...) exact-integer?]{ Returns
 the bitwise ``exclusive or'' of the @scheme[n]s in their (semi-infinite)
 two's complement representation. If no arguments are provided, the
 result is @scheme[0].

@mz-examples[(bitwise-xor 1 5) (bitwise-xor -32 -1)]}


@defproc[(bitwise-not [n exact-integer?])  exact-integer?]{ Returns the
 bitwise ``not'' of @scheme[n] in its (semi-infinite) two's complement
 representation.

@mz-examples[(bitwise-not 5) (bitwise-not -1)]}


@defproc[(bitwise-bit-set? [n exact-integer?] [m exact-nonnegative-integer?])
         boolean?]{

Returns @scheme[#t] when the @scheme[m]th bit of @scheme[n] is set in @scheme[n]'s
        (semi-infinite) two's complement representation.
                   
This is equivalent to
@scheme[(not (zero? (bitwise-and n (arithmetic-shift 1 m))))],
but is faster and runs in constant time when @scheme[n] is positive.

@mz-examples[(bitwise-bit-set? 5 0) (bitwise-bit-set? 5 2) (bitwise-bit-set? -5 (expt 2 700))]}


@defproc[(bitwise-bit-field [n exact-integer?] 
                            [start exact-nonnegative-integer?] 
                            [end (and/c exact-nonnegative-integer?
                                        (start . <= . end))])
         exact-integer?]{

Extracts the bits between position @scheme[start] and @scheme[(- end 1)] (inclusive)
from @scheme[n] and shifts them down to the least significant portion of the number.

This is equivalent to this computation,

@schemeblock[
(bitwise-and (sub1 (arithmetic-shift 1 (- end start)))
             (arithmetic-shift n (- start)))
]

but it runs in constant time when @scheme[n] is positive, @scheme[start] and
@scheme[end] are fixnums, and @scheme[(- end start)] is no more than
the maximum width of a fixnum.

Each pair of examples below uses the same numbers, but shows the result in
both binary and as integers.

@mz-examples[(format "~b" (bitwise-bit-field (string->number "1101" 2) 1 1))
             (bitwise-bit-field 13 1 1)
             (format "~b" (bitwise-bit-field (string->number "1101" 2) 1 3))
             (bitwise-bit-field 13 1 3)
             (format "~b" (bitwise-bit-field (string->number "1101" 2) 1 4))
             (bitwise-bit-field 13 1 4)]
}


@defproc[(arithmetic-shift [n exact-integer?] [m exact-integer?])
 exact-integer?]{ Returns the bitwise ``shift'' of @scheme[n] in its
 (semi-infinite) two's complement representation.  If @scheme[m] is
 non-negative, the integer @scheme[n] is shifted left by @scheme[m] bits;
 i.e., @scheme[m] new zeros are introduced as rightmost digits. If
 @scheme[m] is negative, @scheme[n] is shifted right by @scheme[(- m)]
 bits; i.e., the rightmost @scheme[m] digits are dropped.

@mz-examples[(arithmetic-shift 1 10) (arithmetic-shift 255 -3)]}

@defproc[(integer-length [n exact-integer?]) exact-integer?]{ Returns
 the number of bits in the (semi-infinite) two's complement
 representation of @scheme[n] after removing all leading zeros (for
 non-negative @scheme[n]) or ones (for negative @scheme[n]).

@mz-examples[(integer-length 8) (integer-length -8)]}

@; ------------------------------------------------------------------------
@section{Random Numbers}

@defproc*[([(random [k (integer-in 1 4294967087)]
                    [generator pseudo-random-generator?
                               (current-pseudo-random-generator)])
            exact-nonnegative-integer?]
           [(random [generator pseudo-random-generator?
                               (current-pseudo-random-generator)]) 
            (and/c real? inexact? (>/c 0) (</c 1))])]{  

When called with and integer argument @scheme[k], returns a random
exact integer in the range @scheme[0] to @math{@scheme[k]-1}. When
called with zero arguments, returns a random inexact number between
@scheme[0] and @scheme[1], exclusive.

In each case, the number is provided by the given pseudo-random number
generator (which defaults to the current one, as produced by
@scheme[current-pseudo-random-generator]). The generator maintains an
internal state for generating numbers. The random number generator
uses a 54-bit version of L'Ecuyer's MRG32k3a algorithm
@cite["L'Ecuyer02"].}


@defproc[(random-seed [k (integer-in 1 (sub1 (expt 2 31)))])
          void?]{

Seeds the current pseudo-random number generator with
@scheme[k]. Seeding a generator sets its internal state
deterministically; that is, seeding a generator with a particular
number forces it to produce a sequence of pseudo-random numbers that
is the same across runs and across platforms.}


@defproc[(make-pseudo-random-generator) pseudo-random-generator?]{

Returns a new pseudo-random number generator. The new generator is
seeded with a number derived from @scheme[(current-milliseconds)].}


@defproc[(pseudo-random-generator? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a pseudo-random number generator,
@scheme[#f] otherwise.}


@defparam[current-pseudo-random-generator generator pseudo-random-generator?]{

A parameter that determines the pseudo-random number generator
used by @scheme[random].}


@defproc[(pseudo-random-generator->vector [generator pseudo-random-generator?])
         vector?]{

Produces a vector that represents the complete internal state of
@scheme[generator]. The vector is suitable as an argument to
@scheme[vector->pseudo-random-generator] to recreate the generator in
its current state (across runs and across platforms).}


@defproc[(vector->pseudo-random-generator [vec vector?])
         pseudo-random-generator?]{

Produces a pseudo-random number generator whose internal state
corresponds to @scheme[vec]. The vector @scheme[vec] must contain six
exact integers; the first three integers must be in the range
@scheme[0] to @scheme[4294967086], inclusive; the last three integers
must be in the range @scheme[0] to @scheme[4294944442], inclusive; at
least one of the first three integers must be non-zero; and at least
one of the last three integers must be non-zero.}

@defproc[(vector->pseudo-random-generator! [generator pseudo-random-generator?]
                                           [vec vector?])
         void?]{

Like @scheme[vector->pseudo-random-generator], but changes
@scheme[generator] to the given state, instead of creating a new
generator.}

@; ------------------------------------------------------------------------
@section{Number--String Conversions}

@section-index["numbers" "machine representations"]
@section-index["numbers" "floating-point"]
@section-index["numbers" "big-endian"]
@section-index["numbers" "little-endian"]
@section-index["numbers" "converting"]

@defproc[(number->string [z number?]
                         [radix (or/c 2 8 10 16) 10]) string?]{
 Returns a string that is the printed form of @scheme[z]
 in the base specific by @scheme[radix]. If @scheme[z] is inexact,
 @scheme[radix] must be @scheme[10], otherwise the
 @exnraise[exn:fail:contract].

@mz-examples[(number->string 3.0) (number->string 255 8)]}


@defproc[(string->number [s string?] [radix (integer-in 2 16) 10]) 
         (or/c number? #f)]{

Reads and returns a number datum from @scheme[s] (see
@secref["parse-number"]), returning @scheme[#f] if @scheme[s] does not
parse exactly as a number datum (with no whitespace). The optional
@scheme[radix] argument specifies the default base for the number,
which can be overriden by @litchar{#b}, @litchar{#o}, @litchar{#d}, or
@litchar{#x} in the string.

@mz-examples[(string->number "3.0+2.5i") (string->number "hello")
          (string->number "111" 7)  (string->number "#b111" 7)]
}

@defproc[(real->decimal-string [n real?] [decimal-digits exact-nonnegative-integer? 2])
         string?]{

Prints @scheme[n] into a string and returns the string. The printed
form of @scheme[n] shows exactly @scheme[decimal-digits] digits after
the decimal point. The printed for uses a minus sign if @scheme[n] is
negative, and it does not use a plus sign if @scheme[n] is positive.

Before printing, @scheme[n] is converted to an exact number,
multiplied by @scheme[(expt 10 decimal-digits)], rounded, and then
divided again by @scheme[(expt 10 decimal-digits)].  The result of ths
process is an exact number whose decimal representation has no more
than @scheme[decimal-digits] digits after the decimal (and it is
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

Converts the machine-format number encoded in @scheme[bstr] to an
exact integer. The @scheme[start] and @scheme[end] arguments specify
the substring to decode, where @scheme[(- end start)] must be
@scheme[2], @scheme[4], or @scheme[8]. If @scheme[signed?] is true,
then the bytes are decoded as a two's-complement number, otherwise it
is decoded as an unsigned integer. If @scheme[big-endian?] is true,
then the first character's ASCII value provides the most significant
eight bits of the number, otherwise the first character provides the
least-significant eight bits, and so on.}


@defproc[(integer->integer-bytes [n exact-integer?]
                                 [size-n (or/c 2 4 8)]
                                 [signed? any/c]
                                 [big-endian? any/c (system-big-endian?)]
                                 [dest-bstr (and/c bytes? 
                                                   (not/c immutable?))
                                            (make-bytes size-n)]
                                 [start exact-nonnegative-integer? 0])
          bytes?]{

Converts the exact integer @scheme[n] to a machine-format number
encoded in a byte string of length @scheme[size-n], which must be
@scheme[2], @scheme[4], or @scheme[8]. If @scheme[signed?] is true,
then the number is encoded as two's complement, otherwise it is
encoded as an unsigned bit stream. If @scheme[big-endian?] is true,
then the most significant eight bits of the number are encoded in the
first character of the resulting byte string, otherwise the
least-significant bits are encoded in the first byte, and so on.

The @scheme[dest-bstr] argument must be a mutable byte string of
length @scheme[size-n]. The encoding of @scheme[n] is written into
@scheme[dest-bstr] starting at offset @scheme[start], and
@scheme[dest-bstr] is returned as the result.

If @scheme[n] cannot be encoded in a string of the requested size and
format, the @exnraise[exn:fail:contract]. If @scheme[dest-bstr] is not
of length @scheme[size-n], the @exnraise[exn:fail:contract].}


@defproc[(floating-point-bytes->real [bstr bytes?]
                                     [big-endian? any/c (system-big-endian?)]
                                     [start exact-nonnegative-integer? 0]
                                     [end exact-nonnegative-integer? (bytes-length bstr)])
         (and/c real? inexact?)]{

Converts the IEEE floating-point number encoded in @scheme[bstr] from
position @scheme[start] (inclusive) to @scheme[end] (exclusive) to an
inexact real number. The difference between @scheme[start] an
@scheme[end] must be either 4 or 8 bytes. If @scheme[big-endian?] is
true, then the first byte's ASCII value provides the most significant
eight bits of the IEEE representation, otherwise the first byte
provides the least-significant eight bits, and so on.}


@defproc[(real->floating-point-bytes [x real?]
                                     [size-n (or/c 4 8)]
                                     [big-endian? any/c (system-big-endian?)]
                                     [dest-bstr (and/c bytes? 
                                                       (not/c immutable?))
                                                 (make-bytes size-n)]
                                     [start exact-nonnegative-integer? 0])
          bytes?]{

Converts the real number @scheme[x] to its IEEE representation in a
byte string of length @scheme[size-n], which must be @scheme[4] or
@scheme[8]. If @scheme[big-endian?] is true, then the most significant
eight bits of the number are encoded in the first byte of the
resulting byte string, otherwise the least-significant bits are
encoded in the first character, and so on.

The @scheme[dest-bstr] argument must be a mutable byte string of
length @scheme[size-n]. The encoding of @scheme[n] is written into
@scheme[dest-bstr] starting with byte @scheme[start], and
@scheme[dest-bstr] is returned as the result.

If @scheme[dest-bstr] is provided and it has less than @scheme[start]
plus @scheme[size-n] bytes, the @exnraise[exn:fail:contract].}


@defproc[(system-big-endian?) boolean?]{

Returns @scheme[#t] if the native encoding of numbers is big-endian
for the machine running Scheme, @scheme[#f] if the native encoding
is little-endian.}

@; ------------------------------------------------------------------------
@section{Inexact-Real (Flonum) Operations}

@defmodule[scheme/flonum]

The @schememodname[scheme/flonum] library provides operations like
@scheme[fl+] that consume and produce only real @tech{inexact
numbers}, which are also known as @deftech{flonums}. Flonum-specific
operations provide can better performance when used consistently, and
they are as safe as generic operations like @scheme[+].

@margin-note{See @guidesecref["fixnums+flonums"].}

@subsection{Flonum Arithmetic}

@deftogether[(
@defproc[(fl+ [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(fl- [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(fl* [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(fl/ [a inexact-real?][b inexact-real?]) inexact-real?]
@defproc[(flabs [a inexact-real?]) inexact-real?]
)]{

Like @scheme[+], @scheme[-], @scheme[*], @scheme[/], and @scheme[abs],
but constrained to consume @tech{flonums}. The result is always a
@tech{flonum}.}

@deftogether[(
@defproc[(fl= [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(fl< [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(fl> [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(fl<= [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(fl>= [a inexact-real?][b inexact-real?]) boolean?]
@defproc[(flmin [a inexact-real?]) inexact-real?]
@defproc[(flmax [a inexact-real?]) inexact-real?]
)]{

Like @scheme[=], @scheme[<], @scheme[>], @scheme[<=], @scheme[>=],
@scheme[min], and @scheme[max], but constrained to consume
@tech{flonums}.}

@deftogether[(
@defproc[(flround [a inexact-real?]) inexact-real?]
@defproc[(flfloor [a inexact-real?]) inexact-real?]
@defproc[(flceiling [a inexact-real?]) inexact-real?]
@defproc[(fltruncate [a inexact-real?]) inexact-real?]
)]{

Like @scheme[round], @scheme[floor], @scheme[ceiling], and
@scheme[truncate], but constrained to consume @tech{flonums}.}

@deftogether[(
@defproc[(flsin [a inexact-real?]) inexact-real?]
@defproc[(flcos [a inexact-real?]) inexact-real?]
@defproc[(fltan [a inexact-real?]) inexact-real?]
@defproc[(flasin [a inexact-real?]) inexact-real?]
@defproc[(flacos [a inexact-real?]) inexact-real?]
@defproc[(flatan [a inexact-real?]) inexact-real?]
@defproc[(fllog [a inexact-real?]) inexact-real?]
@defproc[(flexp [a inexact-real?]) inexact-real?]
@defproc[(flsqrt [a inexact-real?]) inexact-real?]
)]{

Like @scheme[sin], @scheme[cos], @scheme[tan], @scheme[asin],
@scheme[acos], @scheme[atan], @scheme[log], @scheme[exp], and
@scheme[flsqrt], but constrained to consume and produce
@tech{flonums}. The result is @scheme[+nan.0] when a number outside
the range @scheme[-1.0] to @scheme[1.0] is given to @scheme[flasin] or
@scheme[flacos], or when a negative number is given to @scheme[fllog]
or @scheme[flsqrt].}

@defproc[(->fl [a exact-integer?]) inexact-real?]{
Like @scheme[exact->inexact], but constrained to consume exact integers,
so the result is always a @tech{flonum}.
}

@subsection{Flonum Vectors}

A @deftech{flvector} is like a @tech{vector}, but it holds only
inexact real numbers. This representation can be more compact, and
unsafe operations on @tech{flvector}s (see
@schememodname[scheme/unsafe/ops]) can execute more efficiently than
unsafe operations on @tech{vectors} of inexact reals.

An f64vector as provided by @schememodname[scheme/foreign] stores the
same kinds of values as an @tech{flvector}, but with extra
indirections that make f64vectors more convenient for working with
foreign libraries. The lack of indirections make unsafe
@tech{flvector} access more efficient.

Two @tech{flvectors} are @scheme[equal?] if they have the same length,
and if the values in corresponding slots of the @tech{flvectors} are
@scheme[equal?].

@defproc[(flvector? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{flvector}, @scheme[#f] otherwise.}

@defproc[(flvector [x inexact-real?] ...) flvector?]{

Creates a @tech{flvector} containing the given inexact real numbers.}

@defproc[(make-flvector [size exact-nonnegative-integer?]
                        [x inexact-real? 0.0]) 
         flvector?]{

Creates a @tech{flvector} with @scheme[size] elements, where every
slot in the @tech{flvector} is filled with @scheme[x].}

@defproc[(flvector-length [vec flvector?]) exact-nonnegative-integer?]{

Returns the length of @scheme[vec] (i.e., the number of slots in the
@tech{flvector}).}


@defproc[(flvector-ref [vec flvector?] [pos exact-nonnegative-integer?])
         inexact-real?]{

Returns the inexact real number in slot @scheme[pos] of
@scheme[vec]. The first slot is position @scheme[0], and the last slot
is one less than @scheme[(flvector-length vec)].}

@defproc[(flvector-set! [vec flvector?] [pos exact-nonnegative-integer?]
                        [x inexact-real?])
         inexact-real?]{

Sets the inexact real number in slot @scheme[pos] of @scheme[vec]. The
first slot is position @scheme[0], and the last slot is one less than
@scheme[(flvector-length vec)].}

                       
@section{Fixnum Operations}

@defmodule[scheme/fixnum]

The @schememodname[scheme/fixnum] library provides operations like
@scheme[fx+] that consume and produce only fixnums. The operations in
this library are meant to be safe versions of unsafe operations like
@scheme[unsafe-fx+]. These safe operations are generally no faster
than using generic primitives like @scheme[+].

The expected use of the @schememodname[scheme/fixnum] library is for
code where the @scheme[require] of @schememodname[scheme/fixnum] is
replaced with

@schemeblock[(require (filtered-in
                       (λ (name) (regexp-replace #rx"unsafe-" name ""))
                       scheme/unsafe/ops))]

to drop in unsafe versions of the library. Alternately, when
encountering crashes with code that uses unsafe fixnum operations, use
the @schememodname[scheme/fixnum] library to help debug the problems.

@deftogether[(
@defproc[(fx+ [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fx- [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fx* [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fxquotient [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fxremainder [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fxmodulo [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fxabs [a fixnum?]) fixnum?]
)]{

Safe versions of @scheme[unsafe-fx+], @scheme[unsafe-fx-],
@scheme[unsafe-fx*], @scheme[unsafe-fxquotient],
@scheme[unsafe-fxremainder], @scheme[unsafe-fxmodulo], and
@scheme[unsafe-fxabs]. The
@exnraise[exn:fail:contract:non-fixnum-result] if the arithmetic
result would not be a fixnum.}


@deftogether[(
@defproc[(fxand [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fxior [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fxxor [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fxnot [a fixnum?]) fixnum?]
@defproc[(fxlshift [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fxrshift [a fixnum?][b fixnum?]) fixnum?]
)]{

Safe versions of @scheme[unsafe-fxand], @scheme[unsafe-fxior],
@scheme[unsafe-fxxor], @scheme[unsafe-fxnot],
@scheme[unsafe-fxlshift], and @scheme[unsafe-fxrshift].  The
@exnraise[exn:fail:contract:non-fixnum-result] if the arithmetic
result would not be a fixnum.}


@deftogether[(
@defproc[(fx= [a fixnum?][b fixnum?]) boolean?]
@defproc[(fx< [a fixnum?][b fixnum?]) boolean?]
@defproc[(fx> [a fixnum?][b fixnum?]) boolean?]
@defproc[(fx<= [a fixnum?][b fixnum?]) boolean?]
@defproc[(fx>= [a fixnum?][b fixnum?]) boolean?]
@defproc[(fxmin [a fixnum?][b fixnum?]) fixnum?]
@defproc[(fxmax [a fixnum?][b fixnum?]) fixnum?]
)]{

Safe versions of @scheme[unsafe-fx=], @scheme[unsafe-fx<],
 @scheme[unsafe-fx>], @scheme[unsafe-fx<=], @scheme[unsafe-fx>=],
 @scheme[unsafe-fxmin], and @scheme[unsafe-fxmax].}



@; ------------------------------------------------------------------------
@section{Extra Constants and Functions}

@note-lib[scheme/math]

@defthing[pi real]{

An approximation to the ratio of a circle's circumference to its
diameter: @number->string[pi].}

@defproc[(sqr [z number?]) number?]{

Returns @scheme[(* z z)].}

@defproc[(sgn [x real?]) (or/c 1 0 -1 1.0 0.0 -1.0)]{

Returns the sign of @scheme[x] as either @math{-1}, @math{0}, or
@math{1}.

@mz-examples[
#:eval math-eval
(sgn 10)
(sgn -10.0)
(sgn 0)
]}

@defproc[(conjugate [z number?]) number?]{

Returns the complex conjugate of @scheme[z].

@mz-examples[
#:eval math-eval
(conjugate 1)
(conjugate 3+4i)
]}

@defproc[(sinh [z number?]) number?]{

Returns the hyperbolic sine of @scheme[z].}

@defproc[(cosh [z number?]) number?]{

Returns the hyperbolic cosine of @scheme[z].}

@defproc[(tanh [z number?]) number?]{

Returns the hyperbolic tangent of @scheme[z].}

@defproc[(order-of-magnitude [r (and/c real? positive?)]) (and/c exact? integer?)]{
Computes the greatest exact integer @scheme[m] such that:
@schemeblock[(<= (expt 10 m)
                 (inexact->exact r))]
Hence also:
@schemeblock[(< (inexact->exact r)
                (expt 10 (add1 m)))]

@mz-examples[#:eval math-eval 
                    (order-of-magnitude 999)
                    (order-of-magnitude 1000)
                    (order-of-magnitude 1/100)
                    (order-of-magnitude 1/101)]
}

@; ----------------------------------------------------------------------

@close-eval[math-eval]
