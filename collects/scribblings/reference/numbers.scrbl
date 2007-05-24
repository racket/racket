#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "numbers"]{Numbers}

All numbers are @idefterm{complex numbers}. Some of them are
@idefterm{real numbers}, and all of the real numbers that can be
represented are also @idefterm{rational numbers}. Among the real
numbers, some are @idefterm{integers}, because @scheme[round] applied
to the number produces the same number.

Orthogonal to those categories, each number is also either
@idefterm{exact} or @idefterm{inexact}. Unless otherwise specified,
computations that involve an inexact number produce inexact results,
so that inexactness acts as a kind of taint on numbers. Certain
operations on inexact numbers, however, produce an exact number, such
as multiplying an inexact number with an exact @scheme[0]. Some
operations, which can produce an irrational number for rational
arguments (e.g., @scheme[sqrt]), may produce inexact results even for
exact arguments.

In the case of complex numbers, either the real and imaginary parts
are both exact or inexact, or the number has an exact zero real part
and an inexact imaginary part; a complex number with an zero imaginary
part (inexact or exact) is a real number.

Inexact real numbers are implemented as either single- or
double-precision IEEE floating-point numbers---the latter by default,
and the former only when support for 32-bit inexact numbers is
specifically enabled when the run-time system is built, and when
computation starts with numerical constants specified as
single-precision numbers.

The precision and size of exact numbers is limited only by available
memory (and the precision of operations that can produce irrational
numbers). In particular, adding, multiplying, subtracting, and
dividing exact numbers always produces an extract result.

@index["division by inexact zero"]{Inexact} numbers can be coerced to
exact form, except for the inexact numbers @as-index{@scheme[+inf.0]}
(positive infinity), @as-index{@scheme[-inf.0]} (negative infinity), and
@as-index{@scheme[+nan.0]} (not-a-number), which have no exact
form. Dividing a number by exact zero raises an exception; dividing a
non-zero number other than @scheme[+nan.0] by an inexact zero returns
@scheme[+inf.0] or @scheme[-inf.0], depending on the sign of the
dividend.  The infinities @scheme[+inf.0] and @scheme[-inf.0] are
integers, and they answer @scheme[#t] for both @scheme[even?] and
@scheme[odd?]. The @scheme[+nan.0] value is not an integer and is not
@scheme[=] to itself, but @scheme[+nan.0] is @scheme[eqv?] to
itself. Conversely, @scheme[(= 0.0 -0.0)] is @scheme[#t], but
@scheme[(eqv? 0.0 -0.0)] is @scheme[#f]. The datum @scheme[-nan.0]
refers to the same constant as @scheme[+nan.0].

Calculations with infinites produce results consistent with IEEE
double-precision floating point where IEEE specifies the result; in
cases where IEEE provides no specification (e.g., @scheme[(angle
+inf.0+inf.0)]), the result corresponds to the limit approaching
infinity, or @scheme[+nan.0] if no such limit exists.

A @pidefterm{fixnum} is an exact integer whose two's complement
representation fit into 31 bits on a 32-bit platform or 63 bits on a
64-bit platform. Two fixnums that are @scheme[=] are also the same
according to @scheme[eq?]. Otherwise, the result of @scheme{eq?}
applied to two numbers is undefined.

Two numbers are @scheme[eqv?] when they are both inexact or both
exact, and when they are @scheme[=] (except for @scheme[+nan.0], as
noted above). Two numbers are @scheme[equal?] when they are
@scheme[eqv?].

@defproc[(number? [v any/c]) boolean?]{ Returns @scheme[#t] if @scheme[v]
 is a number, @scheme[#f] otherwise.

@examples[(number? 1) (number? 2+3i) (number? "hello")]}


@defproc[(complex? [v any/c]) boolean?]{ Returns @scheme[(number? #,
 @scheme[v])], because all numbers are complex numbers.}


@defproc[(real? [v any/c]) boolean?]{ Returns @scheme[#t] if @scheme[v] is
 a real number, @scheme[#f] otherwise. A number with an inexact zero
 imaginary part is a real number.

@examples[(real? 1) (real? 2+3i) (real? "hello")]}


@defproc[(rational? [v any/c]) boolean?]{ Returns @scheme[(real? #,
 @scheme[v])].}


@defproc[(integer? [v any/c]) boolean?]{ Returns @scheme[#t] if @scheme[v]
 is a number that is an integer, @scheme[#f] otherwise. The inexact
 numbers @scheme[+inf.0] and @scheme[-inf.0] are integers, but
 @scheme[+nan.0] is not.

@examples[(integer? 1) (integer? 2.3) (integer? 4.0) (integer? 2+3i) (integer? "hello")]}


@defproc[(exact? [z number?]) boolean?]{ Returns @scheme[#t] if @scheme[z]
 is an exact number, @scheme[#f] otherwise.

@examples[(exact? 1) (exact? 1.0)]}


@defproc[(inexact? [z number?]) boolean?]{ Returns @scheme[#t] if @scheme[z]
 is an inexact number, @scheme[#f] otherwise.

@examples[(inexact? 1) (inexact? 1.0)]}


@defproc[(inexact->exact [z number?]) exact?]{ Coerces @scheme[z] to an
 exact number. If @scheme[z] is already exact, it is returned. If @scheme[z]
 is @scheme[+inf.0], @scheme[-inf.0], or @scheme[+nan.0], then the
 @exnraise[exn:fail:contract].

@examples[(inexact->exact 1) (inexact->exact 1.0)]}

 
@defproc[(exact->inexact [z number?]) inexact?]{ Coerces @scheme[z] to an
 inexact number. If @scheme[z] is already inexact, it is returned.

@examples[(exact->inexact 1) (exact->inexact 1.0)]}


@defproc[(+ [z number?] ...0) number?]{ Returns the sum of the
 @scheme[z]s, adding pairwise from left to right. If no arguments are
 provided, the result is @scheme[0].

@examples[(+ 1 2) (+ 1.0 2+3i 5) (+)]}


@defproc*[([(- [z number?]) number?]
           [(- [z number?] [w number?] ...1) number?])]{
 When no @scheme[w]s are supplied, returns @scheme[(- 0 #, @scheme[z])].
 Otherwise, returns the subtraction of the @scheme[w]s from @scheme[z]
 working pairwise from left to right.}

@examples[(- 5 3.0) (- 1) (- 2+7i 1 3)]


@defproc[(* [z number?] ...0) number?]{ Returns the product of the
 @scheme[z]s, multiplying pairwise from left to right. If no arguments are
 provided, the result is @scheme[1].}

@examples[(* 2 3) (* 8.0 9) (* 1+2i 3+4i)]


@defproc*[([(/ [z number?]) number?]
           [(/ [z number?] [w number?] ...1) number?])] {
 When no @scheme[w]s are supplied, returns @scheme[(/ 1 #, @scheme[z])].
 Otherwise, returns the division @scheme[z] by the var[w]s
 working pairwise from left to right.}

@examples[(/ 3 4) (/ 81 3 3) (/ 10.0) (/ 1+2i 3+4i)]


@defproc[(quotient [n integer?] [m integer?]) number?]{ Returns 
 @scheme[(truncate (/ n m))].}

@examples[(quotient 10 3) (quotient -10.0 3) (quotient +inf.0 3)]


@defproc[(remainder [n integer?] [m integer?]) number?]{ Returns
 @scheme[q] with the same sign as @scheme[n] such that

@itemize{

 @item{@scheme[(abs q)] is between @scheme[0] (inclusive) and @scheme[(abs m)] (exclusive), and}

 @item{@scheme[(+ q (* m (quotient n m)))] equals @scheme[n].}

}

@examples[(remainder 10 3) (remainder -10.0 3) (remainder 10.0 -3) (remainder -10 -3) (remainder +inf.0 3)]}


@defproc[(modulo [n integer?] [m integer?]) number?]{  Returns
 @scheme[q] with the same sign as @scheme[m] where

@itemize{

 @item{@scheme[(abs q)] is between @scheme[0] (inclusive) and @scheme[(abs m)] (exclusive), and}

 @item{the difference between @scheme[q] and @scheme[(- n (* m (quotient n m)))] is a multiple of @scheme[m].}

}

@examples[(modulo 10 3) (modulo -10.0 3)  (modulo 10.0 -3) (modulo -10 -3) (modulo +inf.0 3)]}


@defproc[(add1 [z number?]) number?]{ Returns @scheme[(+ z 1)].}

@defproc[(sub1 [z number?]) number?]{ Returns @scheme[(- z 1)].}

@defproc[(abs [x real?]) number?]{ Returns the absolute value of
 @scheme[x].

@examples[(abs 1.0) (abs -1)]}


@defproc[(= [z number?] [w number?] ...1) boolean?]{ Returns
 @scheme[#t] if all of the arguments are numerically equal,
 @scheme[#f] otherwise.  An inexact number is numerically equal to an
 exact number when the exact coercion of the inexact number is the
 exact number. Also, @scheme[0.0] and @scheme[-0.0] are numerically
 equal, but @scheme[+nan.0] is not numerically equal to itself.

@examples[(= 1 1.0) (= 1 2) (= 2+3i 2+3i 2+3i)]}


@defproc[(< [x real?] [y real?] ...1) boolean?]{ Returns @scheme[#t] if
 the arguments in the given order are in strictly increasing,
 @scheme[#f] otherwise.

@examples[(< 1 1) (< 1 2 3) (< 1 +inf.0) (< 1 +nan.0)]}


@defproc[(<= [x real?] [y real?] ...1) boolean?]{ Returns @scheme[#t]
 if the arguments in the given order are in non-decreasing,
 @scheme[#f] otherwise.

@examples[(<= 1 1) (<= 1 2 1)]}


@defproc[(> [x real?] [y real?] ...1) boolean?]{ Returns @scheme[#t] if
 the arguments in the given order are in strictly decreasing,
 @scheme[#f] otherwise.

@examples[(> 1 1) (> 3 2 1) (> +inf.0 1) (< +nan.0 1)]}


@defproc[(>= [x real?] [y real?] ...1) boolean?]{ Returns @scheme[#t]
 if the arguments in the given order are in non-increasing,
 @scheme[#f] otherwise.

@examples[(>= 1 1) (>= 1 2 1)]}


@defproc[(zero? [z number?]) boolean?]{ Returns @scheme[(= 0 z)].

@examples[(zero? 0) (zero? -0.0)]}


@defproc[(positive? [x real?]) boolean?]{ Returns @scheme[(> x 0)].

@examples[(positive? 10) (positive? -10) (positive? 0.0)]}


@defproc[(negative? [x real?]) boolean?]{ Returns @scheme[(< x 0)].

@examples[(negative? 10) (negative? -10) (negative? -0.0)]}


@defproc[(max [x real?] ...1) boolean?]{ Returns the largest of the
 @scheme[x]s, or @scheme[+nan.0] if any @scheme[x] is @scheme[+nan.0].
 If any @scheme[x] is inexact, the result is coerced to inexact.

@examples[(max 1 3 2) (max 1 3 2.0)]}


@defproc[(min [x real?] ...1) boolean?]{ Returns the smallest of the
 @scheme[x]s, or @scheme[+nan.0] if any @scheme[x] is @scheme[+nan.0].
 If any @scheme[x] is inexact, the result is coerced to inexact.

@examples[(min 1 3 2) (min 1 3 2.0)]}


@defproc[(even? [n integer?]) boolean?]{ Returns @scheme[(zero? (modulo
 n 2))].

@examples[(even? 10.0) (even? 11) (even? +inf.0)]}


@defproc[(odd? [n integer?]) boolean?]{ Returns @scheme[(not (even? n))].

@examples[(odd? 10.0) (odd? 11) (odd? +inf.0)]}


@defproc[(gcd [n integer?] ...0) integer?]{ Returns the greatest common
 divisor of the @scheme[n]s. If no arguments are provided, the result is
 @scheme[0].

@examples[(gcd 10) (gcd 12 81.0)]}


@defproc[(lcm [n integer?] ...0) integer?]{ Returns the least common
 multiple of the @scheme[n]s. If no arguments are provided, the result is
 @scheme[1].

@examples[(lcm 10) (lcm 3 4.0)]}


@defproc[(round [x real?]) integer?]{ Returns the integer closest to
 @scheme[x], resolving ties in favor of an even number.

@examples[(round 17/4) (round -17/4) (round 2.5) (round -2.5)]}


@defproc[(floor [x real?]) integer?]{ Returns the largest integer is that
 is no more than @scheme[x].

@examples[(floor 17/4) (floor -17/4) (floor 2.5) (floor -2.5)]}


@defproc[(ceiling [x real?]) integer?]{ Returns the smallest integer is
 that is at least as large as @scheme[x].

@examples[(ceiling 17/4) (ceiling -17/4) (ceiling 2.5) (ceiling -2.5)]}


@defproc[(truncate [x real?]) integer?]{ Returns the integer farthest
 from @scheme[0] that is no closer to @scheme[0] than @scheme[x].

@examples[(truncate 17/4) (truncate -17/4) (truncate 2.5) (truncate -2.5)]}


@defproc[(numerator [x real?]) (or integer? (one-of/c +nan.0))]{
 Coreces @scheme[x] to an exact number, finds the numerator of the number
 expressed in its simplest fractional form, and returns this number
 coerced to the exactness of @scheme[x]. An exception is when @scheme[x] is
 @scheme[+inf.0], @scheme[-inf.0], and @scheme[+nan.0], in which case
 @scheme[x] is returned.

@examples[(numerator 5) (numerator 34/8) (numerator 2.3) (numerator +inf.0)]}


@defproc[(denominator [x real?]) (or/c integer? (one-of/c +nan.0))]{
 Coreces @scheme[x] to an exact number, finds the numerator of the number
 expressed in its simplest fractional form, and returns this number
 coerced to the exactness of @scheme[x]. Exceptions are when @scheme[x] is
 @scheme[+inf.0] or @scheme[-inf.0], in which case @scheme[1.0] is
 returned, or when @scheme[x] is @scheme[+nan.0], in which case
 @scheme[+nan.0] is returned.

@examples[(denominator 5) (denominator 34/8) (denominator 2.3) (denominator +inf.0)]}


@defproc[(exp [z number?]) number?]{ Returns Euler's number raised to the
 power of @scheme[z]. The result is normally inexact, but it is
 @scheme[1] when @scheme[z] is an exact @scheme[0].

@examples[(exp 1) (exp 2+3i) (exp 0)]}


@defproc[(log [z number?]) number?]{ Returns the natural logarithm of
 @scheme[z].  The result is normally inexact, but it is
 @scheme[0] when @scheme[z] is an exact @scheme[1].

@examples[(log (exp 1)) (log 2+3i) (log 1)]}


@defproc[(sin [z number?]) number?]{ Returns the sine of @scheme[z], where
 @scheme[z] is in radians.

@examples[(sin 3.14159) (sin 1+05.i)]}


@defproc[(cos [z number?]) number?]{ Returns the cosine of @scheme[z],
 where @scheme[z] is in radians.

@examples[(cos 3.14159) (cos 1+05.i)]}


@defproc[(tan [z number?]) number?]{ Returns the tangent of @scheme[z],
 where @scheme[z] is in radians.

@examples[(tan 0.7854) (tan 1+05.i)]}


@defproc[(asin [z number?]) number?]{ Returns the arcsin in radians of @scheme[z].

@examples[(asin 0.25) (asin 1+05.i)]}


@defproc[(acos [z number?]) number?]{ Returns the arccosine in radians
 of @scheme[z].

@examples[(acos 0.25) (acos 1+05.i)]}


@defproc*[([(atan [z number?]) number?]
           [(atan [y real?] [x real?]) number?])]{Returns the arctangent of
 @scheme[z] or of @scheme[(make-rectangular #, @scheme[x] #, @scheme[y])].}

@examples[(atan 0.5) (atan 2 1) (atan -2 -1) (atan 1+05.i)]


@defproc[(sqrt [z number?]) number?]{ Returns the principal square root
 of @scheme[z].The result is exact if @scheme[z] is exact and @scheme[z]'s
 square root is rational.

@examples[(sqrt 4/9) (sqrt 2) (sqrt -1)]}


@defproc[(integer-sqrt [n integer?]) integer?]{ Returns @scheme[(floor
 (sqrt n))] for positive @scheme[n]. For negative @scheme[n], the result is
 @scheme[(* (integer-sqrt (- n)) 0+i)].

@examples[(integer-sqrt 4.0) (integer-sqrt 5)]}


@defproc[(integer-sqrt/remainder [n integer?]) (values integer?
 integer?)]{  Returns @scheme[(integer-sqrt n)] and @scheme[(- n (expt
 (integer-sqrt n) 2))].

@examples[(integer-sqrt/remainder 4.0) (integer-sqrt/remainder 5)]}


@defproc[(expt [z number?] [w number?]) number?]{ Returns @scheme[z]
 raised to the power of @scheme[w]. If @scheme[w] is exact @scheme[0],
 the result is @scheme[1]. If @scheme[z] is exact @scheme[0] and
 @scheme[w] is negative, the @exnraise[exn:fail:contract].

@examples[(expt 2 3) (expt 4 0.5) (expt +inf.0 0)]}

@defproc[(make-rectangular [x real?] [y real?]) number?]{ Returns
 @scheme[(+ x (* y 0+1i))].

@examples[(make-rectangular 3 4.0)]}


@defproc[(make-polar [x real?] [y real?]) number?]{ Returns
 @scheme[(+ (* x (cos y)) (* x (sin y) 0+1i))].

@examples[(make-polar 2 3.14159)]}


@defproc[(real-part [z number?]) real?]{ Returns the real part of
 the complex number @scheme[z] in rectangle coordinates.

@examples[(real-part 3+4i) (real-part 5.0)]}


@defproc[(imag-part [z number?]) real?]{ Returns the imaginary part of
 the complex number @scheme[z] in rectangle coordinates.

@examples[(imag-part 3+4i) (imag-part 5.0) (imag-part 5.0+0.0i)]}


@defproc[(magnitude [z number?]) real?]{ Returns the magnitude of
 the complex number @scheme[z] in polar coordinates.

@examples[(magnitude -3) (magnitude 3.0) (magnitude 3+4i)]}


@defproc[(angle [z number?]) real?]{ Returns the angle of
 the complex number @scheme[z] in polar coordinates.

@examples[(angle -3) (angle 3.0) (angle 3+4i) (angle +inf.0+inf.0i)]}


@defproc[(bitwise-ior [n exact-integer?] ...0) exact-integer?]{ Returns
 the bitwise ``inclusive or'' of the @scheme[n]s in their (semi-infinite)
 two's complement representation. If no arguments are provided, the
 result is @scheme[0].

@examples[(bitwise-ior 1 2) (bitwise-ior -32 1)]}


@defproc[(bitwise-and [n exact-integer?] ...0) exact-integer?]{ Returns
 the bitwise ``and'' of the @scheme[n]s in their (semi-infinite) two's
 complement representation. If no arguments are provided, the result
 is @scheme[-1].

@examples[(bitwise-and 1 2) (bitwise-and -32 -1)]}


@defproc[(bitwise-xor [n exact-integer?] ...0) exact-integer?]{ Returns
 the bitwise ``exclusive or'' of the @scheme[n]s in their (semi-infinite)
 two's complement representation. If no arguments are provided, the
 result is @scheme[0].

@examples[(bitwise-xor 1 5) (bitwise-xor -32 -1)]}


@defproc[(bitwise-not [n exact-integer?])  exact-integer?]{ Returns the
 bitwise ``not'' of @scheme[n] in its (semi-infinite) two's complement
 representation.

@examples[(bitwise-not 5) (bitwise-not -1)]}


@defproc[(arithmetic-shift [n exact-integer?] [m exact-integer?])
 exact-integer?]{ Returns the bitwise ``shift'' of @scheme[n] in its
 (semi-infinite) two's complement representation.  If @scheme[m] is
 non-negative, the integer @scheme[n] is shifted left by @scheme[m] bits;
 i.e., @scheme[m] new zeros are introduced as rightmost digits. If
 @scheme[m] is negative, @scheme[n] is shifted right by @scheme[(- #,
 @scheme[m])] bits; i.e., the rightmost @scheme[m] digits are dropped.

@examples[(arithmetic-shift 1 10) (arithmetic-shift 255 -3)]}


@defproc[(integer-length [n exact-integer?]) exact-integer?]{ Returns
 the number of bits in the (semi-infinite) two's complement
 representation of @scheme[n] after removing all leading zeros (for
 non-negative @scheme[n]) or ones (for negative @scheme[n]).

@examples[(integer-length 8) (integer-length -8)]}


@defproc[(number->string [z number?] [radix (one-of/c 2 8 10
 16) 10]) string?]{ Returns a string that is the printed form of @scheme[z]
 in the base specific by @scheme[radix]. If @scheme[z] is inexact,
 @scheme[radix] must be @scheme[10], otherwise the
 @exnraise[exn:fail:contract].

@examples[(number->string 3.0) (number->string 255 8)]}


@defproc[(string->number [s string?] [radix (exact-integer-in/c 2 16)
 10]) (or/c number? false/c)]{ Reads and returns a number datum from
 @scheme[s] (see @secref["parse-number"]), returning @scheme[#f] if
 @scheme[s] does not parse exactly as a number datum (with no
 whitespace). The optional @scheme[radix] argument specifies the default
 base for the number, which can be overriden by @litchar{#b},
 @litchar{#o}, @litchar{#d}, or @litchar{#x} in the string.

@examples[(string->number "3.0+2.5i") (string->number "hello") 
          (string->number "111" 7)  (string->number "#b111" 7)]
}