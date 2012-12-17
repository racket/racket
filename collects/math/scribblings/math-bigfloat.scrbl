#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base
                     math
                     (only-in typed/racket/base
                              Flonum Real Boolean Integer Exact-Rational Exact-Positive-Integer
                              Any U Listof String False Values Path-String))
          "utils.rkt")

@(define untyped-eval (make-untyped-math-eval))
@interaction-eval[#:eval untyped-eval (require math/bigfloat)]

@title[#:tag "bigfloat"]{Arbitrary-Precision Floating-Point Numbers (@deftech{Bigfloats})}
@(author-neil)

@defmodule[math/bigfloat]

This library provides a Typed Racket interface to @hyperlink["http://www.mpfr.org/"]{MPFR},
a C library that provides
@itemlist[@item{A C type of arbitrary-precision floating-point numbers.}
          @item{Elementary and special functions that are efficient and proved correct.}
          @item{Well-defined semantics that correspond with the latest IEEE 754 standard.}
]
The arbitrary-precision floating-point numbers MPFR provides and operates on are represented by the
Typed Racket type @racket[Bigfloat] and identified by the predicate @racket[bigfloat?].

With a few noted exceptions, bigfloat functions regard their arguments as if they were exact,
regardless of their precision.
Conceptually, they compute exact results using infinitely many bits, and return results with
@racket[(bf-precision)] bits by rounding them using @racket[(bf-rounding-mode)].
In practice, they use finite algorithms that have been painstakingly proved to be
equivalent to that conceptual, infinite process.

MPFR is free and license-compatible with commercial software. It is distributed with Racket
for Windows and Mac OS X, is installed on most Linux systems, and is
@hyperlink["http://www.mpfr.org/ports.html"]{easy to install} on major Unix-like platforms.

@local-table-of-contents[]

@section[#:tag "quick"]{Quick Start}

@itemlist[#:style 'ordered
@item{Set the bigfloat function result precision using @racket[(bf-precision <some-number-of-bits>)].}
@item{Use @racket[bf] to convert real values and well-formed strings to bigfloats.}
@item{Operate on bigfloats using @racket[bf]-prefixed functions like @racket[bf+] and @racket[bfsin].}
@item{Convert bigfloats to real values using @racket[bigfloat->real], @racket[bigfloat->flonum],
      and @racket[bigfloat->integer]. Format them for display using @racket[bigfloat->string].}
]

For examples, continue through the FAQ.

@section{Fictionally Asked Questions}

@bold{Why use @racketmodname[math/bigfloat]}?

There are a few reasons.

@italic{Reason:} Flonums have either too much or too little precision for your application.

@examples[#:eval
          untyped-eval
          (flsqrt 3.0)
          pi
          (bf-precision 16)
          (eval:alts (bfsqrt (bf 3))
                     (eval:result @racketresultfont{(bf #e1.73206)}))
          (bf-precision 179)
          (eval:alts
           pi.bf
           (eval:result
            @racketresultfont{(bf #e3.141592653589793238462643383279502884197169399375105819)}))]

A flonum has a 53-bit significand (we'll say it has 53 bits of
@italic{precision}) and an 11-bit @italic{exponent}. A bigfloat
has an arbitrary precision of at least 2 bits and a 31-bit exponent.

@italic{Reason:} To compute ridiculously large or small numbers with confidence.

@examples[#:eval
          untyped-eval
          (bf-precision 128)
          (eval:alts
           (bfexp (bfexp (bfexp (bf 3))))
           (eval:result @racketresultfont{(bf "2.050986436051648895105860942072054674579e229520860")}))
          (eval:alts
           (bflog (bflog (bflog (bfexp (bfexp (bfexp (bf 3)))))))
           (eval:result @racketresultfont{(bf 3)}))]

@italic{Reason:} To verify your floating-point hardware.

@hyperlink["http://en.wikipedia.org/wiki/IEEE_754-2008"]{IEEE 754-2008} stipulates
that conforming implementations must correctly round the results of all operations.
Roughly speaking, results can't be more than half a bit off, where the bit in
question is the least significant in the significand.

Of course, implementations don't always adhere to standards. For example, on my old
laptop, evaluating @(racket (exp 400)) results in @(racket 5.221469689764346e+173).
Note the last four decimal digits in the significand: @(racket 4346). But they should be @racket[4144]:
@interaction[#:eval untyped-eval
                    (bf-precision 53)
                    (eval:alts
                     (bigfloat->flonum (bfexp (bf 400)))
                     (eval:result @racketresultfont{5.221469689764144e+173}))]
My new laptop computes @(racket 5.221469689764144e+173) as it should.

@italic{Reason:} To control rounding of the least significant bit.

IEEE 754 provides for different rounding modes for the smallest bit of
a flonum result, such as round to even and round toward zero. We might use
this to implement interval arithmetic correctly, by rounding lower bounds
downward and upper bounds upward. But there isn't a portable way to set the
rounding mode!

MPFR allows the rounding mode to be different for any operation, and
@racketmodname[math/bigfloat] exposes this capability using the parameter
@racket[bf-rounding-mode].

@bold{When shouldn't I use @racketmodname[math/bigfloat]?}

When you need raw speed. Bigfloat functions can be hundreds to thousands of times slower
than flonum functions.

That's not to say that they're @italic{inefficient}. For example, @(racket bflog)
implements the algorithm with the best known asymptotic complexity. It just doesn't
run directly on hardware, and it can't take fixed-precision-only shortcuts.

@bold{Why are there junk digits on the end of @racket[(bf 1.1)]?}

That's approximately the value of the flonum @racket[1.1]. Use @code{(bf #e1.1)} or
@code{(bf "1.1")} to make the junk go away. In general, you should prefer to convert
exact rationals and strings to bigfloats.

@bold{Why is the last digit of @racket[pi.bf] not rounded correctly?}

All the @italic{bits} but the last is exact, and the last bit is correctly rounded.
This doesn't guarantee that the last digit will be.

A decimal digit represents at most log(10)/log(2) ≈ 3.3 bits. This is an irrational
number, so the decimal/bit boundary never lines up except at the decimal point.
Thus, the last decimal digit of any bigfloat must represent fewer than 3.3 bits,
so it's wrong more often than not. But it's the last @italic{bit} that counts.

@section{Type and Constructors}

@deftogether[(@defidform[Bigfloat]
              @defproc[(bigfloat? [v Any]) Boolean])]{
An opaque type that represents an arbitrary-precision floating-point number, or a @tech{bigfloat},
and the opaque type's predicate.
}

@defproc*[([(bf [x (U String Real)]) Bigfloat]
           [(bf [sig Integer] [exp Integer]) Bigfloat])]{
The one-argument variant converts a string or real @racket[x] to a bigfloat.

@interaction[#:eval untyped-eval
                    (bf-precision 128)
                    (eval:alts (bf 4) (eval:result @racketresultfont{(bf 4)}))
                    (eval:alts
                     (bf 1/7)
                     (eval:result
                      @racketresultfont{(bf #e0.1428571428571428571428571428571428571426)}))
                    (eval:alts
                     (bf #e4.1)
                     (eval:result
                      @racketresultfont{(bf #e4.099999999999999999999999999999999999995)}))
                    (eval:alts
                     (bf "not a number")
                     (eval:result
                      ""
                      ""
                      "bf: expected a well-formed decimal number; given \"not a number\""))
                    (eval:alts
                     (bf "15e200000000")
                     (eval:result
                      @racketresultfont{(bf "1.499999999999999999999999999999999999998e200000001")}))]

In the last example, the result of @racket[(bf "15e200000000")] is displayed as a
string conversion because the exact rational number would be very large.

@margin-note{* It can be a good idea if you're testing a flonum implementation of a function
             against a bigfloat implementation.}
Converting from flonum literals is usually a bad idea* because flonums have only
53 bits precision. Prefer to pass exact rationals and strings to @racket[bf].

The two-argument variant converts a signed significand @racket[sig] and a power of
2 @racket[exp] to a bigfloat. Generally, @racket[(bf sig exp) = (bf (* sig (expt 2 exp)))],
but the two-argument variant is much faster, especially for large @racket[exp].
@examples[#:eval untyped-eval
                 (eval:alts
                  (bf 200 56)
                  (eval:result @racketresultfont{(bf 14411518807585587200)}))
                 (eval:alts
                  (bf (* 200 (expt 2 56)))
                  (eval:result @racketresultfont{(bf 14411518807585587200)}))]

The @racket[bfrandom] function generates random bigfloats between 0 and 1 using
the two-argument variant in this way:
@interaction[#:eval untyped-eval
                    (require (only-in math/base random-bits))
                    (bf-precision 64)
                    (eval:alts
                     (bf (random-bits 64) -64)
                     (eval:result @racketresultfont{(bf #e0.416872969910248753552)}))]
}

@defproc[(bfrandom) Bigfloat]{
Returns a uniformly distributed random bigfloat in the interval [0,1].
}

@defproc[(bfcopy [x Bigfloat]) Bigfloat]{
Returns the @racket[(bf-precision)]-bit bigfloat closest to @racket[x],
under the current @racket[bf-rounding-mode].

A common pattern to compute bigfloats in higher precision is
@interaction[#:eval untyped-eval
                    (bf-precision 64)
                    (eval:alts
                     (bfcopy
                      (parameterize ([bf-precision  (+ (bf-precision) 10)])
                        (bf/ (bf+ 1.bf (bfsqrt 5.bf)) 2.bf)))
                     (eval:result @racketresultfont{(bf #e1.61803398874989484821)}))]
This example computes the golden ratio (@racket[phi.bf]) with 10 bits more than requested,
to make up for triple rounding error.
}

@section{Accessors and Conversion Functions}

@defproc[(bigfloat-precision [x Bigfloat]) Exact-Positive-Integer]{
Returns the number of bits in the significand of @racket[x]. This is almost always
the value of @racket[(bf-precision)] when @racket[x] was created.
}

@defproc[(bigfloat-signbit [x Bigfloat]) (U 0 1)]{
Returns the sign bit of the significand of @racket[x].
@examples[#:eval untyped-eval
                 (eval:alts
                  (bigfloat-signbit -1.bf)
                  (eval:result @racketresultfont{1}))
                 (eval:alts
                  (bigfloat-signbit 0.bf)
                  (eval:result @racketresultfont{0}))
                 (eval:alts
                  (bigfloat-signbit -0.bf)
                  (eval:result @racketresultfont{1}))
                 (eval:alts
                  (bigfloat-signbit -inf.bf)
                  (eval:result @racketresultfont{1}))]
}

@deftogether[(@defproc[(bigfloat-significand [x Bigfloat]) Integer]
              @defproc[(bigfloat-exponent [x Bigfloat]) Integer])]{
Return the @italic{signed} significand or exponent of @racket[x].

To access the significand and exponent at the same time, use @racket[bigfloat->sig+exp].
}

@defproc[(bigfloat->sig+exp [x Bigfloat]) (Values Integer Integer)]{
Returns the @italic{signed} significand and exponent of @racket[x].

If @racket[(values sig exp) = (bigfloat->sig+exp x)], its value as an exact rational
is @racket[(* sig (expt 2 exp))]. In fact, @racket[bigfloat->rational] converts
bigfloats to rationals in exactly this way, after ensuring that @racket[(bfrational? x)]
is @racket[#t].

This function and the two-argument variant of @racket[bf] are mutual inverses.
}

@deftogether[(@defproc[(bigfloat->integer [x Bigfloat]) Integer]
              @defproc[(bigfloat->rational [x Bigfloat]) Exact-Rational]
              @defproc[(bigfloat->real [x Bigfloat]) (U Exact-Rational Flonum)]
              @defproc[(bigfloat->flonum [x Bigfloat]) Flonum])]{
Convert bigfloats to integer, exact rational, real and flonum values respectively.

@racket[bigfloat->integer], @racket[bigfloat->rational] and @racket[bigfloat->real] return values
that can be converted exactly back to @racket[x] using @racket[bf]. For the first two, this is done
by raising an error if @racket[x] is not respectively integer or rational. On the other hand,
@racket[bigfloat->real] returns @racket[+inf.0], @racket[-inf.0] or @racket[+nan.0] when @racket[x]
is not a rational bigfloat.

@racket[bigfloat->flonum] rounds @racket[x] to 53 bits precision to fit the value into a flonum,
using the current value of @racket[bf-rounding-mode].

@interaction[#:eval untyped-eval
                    (bf-precision 64)
                    (eval:alts
                     (bigfloat->integer (bf 21/10))
                     (eval:result
                      ""
                      ""
                      "bigfloat->integer: contract violation
  expected: bfinteger?
  given: (bf #e2.09999999999999999991)"))
                    (eval:alts
                     (bigfloat->integer (bfround (bf 21/10)))
                     (eval:result @racketresultfont{2}))
                    (eval:alts (define x (bf 1/7)) (eval:result ""))
                    (eval:alts
                     (bigfloat->flonum x)
                     (eval:result @racketresultfont{0.14285714285714285}))
                    (eval:alts
                     (bigfloat->rational x)
                     (eval:result @racketresultfont{10540996613548315209/73786976294838206464}))
                    (eval:alts
                     (rationalize (bigfloat->rational x) (expt 2 (- (bf-precision))))
                     (eval:result @racketresultfont{1/7}))
                    (eval:alts
                     (bf= x (bf (bigfloat->rational x)))
                     (eval:result @racketresultfont{#t}))]

@bold{Be careful with exact conversions.} Bigfloats with large exponents may not fit in memory as
integers or exact rationals. Worse, they might fit, but have all your RAM and swap space for lunch.
}

@deftogether[(@defproc[(bigfloat->string [x Bigfloat]) String]
              @defproc[(string->bigfloat [s String]) (U Bigfloat False)])]{
Convert a bigfloat @racket[x] to a string @racket[s] and back.

The string returned by @racket[bigfloat->string] includes enough digits that
@racket[string->bigfloat] can reconstruct the bigfloat precisely. In other words,
@racket[string->bigfloat] is a left inverse of @racket[bigfloat->string].

If @racket[s] isn't a well-formed decimal number with an optional exponent part,
@racket[string->bigfloat] returns @racket[#f]. (In contrast, @racket[(bf s)] raises an error.)

@examples[#:eval untyped-eval
                 (bf-precision 64)
                 (eval:alts
                  (bigfloat->string (bf 4))
                  (eval:result @racketresultfont{"4"}))
                 (eval:alts
                  (bigfloat->string (bf #e0.0001))
                  (eval:result @racketresultfont{"1.00000000000000000001e-4"}))
                 (eval:alts
                  (string->bigfloat "0.14285714285714285714")
                  (eval:result @racketresultfont{(bf #e0.142857142857142857141)}))
                 (eval:alts
                  (string->bigfloat "square root of two")
                  (eval:result @racketresultfont{#f}))
                 (eval:alts
                  (string->bigfloat (bigfloat->string pi.bf))
                  (eval:result @racketresultfont{(bf #e3.14159265358979323851)}))
                 (eval:alts
                  pi.bf
                  (eval:result @racketresultfont{(bf #e3.14159265358979323851)}))]
}

@section{Parameters}

@defparam[bf-precision bits Integer]{
A parameter that determines the precision of bigfloats returned from most bigfloat functions.
Exceptions are noted in the documentation for functions that do not use @racket[bf-precision].

For nonzero, rational bigfloats, the number of bits @racket[bits] includes the leading one bit.
For example, to simulate 64-bit floating point, use @racket[(bf-precision 53)] even though
flonums have a 52-bit significand, because the one bit is implicit in a flonum.

This parameter has a guard that ensures @racket[(bf-precision)] is between
@racket[bf-min-precision] and @racket[bf-max-precision].
}

@defparam[bf-rounding-mode mode (U 'nearest 'zero 'up 'down)]{
A parameter that determines the mode used to round the results of most bigfloat functions.
Conceptually, rounding is applied to infinite-precision results to fit them into
@racket[(bf-precision)] bits.
}

@defthing[bf-min-precision Exact-Positive-Integer]{
Equal to @racket[2], because single-bit bigfloats can't be correctly rounded.
}

@defthing[bf-max-precision Exact-Positive-Integer]{
The largest value of @racket[(bf-precision)]. This is platform-dependent, and probably much
larger than you'll ever need.
}

@section[#:tag "constants"]{Constants}

Most bigfloat ``constants'' are actually identifier macros that expand to the application
of a zero-argument function. This allows, for example, @racket[pi.bf] to depend on the
current value of @racket[bf-precision], and allows all of them to be constructed lazily.
Most constants are memoized, possibly at multiple precisions.

@deftogether[(@defthing[pi.bf Bigfloat]
              @defthing[phi.bf Bigfloat]
              @defthing[gamma.bf Bigfloat]
              @defthing[catalan.bf Bigfloat]
              @defthing[log2.bf Bigfloat])]{
Approximations of π, φ, γ, G and log(2).
@examples[#:eval
          untyped-eval
          (bf-precision 10)
          (eval:alts pi.bf (eval:result @racketresultfont{(bf #e3.1406)}))
          (bf-precision 179)
          (eval:alts
           pi.bf
           (eval:result
            @racketresultfont{(bf #e3.141592653589793238462643383279502884197169399375105819)}))
          (eval:alts
           phi.bf
           (eval:result
            @racketresultfont{bf #e1.618033988749894848204586834365638117720309179805762863)}))
          (eval:alts
           gamma.bf
           (eval:result
            @racketresultfont{(bf #e0.5772156649015328606065120900824024310421593359399235988)}))
          (eval:alts
           catalan.bf
           (eval:result
            @racketresultfont{(bf #e0.9159655941772190150546035149323841107741493742816721343)}))
          (eval:alts
           log2.bf
           (eval:result
            @racketresultfont{(bf #e0.6931471805599453094172321214581765680755001343602552545)}))]
}

@deftogether[(@defthing[-inf.bf Bigfloat]
              @defthing[-max.bf Bigfloat]
              @defthing[-min.bf Bigfloat]
              @defthing[-0.bf Bigfloat]
              @defthing[0.bf Bigfloat]
              @defthing[+min.bf Bigfloat]
              @defthing[+max.bf Bigfloat]
              @defthing[+inf.bf Bigfloat]
              @defthing[+nan.bf Bigfloat]
              @defthing[epsilon.bf Bigfloat])]{
Bigfloat constants corresponding to
@racket[-inf.0], @racket[-max.0] @racket[-min.0], @racket[-0.0],
@racket[0.0], @racket[+min.0], @racket[+max.0], @racket[+inf.0],
@racket[+nan.0] and @racket[epsilon.0].

The constants @racket[-inf.bf], @racket[-0.bf], @racket[0.bf], @racket[+inf.bf],
and @racket[+nan.bf] have fixed precision.
}

@deftogether[(@defthing[-10.bf Bigfloat]
              @defthing[-9.bf Bigfloat]
              @defthing[-8.bf Bigfloat]
              @defthing[-7.bf Bigfloat]
              @defthing[-6.bf Bigfloat]
              @defthing[-5.bf Bigfloat]
              @defthing[-4.bf Bigfloat]
              @defthing[-3.bf Bigfloat]
              @defthing[-2.bf Bigfloat]
              @defthing[-1.bf Bigfloat]
              @defthing[1.bf Bigfloat]
              @defthing[2.bf Bigfloat]
              @defthing[3.bf Bigfloat]
              @defthing[4.bf Bigfloat]
              @defthing[5.bf Bigfloat]
              @defthing[6.bf Bigfloat]
              @defthing[7.bf Bigfloat]
              @defthing[8.bf Bigfloat]
              @defthing[9.bf Bigfloat]
              @defthing[10.bf Bigfloat])]{
More fixed-precision bigfloat constants.
}

@section[#:tag "predicates"]{Predicates}

@deftogether[(@defproc[(bfzero? [x Bigfloat]) Boolean]
              @defproc[(bfpositive? [x Bigfloat]) Boolean]
              @defproc[(bfnegative? [x Bigfloat]) Boolean]
              @defproc[(bfinteger? [x Bigfloat]) Boolean]
              @defproc[(bfeven? [x Bigfloat]) Boolean]
              @defproc[(bfodd? [x Bigfloat]) Boolean]
              @defproc[(bfrational? [x Bigfloat]) Boolean]
              @defproc[(bfinfinite? [x Bigfloat]) Boolean]
              @defproc[(bfnan? [x Bigfloat]) Boolean])]{
Unary predicates corresponding to @racket[zero?], @racket[positive?],
@racket[negative?], @racket[integer?], @racket[even?], @racket[odd?],
@racket[rational?], @racket[infinite?] and @racket[nan?].
}

@deftogether[(@defproc[(bf= [x Bigfloat] [y Bigfloat]) Boolean]
              @defproc[(bf> [x Bigfloat] [y Bigfloat]) Boolean]
              @defproc[(bf< [x Bigfloat] [y Bigfloat]) Boolean]
              @defproc[(bf>= [x Bigfloat] [y Bigfloat]) Boolean]
              @defproc[(bf<= [x Bigfloat] [y Bigfloat]) Boolean])]{
Standard comparison functions. As is usual, infinities are either greater or less
than any other bigfloat, and every comparison returns @racket[#f] when either argument
is @racket[+nan.bf].
}

@section[#:tag "rounding"]{Rounding}

@deftogether[(@defproc[(bftruncate [x Bigfloat]) Bigfloat]
              @defproc[(bffloor [x Bigfloat]) Bigfloat]
              @defproc[(bfceiling [x Bigfloat]) Bigfloat]
              @defproc[(bfround [x Bigfloat]) Bigfloat])]{
Like @racket[truncate], @racket[floor], @racket[ceiling] and @racket[round], but
for bigfloats.

Rounding is to the nearest integer, with ties broken by rounding to even.
@examples[#:eval untyped-eval
                 (eval:alts (bfround (bf 1.5)) (eval:result @racketresultfont{(bf 2)}))
                 (eval:alts (bfround (bf 2.5)) (eval:result @racketresultfont{(bf 2)}))
                 (eval:alts (bfround (bf -1.5)) (eval:result @racketresultfont{(bf -2)}))
                 (eval:alts (bfround (bf -2.5)) (eval:result @racketresultfont{(bf -2)}))]
}

@defproc[(bffrac [x Bigfloat]) Bigfloat]{
Returns the fractional part of @racket[x], with the same sign as @racket[x].
}

@defproc[(bfrint [x Bigfloat]) Bigfloat]{
Rounds @racket[x] to the nearest integer bigfloat, in the direction specified by
@racket[(bf-rounding-mode)].
}

@section[#:tag "ops"]{Mathematical Operations}

@deftogether[(@defproc[(bfmax [x Bigfloat] ...) Bigfloat]
              @defproc[(bfmin [x Bigfloat] ...) Bigfloat])]{
Return the maximum and minimum of their arguments, respectively.

When given no arguments, @racket[bfmin] returns @racket[+inf.bf], and
@racket[bfmax] returns @racket[-inf.bf].
}

@deftogether[(@defproc[(bf+ [x Bigfloat] ...) Bigfloat]
              @defproc[(bf* [x Bigfloat] ...) Bigfloat]
              @defproc[(bf- [x Bigfloat] [y Bigfloat] ...) Bigfloat]
              @defproc[(bf/ [x Bigfloat] [y Bigfloat] ...) Bigfloat]
              @defproc[(bfsqr [x Bigfloat]) Bigfloat]
              @defproc[(bfabs [x Bigfloat]) Bigfloat]
              @defproc[(bfsgn [x Bigfloat]) Bigfloat])]{
Standard arithmetic functions, corresponding to @racket[+], @racket[*],
@racket[-], @racket[/], @racket[sqr], @racket[abs] and @racket[sgn].

When @racket[bf+] and @racket[bf-] are given more than two arguments, they compute
the answers in a way that incurs rounding error only once.
}

@deftogether[(@defproc[(bfsqrt [x Bigfloat]) Bigfloat]
              @defproc[(bf1/sqrt [x Bigfloat]) Bigfloat]
              @defproc[(bfcbrt [x Bigfloat]) Bigfloat])]{
Return the square root, @italic{reciprocal} square root, and cube root of @racket[x].
}

@defproc[(bfroot [x Bigfloat] [n Integer]) Bigfloat]{
Returns the @racket[n]th root of @racket[x]. @racket[n] must be a nonnegative fixnum.
}

@defproc[(bfhypot [x Bigfloat] [y Bigfloat]) Bigfloat]{
Computes @racket[(bfsqrt (bf+ (bfsqr x) (bfsqr y)))] without uncessary overflow,
incurring rounding error only once. See @racket[flhypot] for an example using flonums.
}

@deftogether[(@defproc[(bflog [x Bigfloat]) Bigfloat]
              @defproc[(bflog2 [x Bigfloat]) Bigfloat]
              @defproc[(bflog10 [x Bigfloat]) Bigfloat])]{
Return the log of @racket[x] in base @italic{e}, 2 and 10.
}

@deftogether[(@defproc[(bfexp [x Bigfloat]) Bigfloat]
              @defproc[(bfexp2 [x Bigfloat]) Bigfloat]
              @defproc[(bfexp10 [x Bigfloat]) Bigfloat])]{
Return the exponential of @racket[x] in base @italic{e}, 2 and 10.
}

@deftogether[(@defproc[(bflog1p [x Bigfloat]) Bigfloat]
              @defproc[(bfexpm1 [x Bigfloat]) Bigfloat])]{
Like @racket[(bflog (bf+ 1.bf x))] and @racket[(bf- (bfexp x) 1.bf)], but correct
when @racket[x] is near zero. See @racket[fllog1p] for motivation and examples.
}

@defproc[(bfexpt [x Bigfloat] [y Bigfloat]) Bigfloat]{
Computes @racket[x]@superscript{@racket[y]}. See @racket[flexpt] and @racket[expt].
}

@deftogether[(@defproc[(bfsin [x Bigfloat]) Bigfloat]
              @defproc[(bfcos [x Bigfloat]) Bigfloat]
              @defproc[(bftan [x Bigfloat]) Bigfloat]
              @defproc[(bfasin [x Bigfloat]) Bigfloat]
              @defproc[(bfacos [x Bigfloat]) Bigfloat]
              @defproc[(bfatan [x Bigfloat]) Bigfloat]
              @defproc[(bfatan2 [x Bigfloat] [y Bigfloat]) Bigfloat])]{
Standard trigonometric functions and their inverses.
}

@deftogether[(@defproc[(bfsinh [x Bigfloat]) Bigfloat]
              @defproc[(bfcosh [x Bigfloat]) Bigfloat]
              @defproc[(bftanh [x Bigfloat]) Bigfloat]
              @defproc[(bfasinh [x Bigfloat]) Bigfloat]
              @defproc[(bfacosh [x Bigfloat]) Bigfloat]
              @defproc[(bfatanh [x Bigfloat]) Bigfloat])]{
Standard hyperbolic functions and their inverses.
}

@deftogether[(@defproc[(bfsec [x Bigfloat]) Bigfloat]
              @defproc[(bfcsc [x Bigfloat]) Bigfloat]
              @defproc[(bfcot [x Bigfloat]) Bigfloat])]{
Standard @italic{reciprocal} trigonometric functions. MPFR does not implement their inverses.
}

@deftogether[(@defproc[(bfsech [x Bigfloat]) Bigfloat]
              @defproc[(bfcsch [x Bigfloat]) Bigfloat]
              @defproc[(bfcoth [x Bigfloat]) Bigfloat])]{
Standard @italic{reciprocal} hyperbolic functions. MPFR does not implement their inverses.
}

@defproc[(bfsin+cos [x Bigfloat]) (Values Bigfloat Bigfloat)]{
Simultaneously computes the sine and cosine of @racket[x].
}

@defproc[(bfsinh+cosh [x Bigfloat]) (Values Bigfloat Bigfloat)]{
Simultaneously computes the hyperbolic sine and cosine of @racket[x].
}

@defproc[(bffactorial [x Integer]) Bigfloat]{
Returns the factorial of @racket[x].
}

@defproc[(bfgamma [x Bigfloat]) Bigfloat]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Gamma_function"]{gamma function},
a generalization of the factorial function.
}

@deftogether[(@defproc[(bflog-gamma [x Bigfloat]) Bigfloat]
              @defproc[(bflog-gamma/sign [x Bigfloat]) (Values Bigfloat (U -1 1))])]{
Computes the @hyperlink["http://mathworld.wolfram.com/LogGammaFunction.html"]{log-gamma function},
or the log of the absolute value of the gamma function. @racket[bflog-gamma/sign] additionally
returns the sign of @racket[(bfgamma x)].
}

@defproc[(bfpsi0 [x Bigfloat]) Bigfloat]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Digamma_function"]{digamma function},
the logarithmic derivative of the gamma function.
}

@defproc[(bfeint [x Bigfloat]) Bigfloat]{
Returns the @hyperlink["http://en.wikipedia.org/wiki/Exponential_integral"]{exponential integral}
of @racket[x].
}

@defproc[(bfli2 [x Bigfloat]) Bigfloat]{
Returns the dilogarithm of @racket[x], or the
@hyperlink["http://en.wikipedia.org/wiki/Polylogarithm"]{polylogarithm} of order 2.
}

@defproc[(bfzeta [x Bigfloat]) Bigfloat]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Riemann_zeta_function"]{Riemann zeta function}.
}

@deftogether[(@defproc[(bferf [x Bigfloat]) Bigfloat]
              @defproc[(bferfc [x Bigfloat]) Bigfloat])]{
Compute the @hyperlink["http://en.wikipedia.org/wiki/Error_function"]{error function and
complementary error function}, respectively.
}

@deftogether[(@defproc[(bfbesj0 [x Bigfloat]) Bigfloat]
              @defproc[(bfbesj1 [x Bigfloat]) Bigfloat]
              @defproc[(bfbesj [n Integer] [x Bigfloat]) Bigfloat]
              @defproc[(bfbesy0 [x Bigfloat]) Bigfloat]
              @defproc[(bfbesy1 [x Bigfloat]) Bigfloat]
              @defproc[(bfbesy [n Integer] [x Bigfloat]) Bigfloat])]{
These compute @hyperlink["http://en.wikipedia.org/wiki/Bessel_function"]{Bessel functions}.

A ``@racket[j]'' in the name indicates that a function computes a Bessel function of the
first kind. A ``@racket[y]'' indicates the second kind.

The ``@racket[j]'' or ``@racket[y]'' is followed by the @italic{order}: zero, one, or
@racket[n] (user-specified).
}

@defproc[(bfagm [x Bigfloat] [y Bigfloat]) bigfloat]{
Returns the
@hyperlink["http://en.wikipedia.org/wiki/Arithmetic-geometric_mean"]{arithmetic-geometric mean}
of @racket[x] and @racket[y]. Typically, this isn't directly useful, but it's used in some
asymptotically fast algorithms such as the one that computes @racket[bflog].
}


@section[#:tag "misc"]{Low-level Functions}

@deftogether[(@defproc[(bigfloat->ordinal [x Bigfloat]) Integer]
              @defproc[(ordinal->bigfloat [n Integer]) Bigfloat]
              @defproc[(bigfloats-between [x Bigfloat] [y Bigfloat]) Integer]
              @defproc[(bfstep [x Bigfloat] [n Integer]) Bigfloat]
              @defproc[(bfnext [x Bigfloat]) Bigfloat]
              @defproc[(bfprev [x Bigfloat]) Bigfloat])]{
Like @racket[flonum->ordinal], @racket[ordinal->flonum], @racket[flonums-between],
@racket[flstep], @racket[flnext] and @racket[flprev], but for bigfloats.

The major difference is that these operate using @racket[(bf-precision)] bits.
Additionally, unlike other bigfloat functions, all of these convert their bigfloat arguments
to @racket[(bf-precision)] bits.
}

@defproc[(bfshift [x Bigfloat] [n Integer]) Bigfloat]{
Like @racket[arithmetic-shift], but for bigfloats. More precisely, this returns
@racket[(bf* x (bfexpt (bf 2) (bf n)))], but is much faster.
}

@defproc[(bfcanonicalize [x Bigfloat]) Bigfloat]{
@margin-note*{Bigfloats are canonicalized before hashing, to ensure that equality
              implies an equal hash.}
If @racket[x] is nonzero and rational, returns a new bigfloat with no more bits of
precision than are necessary to encode @racket[x] exactly, by removing all low-order
zeros from the significand and adjusting the exponent.

For zero or non-rational @racket[x], returns @racket[-inf.bf], @racket[-0.bf],
@racket[0.bf], @racket[+inf.bf], or @racket[+nan.bf], depending on the value of @racket[x].

Two nonzero, rational bigfloats are @racket[equal?] if and only if their canonicalized
significands and exponents are equal. Two zero or non-rational bigfloats are @racket[equal?]
if and only if their canonicalizations are @racket[eq?].

Canonicalizing bigfloats won't change answers computed from them.

@examples[#:eval untyped-eval
                 (bf-precision 64)
                 (eval:alts (define x (bf 1 -2)) (eval:result ""))
                 (eval:alts x (eval:result @racketresultfont{(bf #e0.25)}))
                 (eval:alts (bfcanonicalize x)
                            (eval:result @racketresultfont{(bf #e0.25)}))
                 (eval:alts (bigfloat-precision x)
                            (eval:result @racketresultfont{64}))
                 (eval:alts (bigfloat-precision (bfcanonicalize x))
                            (eval:result @racketresultfont{2}))]
}

@(close-eval untyped-eval)
