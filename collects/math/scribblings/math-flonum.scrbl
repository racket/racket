#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/vector racket/list
                     math plot
                     (only-in typed/racket/base
                              ->
                              Flonum Integer Index Real Boolean Any Listof Vectorof FlVector))
          "utils.rkt")

@(define untyped-eval (make-untyped-math-eval))
@interaction-eval[#:eval untyped-eval (require racket/list)]

@title[#:tag "flonum"]{Flonums}
@(author-neil)

@defmodule[math/flonum]

For convenience, @racketmodname[math/flonum] re-exports @racketmodname[racket/flonum]
as well as providing the functions document below.

@local-table-of-contents[]

@section{Additional Flonum Functions}

@defproc[(fl [x Real]) Flonum]{
Equivalent to @racket[(real->double-flonum x)], but much easier to read and write.
@examples[#:eval untyped-eval
                 (fl 1/2)
                 (fl 0.5)
                 (fl 0.5f0)]
Note that @racket[exact->inexact] does not always convert a @racket[Real] to a @racket[Flonum]:
@interaction[#:eval untyped-eval
                    (exact->inexact 0.5f0)
                    (flabs (exact->inexact 0.5f0))]
You should prefer @racket[fl] over @racket[exact->inexact], especially in Typed Racket code.
}

@deftogether[(@defproc[(flsgn [x Flonum]) Flonum]
              @defproc[(fleven? [x Flonum]) Boolean]
              @defproc[(flodd? [x Flonum]) Boolean])]{
Like @racket[sgn], @racket[even?] and @racket[odd?], but restricted to flonum input.
@examples[#:eval untyped-eval
                 (map flsgn '(-2.0 -0.0 0.0 2.0))
                 (map fleven? '(2.0 1.0 0.5))
                 (map flodd? '(2.0 1.0 0.5))]
}

@defproc[(flhypot [x Flonum] [y Flonum]) Flonum]{
Computes @racket[(flsqrt (+ (* x x) (* y y)))] in way that overflows only when the
answer is too large.
@examples[#:eval untyped-eval
                 (flsqrt (+ (* 1e200 1e200) (* 1e199 1e199)))
                 (flhypot 1e200 1e199)]
}

@defproc[(flsum [xs (Listof Flonum)]) Flonum]{
Like @racket[(apply + xs)], but incurs rounding error only once.
@examples[#:eval untyped-eval
                 (+ 1.0 1e-16)
                 (+ (+ 1.0 1e-16) 1e-16)
                 (flsum '(1.0 1e-16 1e-16))]
The @racket[sum] function does the same for heterogenous lists of reals.

Worst-case time complexity is O(@italic{n}@superscript{2}), though the pathological
inputs needed to observe quadratic time are exponentially improbable and are hard
to generate purposely. Expected time complexity is O(@italic{n} log(@italic{n})).

See @racket[flvector-sums] for a variant that computes all the partial sums in @racket[xs].
}

@deftogether[(@defproc[(flsinh [x Flonum]) Flonum]
              @defproc[(flcosh [x Flonum]) Flonum]
              @defproc[(fltanh [x Flonum]) Flonum])]{
Return the @hyperlink["http://en.wikipedia.org/wiki/Hyperbolic_function"]{hyperbolic sine, cosine and tangent}
of @racket[x], respectively.

@examples[#:eval untyped-eval
                 (plot (list
                        (function (compose flsinh fl) #:label "flsinh x")
                        (function (compose flcosh fl) #:label "flcosh x" #:color 2)
                        (function (compose fltanh fl) #:label "fltanh x" #:color 3))
                       #:x-min -2 #:x-max 2 #:y-label #f #:legend-anchor 'bottom-right)]

Maximum observed error is 2 @tech{ulps}, making these functions (currently) much more accurate than their
@racketmodname[racket/math] counterparts. They also return sensible values on the largest possible domain.
}

@deftogether[(@defproc[(flasinh [y Flonum]) Flonum]
              @defproc[(flacosh [y Flonum]) Flonum]
              @defproc[(flatanh [y Flonum]) Flonum])]{
Return the @hyperlink["http://en.wikipedia.org/wiki/Inverse_hyperbolic_function"]{inverse hyperbolic sine, cosine and tangent}
of @racket[y], respectively.

These functions are as robust and accurate as their corresponding inverses.
}

@deftogether[(@defproc[(flfactorial [n Flonum]) Flonum]
              @defproc[(flbinomial [n Flonum] [k Flonum]) Flonum]
              @defproc[(flpermutations [n Flonum] [k Flonum]) Flonum]
              @defproc[(flmultinomial [n Flonum] [ks (Listof Flonum)]) Flonum])]{
Like @racket[(fl (factorial (fl->exact-integer n)))] and so on, but computed in constant
time. Also, these return @racket[+nan.0] instead of raising exceptions.

For factorial-like functions that return sensible values for non-integers, see
@racket[gamma] and @racket[beta].
}

@deftogether[(@defproc[(fllog-factorial [n Flonum]) Flonum]
              @defproc[(fllog-binomial [n Flonum] [k Flonum]) Flonum]
              @defproc[(fllog-permutations [n Flonum] [k Flonum]) Flonum]
              @defproc[(fllog-multinomial [n Flonum] [ks (Listof Flonum)]) Flonum])]{
Like @racket[(fllog (flfactorial n))] and so on, but more accurate and without unnecessary overflow.

For log-factorial-like functions that return sensible values for non-integers, see
@racket[log-gamma] and @racket[log-beta].
}

@deftogether[(@defproc[(fllog1p [x Flonum]) Flonum]
              @defproc[(flexpm1 [x Flonum]) Flonum])]{
Like @racket[(fllog (+ 1.0 x))] and @racket[(- (flexp x) 1.0)], but accurate when
@racket[x] is small (within 1 @tech{ulp}).

For example, one difficult input for @racket[(fllog (+ 1.0 x))] and @racket[(- (flexp x) 1.0)]
is @racket[x] = @racket[1e-14], which @racket[fllog1p] and @racket[flexpm1] compute correctly:
@interaction[#:eval untyped-eval
                    (fllog (+ 1.0 1e-14))
                    (fllog1p 1e-14)
                    (- (flexp 1e-14) 1.0)
                    (flexpm1 1e-14)]

These functions are mutual inverses:
@interaction[#:eval untyped-eval
                    (plot (list
                           (function (λ (x) x) #:color 0 #:style 'long-dash)
                           (function (compose fllog1p fl) #:label "fllog1p x")
                           (function (compose flexpm1 fl) #:label "flexpm1 x" #:color 2))
                          #:x-min -4 #:x-max 4 #:y-min -4 #:y-max 4)]
Notice that both graphs pass through the origin. Thus, inputs close to @racket[0.0],
around which flonums are particularly dense, result in outputs that are also close
to @racket[0.0]. Further, both functions are approximately the identity function
near @racket[0.0], so the output density is approximately the same.

Many flonum functions defined in terms of @racket[fllog] and @racket[flexp]
become much more accurate when their defining expressions are put in terms of
@racket[fllog1p] and @racket[flexpm1]. The functions exported by this module and
by @racketmodname[math/special-functions] use them extensively.

One notorious culprit is @racket[(flexpt (- 1.0 x) y)], when @racket[x] is near
@racket[0.0]. Computing it directly too often results in the wrong answer:
@interaction[#:eval untyped-eval (flexpt (- 1.0 1e-20) 1e20)]
We should expect that multiplying a number just less than @racket[1.0] by itself
that many times would result in something less than @racket[1.0]. The problem
comes from subtracting such a small number from @racket[1.0] in the first place:
@interaction[#:eval untyped-eval (- 1.0 1e-20)]
Fortunately, we can compute this correctly by putting the expression in terms
of @racket[fllog1p], which avoids the error-prone subtraction:
@interaction[#:eval untyped-eval (flexp (* 1e20 (fllog1p (- 1e-20))))]
But see @racket[flexpt1p], which is more accurate still.
}

@defproc[(flexpt1p [x Flonum] [y Flonum]) Flonum]{
Like @racket[(flexpt (+ 1.0 x) y)], but accurate for any @racket[x] and @racket[y].
}

@defproc[(make-flexpt [x Real]) (Flonum -> Flonum)]{
Equivalent to @racket[(λ (y) (flexpt x y))] when @racket[x] is a flonum, but much more
accurate for large @racket[y] when @racket[x] cannot be exactly represented
by a flonum.

Suppose we want to compute π@superscript{@racket[y]}, where @racket[y] is a flonum.
If we use @racket[flexpt] with an @italic{approximation} of the irrational base π,
the error is low near zero, but grows with distance from the origin:
@interaction[#:eval untyped-eval
                    (eval:alts (bf-precision 128)
                               (eval:result ""))
                    (eval:alts (define y 150.0)
                               (eval:result ""))
                    (eval:alts (define pi^y (bigfloat->rational (bfexpt pi.bf (bf y))))
                               (eval:result ""))
                    (eval:alts (flulp-error (flexpt pi y) pi^y)
                               (eval:result @racketresultfont{43.12619934359266}))]
Using @racket[make-flexpt], the error is near rounding error everywhere:
@interaction[#:eval untyped-eval
                    (eval:alts (define flexppi (make-flexpt (bigfloat->rational pi.bf)))
                               (eval:result ""))
                    (eval:alts (flulp-error (flexppi y) pi^y)
                               (eval:result @racketresultfont{0.8738006564073412}))]
This example is used in the implementations of @racket[zeta] and @racket[psi].
}

@defproc[(flsqrt1pm1 [x Flonum]) Flonum]{
Like @racket[(- (flsqrt (+ 1.0 x)) 1.0)], but accurate when @racket[x] is small.
}

@defproc[(fllog1pmx [x Flonum]) Flonum]{
Like @racket[(- (fllog1p x) x)], but accurate when @racket[x] is small.
}

@defproc[(flexpsqr [x Flonum]) Flonum]{
Like @racket[(flexp (* x x))], but accurate when @racket[x] is large.
}

@defproc[(flgauss [x Flonum]) Flonum]{
Like @racket[(flexp (- (* x x)))], but accurate when @racket[x] is large.
}

@defproc[(flexp1p [x Flonum]) Flonum]{
Like @racket[(flexp (+ 1.0 x))], but accurate when @racket[x] is near a power of 2.
}

@deftogether[(@defproc[(flsinpix [x Flonum]) Flonum]
              @defproc[(flcospix [x Flonum]) Flonum]
              @defproc[(fltanpix [x Flonum]) Flonum])]{
Like @racket[(flsin (* pi x))], @racket[(flcos (* pi x))] and @racket[(fltan (* pi x))], respectively,
but accurate near roots and singularities. When @racket[x = (+ n 0.5)] for some integer @racket[n],
@racket[(fltanpix x) = +nan.0].
}

@deftogether[(@defproc[(flcscpix [x Flonum]) Flonum]
              @defproc[(flsecpix [x Flonum]) Flonum]
              @defproc[(flcotpix [x Flonum]) Flonum])]{
Like @racket[(/ 1.0 (flsinpix x))], @racket[(/ 1.0 (flcospix x))] and @racket[(/ 1.0 (fltanpix x))],
respectively, but the first two return @racket[+nan.0] at singularities and @racket[flcotpix] avoids
a double reciprocal.
}

@section{Log-Space Arithmetic}

It is often useful, especially when working with probabilities and probability
densities, to represent nonnegative numbers in @deftech{log space}, or as the
natural logs of their true values. Generally, the reason is that the @italic{smallest}
positive flonum is @italic{too large}.

For example, say we want the probability density of the standard normal distribution
(the bell curve) at 50 standard deviations from zero:
@interaction[#:eval untyped-eval
                    (require math/distributions)
                    (pdf (normal-dist) 50.0)]
Mathematically, the density is nonzero everywhere, but the density at 50 is less than
@racket[+min.0]. However, its density in log space, or its log-density, is representable:
@interaction[#:eval untyped-eval
                    (pdf (normal-dist) 50.0 #t)]
While this example may seem contrived, it is very common, when computing the density
of a @italic{vector} of data, for the product of the densities to be too small to represent directly.

In log space, exponentiation becomes multiplication, multiplication becomes addition, and
addition becomes tricky. See @racket[lg+] and @racket[lgsum] for solutions.

@deftogether[(@defproc[(lg* [logx Flonum] [logy Flonum]) Flonum]
              @defproc[(lg/ [logx Flonum] [logy Flonum]) Flonum]
              @defproc[(lgprod [logxs (Listof Flonum)]) Flonum])]{
Equivalent to @racket[(fl+ logx logy)], @racket[(fl- logx logy)] and @racket[(flsum logxs)], respectively.
}

@deftogether[(@defproc[(lg+ [logx Flonum] [logy Flonum]) Flonum]
              @defproc[(lg- [logx Flonum] [logy Flonum]) Flonum])]{
Like @racket[(fllog (+ (flexp logx) (flexp logy)))] and @racket[(fllog (- (flexp logx) (flexp logy)))],
respectively, but more accurate and less prone to overflow and underflow.
     
When @racket[logy > logx], @racket[lg-] returns @racket[+nan.0]. Both functions correctly treat
@racket[-inf.0] as log-space @racket[0.0].

To add more than two log-space numbers with the same guarantees, use @racket[lgsum].

@examples[#:eval untyped-eval
                 (lg+ (fllog 0.5) (fllog 0.2))
                 (flexp (lg+ (fllog 0.5) (fllog 0.2)))
                 (lg- (fllog 0.5) (fllog 0.2))
                 (flexp (lg- (fllog 0.5) (fllog 0.2)))
                 (lg- (fllog 0.2) (fllog 0.5))]

Though more accurate than a naive implementation, both functions are prone to @tech{catastrophic
cancellation} in regions where they output a value close to @racket[0.0] (or log-space @racket[1.0]).
While these outputs have high relative error, their absolute error is very low, and when
exponentiated, nearly have just rounding error. Further, catastrophic cancellation is unavoidable
when @racket[logx] and @racket[logy] themselves have error, which is by far the common case.

These are, of course, excuses---but for floating-point research generally. There are currently no
reasonably fast algorithms for computing @racket[lg+] and @racket[lg-] with low relative error.
For now, if you need that kind of accuracy, use @racketmodname[math/bigfloat].
}

@defproc[(lgsum [logxs (Listof Flonum)]) Flonum]{
Like folding @racket[lg+] over @racket[logxs], but more accurate. Analogous to @racket[flsum].
}

@deftogether[(@defproc[(lg1+ [logx Flonum]) Flonum]
              @defproc[(lg1- [logx Flonum]) Flonum])]{
Equivalent to @racket[(lg+ (fllog 1.0) logx)] and @racket[(lg- (fllog 1.0) logx)],
respectively, but faster.
}

@defproc[(flprobability? [x Flonum] [log? Any #f]) Boolean]{
When @racket[log?] is @racket[#f], returns @racket[#t] when @racket[(<= 0.0 x 1.0)].
When @racket[log?] is @racket[#t], returns @racket[#t] when @racket[(<= -inf.0 x 0.0)].
@examples[#:eval untyped-eval
                 (flprobability? -0.1)
                 (flprobability? 0.5)
                 (flprobability? +nan.0 #t)]
}

@section{Debugging Flonum Functions}

The following functions and constants are useful in authoring and debugging flonum functions
that must be accurate on the largest possible domain.

Suppose we approximate @racket[flexp] using its Taylor series centered at @racket[1.0], truncated
after three terms (a second-order polynomial):
@racketblock+eval[#:eval untyped-eval
(define (exp-taylor-1 x)
  (let ([x  (- x 1.0)])
    (* (flexp 1.0) (+ 1.0 x (* 0.5 x x)))))
]

We can use @racketmodname[plot] and @racket[flstep] (documented below) to compare its output
to that of @racket[flexp] on very small intervals:
@interaction[#:eval untyped-eval
(plot (list (function exp-taylor-1 #:label "exp-taylor-1 x")
            (function exp #:color 2 #:label "exp x"))
      #:x-min (flstep 1.00002 -40)
      #:x-max (flstep 1.00002 40)
      #:width 480)
]
Such plots are especially useful when centered at a boundary between two different
approximation methods.

For larger intervals, assuming the approximated function is fairly smooth,
we can get a better idea how close the approximation is using @racket[flulp-error]:
@interaction[#:eval untyped-eval
(plot (function (λ (x) (flulp-error (exp-taylor-1 x) (exp x))))
      #:x-min 0.99998 #:x-max 1.00002 #:y-label "Error (ulps)")
]
We can infer from this plot that our Taylor series approximation has close to
rounding error (no more than an @tech{ulp}) near @racket[1.0], but quickly becomes worse farther away.

To get a ground-truth function such as @racket[exp] to test against, compute the
outputs as accurately as possible using exact rationals or high-precision @tech{bigfloats}.

@subsection{Measuring Floating-Point Error}

@defproc[(flulp [x Flonum]) Flonum]{
Returns @racket[x]'s @deftech{ulp}, or @bold{u}nit in @bold{l}ast @bold{p}lace:
the magnitude of the least significant bit in @racket[x].
@examples[#:eval untyped-eval
                 (flulp 1.0)
                 (flulp 1e-100)
                 (flulp 1e200)]
}

@defproc[(flulp-error [x Flonum] [r Real]) Flonum]{
Returns the absolute number of @tech{ulps} difference between @racket[x] and @racket[r].

For non-rational arguments such as @racket[+nan.0], @racket[flulp-error] returns @racket[0.0]
if @racket[(eqv? x r)]; otherwise it returns @racket[+inf.0].

A flonum function with maximum error @racket[0.5] ulps exhibits only rounding error;
it is @italic{correct}. A flonum function with maximum error no greater than a few ulps
is @italic{accurate}. Most moderately complicated flonum functions, when implemented
directly, seem to have over a hundred thousand ulps maximum error.

@examples[#:eval untyped-eval
                 (flulp-error 0.5 1/2)
                 (flulp-error #i1/7 1/7)
                 (flulp-error +inf.0 +inf.0)
                 (flulp-error +inf.0 +nan.0)
                 (flulp-error 1e-20 0.0)
                 (flulp-error (- 1.0 (fl 4999999/5000000)) 1/5000000)]
@margin-note*{* You can make an exception when the result is to be exponentiated.
              If @racket[x] has small @racket[absolute-error], then @racket[(exp x)]
              has small @racket[relative-error] and small @racket[flulp-error].}
The last example subtracts two nearby flonums, the second of which had already been
rounded, resulting in horrendous error. This is an example of @deftech{catastrophic
cancellation}. Avoid subtracting nearby flonums whenever possible.*

See @racket[relative-error] for a similar way to measure approximation error when the
approximation is not necessarily represented by a flonum.
}

@subsection{Flonum Constants}

@deftogether[(@defthing[-max.0 Flonum]
              @defthing[-min.0 Flonum]
              @defthing[+min.0 Flonum]
              @defthing[+max.0 Flonum])]{
The nonzero, rational flonums with maximum and minimum magnitude.
@examples[#:eval untyped-eval (list -max.0 -min.0 +min.0 +max.0)]
}

@defthing[epsilon.0 Flonum]{
The smallest flonum that can be added to @racket[1.0] to yield a larger number,
or the magnitude of the least significant bit in @racket[1.0].
@examples[#:eval untyped-eval
                 epsilon.0
                 (flulp 1.0)]

Epsilon is often used in stopping conditions for iterative or additive approximation methods.
For example, the following function uses it to stop Newton's method to compute square roots.
(Please do not assume this example is robust.)
@racketblock[(define (newton-sqrt x)
               (let loop ([y  (* 0.5 x)])
                 (define dy (/ (- x (sqr y)) (* 2.0 y)))
                 (if ((abs dy) . <= . (abs (* 0.5 epsilon.0 y)))
                     (+ y dy)
                     (loop (+ y dy)))))]
When @racket[((abs dy) . <= . (abs (* 0.5 epsilon.0 y)))], adding @racket[dy] to @racket[y]
rarely results in a different flonum. The value @racket[0.5] can be changed to allow
looser approximations. This is a good idea when the approximation does not have to be
as close as possible (e.g. it is only a starting point for another approximation method),
or when the computation of @racket[dy] is known to be inaccurate.

Approximation error is often understood in terms of relative error in epsilons.
Number of epsilons relative error roughly corresponds with error in ulps, except
when the approximation is subnormal.
}

@subsection{Low-Level Flonum Operations}

@defproc[(flonum->bit-field [x Flonum]) Natural]{
Returns the bits comprising @racket[x] as an integer.
A convenient shortcut for composing @racket[integer-bytes->integer] with
@racket[real->floating-point-bytes].
@examples[#:eval untyped-eval
                 (number->string (flonum->bit-field -inf.0) 16)
                 (number->string (flonum->bit-field +inf.0) 16)
                 (number->string (flonum->bit-field -0.0) 16)
                 (number->string (flonum->bit-field 0.0) 16)
                 (number->string (flonum->bit-field -1.0) 16)
                 (number->string (flonum->bit-field 1.0) 16)
                 (number->string (flonum->bit-field +nan.0) 16)]
}

@defproc[(bit-field->flonum [i Integer]) Flonum]{
The inverse of @racket[flonum->bit-field].
}

@defproc[(flonum->ordinal [x Flonum]) Integer]{
Returns the signed ordinal index of @racket[x] in a total order over flonums.

When inputs are not @racket[+nan.0], this function is monotone and symmetric;
i.e. if @racket[(fl<= x y)] then @racket[(<= (flonum->ordinal x) (flonum->ordinal y))],
and @racket[(= (flonum->ordinal (- x)) (- (flonum->ordinal x)))].
@examples[#:eval untyped-eval
                 (flonum->ordinal -inf.0)
                 (flonum->ordinal +inf.0)
                 (flonum->ordinal -0.0)
                 (flonum->ordinal 0.0)
                 (flonum->ordinal -1.0)
                 (flonum->ordinal 1.0)
                 (flonum->ordinal +nan.0)]
These properties mean that @racket[flonum->ordinal] does not distinguish @racket[-0.0]
and @racket[0.0].
}

@defproc[(ordinal->flonum [i Integer]) Flonum]{
The inverse of @racket[flonum->ordinal].
}

@defproc[(flonums-between [x Flonum] [y Flonum]) Integer]{
Returns the number of flonums between @racket[x] and @racket[y], excluding one endpoint.
Equivalent to @racket[(- (flonum->ordinal y) (flonum->ordinal x))].
@examples[#:eval untyped-eval
                 (flonums-between 0.0 1.0)
                 (flonums-between 1.0 2.0)
                 (flonums-between 2.0 3.0)
                 (flonums-between 1.0 +inf.0)]
}

@defproc[(flstep [x Flonum] [n Integer]) Flonum]{
Returns the flonum @racket[n] flonums away from @racket[x], according to @racket[flonum->ordinal].
If @racket[x] is @racket[+nan.0], returns @racket[+nan.0].
@examples[#:eval untyped-eval
                 (flstep 0.0 1)
                 (flstep (flstep 0.0 1) -1)
                 (flstep 0.0 -1)
                 (flstep +inf.0 1)
                 (flstep +inf.0 -1)
                 (flstep -inf.0 -1)
                 (flstep -inf.0 1)
                 (flstep +nan.0 1000)]
}

@deftogether[(@defproc[(flnext [x Flonum]) Flonum]
              @defproc[(flprev [x Flonum]) Flonum])]{
Equivalent to @racket[(flstep x 1)] and @racket[(flstep x -1)], respectively.
}

@defproc[(flsubnormal? [x Flonum]) Boolean]{
Returns @racket[#t] when @racket[x] is a
@hyperlink["http://en.wikipedia.org/wiki/Denormal_number"]{subnormal number}.

Though flonum operations on subnormal numbers are still often implemented
by software exception handling, the situation is improving. Robust
flonum functions should handle subnormal inputs correctly, and reduce error
in outputs as close to zero @tech{ulps} as possible.
}

@deftogether[(@defthing[-max-subnormal.0 Flonum]
              @defthing[+max-subnormal.0 Flonum])]{
The maximum positive and negative subnormal flonums. A flonum @racket[x] is subnormal when
it is not zero and @racket[((abs x) . <= . +max-subnormal.0)].
@examples[#:eval untyped-eval +max-subnormal.0]
}

@section{Additional Flonum Vector Functions}

@defproc[(build-flvector [n Integer] [proc (Index -> Flonum)]) FlVector]{
Creates a length-@racket[n] flonum vector by applying @racket[proc] to the indexes
from @racket[0] to @racket[(- n 1)]. Analogous to @racket[build-vector].
@examples[#:eval untyped-eval
                 (build-flvector 10 fl)]
}

@defform[(inline-build-flvector n proc)
         #:contracts ([n Integer]
                      [proc (Index -> Flonum)])]{
Like @racket[build-flvector], but always inlined. This increases speed at the expense of code size.
}

@defproc[(flvector-map [proc (Flonum Flonum ... -> Flonum)] [xs FlVector] [xss FlVector] ...)
         FlVector]{
Applies @racket[proc] to the corresponding elements of @racket[xs] and @racket[xss]. Analogous to
@racket[vector-map].

The @racket[proc] is meant to accept the same number of arguments as the number of its following
flonum vector arguments. However, a current limitation in Typed Racket requires @racket[proc]
to accept @italic{any} number of arguments. To map a single-arity function such as @racket[fl+]
over the corresponding number of flonum vectors, for now, use @racket[inline-flvector-map].
}

@defform[(inline-flvector-map proc xs xss ...)
         #:contracts ([proc (Flonum Flonum ... -> Flonum)]
                      [xs FlVector]
                      [xss FlVector])]{
Like @racket[flvector-map], but always inlined.
}

@defproc[(flvector-copy! [dest FlVector]
                         [dest-start Integer]
                         [src FlVector]
                         [src-start Integer 0]
                         [src-end Integer (flvector-length src)])
         Void]{
Like @racket[vector-copy!], but for flonum vectors.
}

@deftogether[(@defproc[(list->flvector [vs (Listof Real)]) FlVector]
              @defproc[(flvector->list [xs FlVector]) (Listof Flonum)]
              @defproc[(vector->flvector [vs (Vectorof Real)]) FlVector]
              @defproc[(flvector->vector [xs FlVector]) (Vectorof Flonum)])]{
Convert between lists and flonum vectors, and between vectors and flonum vectors.
}

@deftogether[(@defproc[(flvector+ [xs FlVector] [ys FlVector]) FlVector]
              @defproc[(flvector* [xs FlVector] [ys FlVector]) FlVector]
              @defproc*[([(flvector- [xs FlVector]) FlVector]
                         [(flvector- [xs FlVector] [ys FlVector]) FlVector])]
              @defproc*[([(flvector/ [xs FlVector]) FlVector]
                         [(flvector/ [xs FlVector] [ys FlVector]) FlVector])]
              @defproc[(flvector-scale [xs FlVector] [y Flonum]) FlVector]
              @defproc[(flvector-abs [xs FlVector]) FlVector]
              @defproc[(flvector-sqr [xs FlVector]) FlVector]
              @defproc[(flvector-sqrt [xs FlVector]) FlVector]
              @defproc[(flvector-min [xs FlVector] [ys FlVector]) FlVector]
              @defproc[(flvector-max [xs FlVector] [ys FlVector]) FlVector])]{
Arithmetic lifted to operate on flonum vectors.
}

@defproc[(flvector-sum [xs FlVector]) Flonum]{
Like @racket[flsum], but operates on flonum vectors. In fact, @racket[flsum] is defined in terms
of @racket[flvector-sum].
}

@defproc[(flvector-sums [xs FlVector]) FlVector]{
Computes the partial sums of the elements in @racket[xs] in a way that incurs rounding error only
once for each partial sum.
@examples[#:eval untyped-eval
                 (flvector-sums
                  (flvector 1.0 1e-16 1e-16 1e-16 1e-16 1e100 -1e100))]
Compare the same example computed by direct summation:
@interaction[#:eval untyped-eval
                    (rest
                     (reverse
                      (foldl (λ (x xs) (cons (+ x (first xs)) xs))
                             (list 0.0)
                             '(1.0 1e-16 1e-16 1e-16 1e-16 1e100 -1e100))))]
}

@(close-eval untyped-eval)
