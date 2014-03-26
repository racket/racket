#lang unstable/2d scribble/manual

@(require scribble/eval
          scribble/core
          scribble/html-properties
          racket/sandbox
          unstable/2d/tabular
          (for-label racket/base racket/vector
                     (except-in racket/list permutations) ; FIXME
                     math plot
                     (only-in typed/racket/base
                              ->
                              Flonum Integer Index Real Boolean Any Listof Vectorof FlVector
                              Nonnegative-Flonum Values))
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

@deftogether[(@defproc[(flrational? [x Flonum]) Boolean]
              @defproc[(flinfinite? [x Flonum]) Boolean]
              @defproc[(flnan? [x Flonum]) Boolean]
              @defproc[(flinteger? [x Flonum]) Boolean])]{
Like @racket[rational?], @racket[infinite?], @racket[nan?] and @racket[integer?], but restricted
to flonum input.
In Typed Racket, these are 2-3 times faster as well.
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

@defproc[(flexpt+ [x1 Flonum] [x2 Flonum] [y Flonum]) Flonum]{
Like @racket[(flexpt (+ x1 x2) y)], but more accurate.
}

@defproc[(flexp2 [x Flonum]) Nonnegative-Flonum]{
Equivalent to @racket[(flexpt 2.0 x)], but faster when @racket[x] is an integer.
}

@defproc[(fllog2 [x Flonum]) Flonum]{
Computes the base-2 log of @racket[x] more accurately than @racket[(/ (fllog x) (fllog 2.0))].
In particular, @racket[(fllog2 x)] is correct for any power of two @racket[x].
@examples[#:eval untyped-eval
                 (fllog2 4.5)
                 (/ (fllog (flexp2 -1066.0)) (fllog 2.0))
                 (fllog2 (flexp2 -1066.0))]
Maximum observed error is 0.5006 @tech{ulps}, but is almost always no more than 0.5 (i.e. it is
almost always @italic{correct}).
}

@defproc[(fllogb [b Flonum] [x Flonum]) Flonum]{
Computes the base-@racket[b] log of @racket[x] more accurately than @racket[(/ (fllog x) (fllog b))],
and handles limit values correctly.
@examples[#:eval untyped-eval
                 (plot3d (contour-intervals3d (λ (b x) (fllogb (fl b) (fl x))) 0 4 0 4)
                         #:x-label "b" #:y-label "x")]

Maximum observed error is 2.1 @tech{ulps}, but is usually less than 0.7 (i.e. near rounding error).

Except possibly at limit values (such as @racket[0.0] and @racket[+inf.0], and @racket[b = 1.0])
and except when the inner expression underflows or overflows, @racket[fllogb] approximately meets
these identities for @racket[b > 0.0]:
@itemlist[@item{Left inverse: @racket[(fllogb b (flexpt b y)) = y]}
          @item{Right inverse: @racket[(flexpt b (fllogb b x)) = x] when @racket[x > 0.0]}]

Unlike with @racket[flexpt], there is no standard for @racket[fllogb]'s behavior at limit values.
Fortunately, deriving the following rules (applied in order) is not prohibitively difficult.
@centered[
  #2dtabular
  ╔═════════════════════════════════╦══════════════════╦═════════════════╗
  ║ @bold{Case}                     ║@bold{Condition}  ║ @bold{Value}    ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb b 1.0)]         ║                  ║ @racket[0.0]    ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb 1.0 x)]         ║                  ║ @racket[+nan.0] ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb b x)]           ║ @racket[b < 0.0] ║ @racket[+nan.0] ║
  ║                                 ║      " or "      ║                 ║
  ║                                 ║ @racket[x < 0.0] ║                 ║
  ╠═════════════════════════════════╩══════════════════╩═════════════════╣
  ║                                                                      ║
  ║                        @italic{Double limits}                        ║
  ║                                                                      ║
  ╠═════════════════════════════════╦══════════════════╦═════════════════╣
  ║ @racket[(fllogb 0.0 0.0)]       ║                  ║ @racket[+inf.0] ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb 0.0 +inf.0)]    ║                  ║ @racket[-inf.0] ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb +inf.0 0.0)]    ║                  ║ @racket[-inf.0] ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb +inf.0 +inf.0)] ║                  ║ @racket[+inf.0] ║
  ╠═════════════════════════════════╩══════════════════╩═════════════════╣
  ║                                                                      ║
  ║              @italic{Limits with respect to @racket[b]}              ║
  ║                                                                      ║
  ╠═════════════════════════════════╦══════════════════╦═════════════════╣
  ║ @racket[(fllogb 0.0 x)]         ║ @racket[x < 1.0] ║ @racket[0.0]    ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb 0.0 x)]         ║ @racket[x > 1.0] ║ @racket[-0.0]   ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb +inf.0 x)]      ║ @racket[x > 1.0] ║ @racket[0.0]    ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb +inf.0 x)]      ║ @racket[x < 1.0] ║ @racket[-0.0]   ║
  ╠═════════════════════════════════╩══════════════════╩═════════════════╣
  ║                                                                      ║
  ║              @italic{Limits with respect to @racket[x]}              ║
  ║                                                                      ║
  ╠═════════════════════════════════╦══════════════════╦═════════════════╣
  ║ @racket[(fllogb b 0.0)]         ║ @racket[b < 1.0] ║ @racket[+inf.0] ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb b 0.0)]         ║ @racket[b > 1.0] ║ @racket[-inf.0] ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb b +inf.0)]      ║ @racket[b > 1.0] ║ @racket[+inf.0] ║
  ╠═════════════════════════════════╬══════════════════╬═════════════════╣
  ║ @racket[(fllogb b +inf.0)]      ║ @racket[b < 1.0] ║ @racket[-inf.0] ║
  ╠═════════════════════════════════╩══════════════════╩═════════════════╣
  ║ #:style                                                              ║
  ║ (style 'plain                                                        ║
  ║        (list (table-columns (list (style 'plain (list 'left))        ║
  ║                                   (style 'plain (list 'center))      ║
  ║                                   (style 'plain (list 'right))))     ║
  ║              (attributes '((width . "90%")))))                       ║
  ╚══════════════════════════════════════════════════════════════════════╝]
Most of these rules are derived by taking limits of the mathematical base-@racket[b] log function.
Except for @racket[(fllogb 1.0 x)], when doing so gives rise to ambiguities, they are resolved using
@racket[flexpt]'s behavior, which follows the IEEE 754 and C99 standards for @tt{pow}.

For example, consider @racket[(fllogb 0.0 0.0)].
Taking an interated limit, we get ∞ if the outer limit is with respect to @racket[x], or 0 if the
outer limit is with respect to @racket[b].
This would normally mean @racket[(fllogb 0.0 0.0) = +nan.0].

However, choosing @racket[+inf.0] ensures that these additional left-inverse and right-inverse
identities hold:
@racketblock[(fllogb 0.0 (flexpt 0.0 +inf.0)) = +inf.0
             (flexpt 0.0 (fllogb 0.0 0.0)) = 0.0]
Further, choosing @racket[0.0] does not ensure that any additional identities hold.
}

@defproc[(flbracketed-root [f (Flonum -> Flonum)] [a Flonum] [b Flonum]) Flonum]{
Uses the @hyperlink["http://en.wikipedia.org/wiki/Brent%27s_method"]{Brent-Dekker method} to
find a floating-point root of @racket[f] (an @racket[x : Flonum] for which @racket[(f x)] is
very near a zero crossing) between @racket[a] and @racket[b].
The values @racket[(f a)] and @racket[(f b)] must have opposite signs, but @racket[a] and @racket[b]
may be in any order.
@examples[#:eval untyped-eval
                 (define (f x) (+ 1.0 (* (+ x 3.0) (sqr (- x 1.0)))))
                 (define x0 (flbracketed-root f -4.0 2.0))
                 (plot (list (x-axis)
                             (function f -4 2)
                             (function-label f x0))
                       #:y-min -10)
                 (f (flprev x0))
                 (f x0)
                 (flbracketed-root f -1.0 2.0)]
Caveats:
@(itemlist
  @item{There is no guarantee that @racket[flbracketed-root] will find any @emph{particular} root.
        Moreover, future updates to its implementation could make it find different ones.}
  @item{There is currently no guarantee that it will find the @emph{closest} @racket[x] to an exact root.}
  @item{It currently runs for at most 5000 iterations.})
It usually requires far fewer iterations, especially if the initial bounds @racket[a] and @racket[b] are tight.
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

@section{Double-Double Operations}

For extra precision, floating-point computations may use two nonoverlapping flonums to represent a single number.
Such pairs are often called @deftech{double-double} numbers.
The exact sum of the pair is the number it represents.
(Because they are nonoverlapping, the floating-point sum is equal to the largest.)

For speed, especially with arithmetic operations, there is no data type for double-double numbers.
They are always unboxed: given as two arguments, and received as two values.
In both cases, the number with higher magnitude is first.

Inputs are never checked to ensure they are sorted and nonoverlapping, but outputs are guaranteed to be
sorted and nonoverlapping if inputs are.

@defproc*[([(fl2 [x Real]) (Values Flonum Flonum)]
           [(fl2 [x Flonum] [y Flonum]) (Values Flonum Flonum)])]{
Converts a real number or the sum of two flonums into a @tech{double-double}.
@examples[#:eval untyped-eval
                 (fl 1/7)
                 (relative-error (fl 1/7) 1/7)
                 (define-values (x2 x1) (fl2 1/7))
                 (list x2 x1)
                 (fl (relative-error (+ (inexact->exact x2)
                                        (inexact->exact x1))
                                     1/7))]
Notice that the exact sum of @racket[x2] and @racket[x1] in the preceeding example has very low
relative error.

If @racket[x] is not rational, @racket[fl2] returns @racket[(values x 0.0)].
}

@defproc[(fl2->real [x2 Flonum] [x1 Flonum]) Real]{
Returns the exact sum of @racket[x2] and @racket[x1] if @racket[x2] is rational, @racket[x2] otherwise.
@examples[#:eval untyped-eval
                 (define-values (x2 x1) (fl2 1/7))
                 (fl2->real x2 x1)]
}

@defproc[(fl2? [x2 Flonum] [x1 Flonum]) Boolean]{
When @racket[x2] is rational, returns @racket[#t] when @racket[(flabs x2) > (flabs x1)] and @racket[x2]
and @racket[x1] are nonoverlapping.
When @racket[x2] is not rational, returns @racket[(fl= x1 0.0)].

@examples[#:eval untyped-eval
                 (define-values (x2 x1) (fl2 1/7))
                 (fl2? x2 x1)
                 (fl2? #i1/7 #i1/13)
                 (fl2? +inf.0 0.0001)]

This function is quite slow, so it is used only for testing.
}

@deftogether[(@defproc[(fl+/error [x Flonum] [y Flonum]) (Values Flonum Flonum)]
              @defproc[(fl-/error [x Flonum] [y Flonum]) (Values Flonum Flonum)]
              @defproc[(fl*/error [x Flonum] [y Flonum]) (Values Flonum Flonum)]
              @defproc[(fl//error [x Flonum] [y Flonum]) (Values Flonum Flonum)]
              @defproc[(flsqr/error [x Flonum]) (Values Flonum Flonum)]
              @defproc[(flsqrt/error [x Flonum]) (Values Flonum Flonum)]
              @defproc[(flexp/error [x Flonum]) (Values Flonum Flonum)]
              @defproc[(flexpm1/error [x Flonum]) (Values Flonum Flonum)])]{
Compute the same values as @racket[(fl+ x y)], @racket[(fl- x y)], @racket[(fl* x y)], @racket[(fl/ x y)],
@racket[(fl* x x)], @racket[(flsqrt x)], @racket[(flexp x)] and @racket[(flexpm1 x)], but return the normally
rounded-off low-order bits as the second value.
The result is an unboxed @tech{double-double}.

Use these functions to generate double-double numbers directly from the results of floating-point operations.

@examples[#:eval untyped-eval
                 (define x1 (fl 1/7))
                 (define x2 (fl 1/13))
                 (define z* (bigfloat->real (bfexp (bf* (bf x1) (bf x2)))))
                 (relative-error (flexp (fl* x1 x2)) z*)
                 (let*-values ([(y2 y1)  (fl*/error x1 x2)]
                               [(z2 z1)  (fl2exp y2 y1)])
                   (fl (relative-error (fl2->real z2 z1) z*)))]

For @racket[flexp/error] and @racket[flexpm1/error], the largest observed error is 3 ulps.
(See @racket[fl2ulp].)
For the rest, the largest observed error is 0.5 ulps.
}

@deftogether[(@defproc[(fl2zero? [x2 Flonum] [x1 Flonum]) Boolean]
              @defproc[(fl2rational? [x2 Flonum] [x1 Flonum]) Boolean]
              @defproc[(fl2positive? [x2 Flonum] [x1 Flonum]) Boolean]
              @defproc[(fl2negative? [x2 Flonum] [x1 Flonum]) Boolean]
              @defproc[(fl2infinite? [x2 Flonum] [x1 Flonum]) Boolean]
              @defproc[(fl2nan? [x2 Flonum] [x1 Flonum]) Boolean])]{
Like @racket[zero?], @racket[rational?], @racket[positive?], @racket[negative?], @racket[infinite?] and @racket[nan?],
but for @tech{double-double} flonums.
}

@deftogether[(@defproc[(fl2+ [x2 Flonum] [x1 Flonum] [y2 Flonum] [y1 Flonum 0.0]) (Values Flonum Flonum)]
              @defproc[(fl2- [x2 Flonum] [x1 Flonum] [y2 Flonum] [y1 Flonum 0.0]) (Values Flonum Flonum)]
              @defproc[(fl2* [x2 Flonum] [x1 Flonum] [y2 Flonum] [y1 Flonum 0.0]) (Values Flonum Flonum)]
              @defproc[(fl2/ [x2 Flonum] [x1 Flonum] [y2 Flonum] [y1 Flonum 0.0]) (Values Flonum Flonum)]
              @defproc[(fl2abs [x2 Flonum] [x1 Flonum 0.0]) (Values Flonum Flonum)]
              @defproc[(fl2sqr [x2 Flonum] [x1 Flonum 0.0]) (Values Flonum Flonum)]
              @defproc[(fl2sqrt [x2 Flonum] [x1 Flonum 0.0]) (Values Flonum Flonum)])]{
Arithmetic and square root for @tech{double-double} flonums.

For arithmetic, error is less than 8 ulps. (See @racket[fl2ulp].)
For @racket[fl2sqr] and @racket[fl2sqrt], error is less than 1 ulp, and @racket[fl2abs] is exact.
}

@deftogether[(@defproc[(fl2= [x2 Flonum] [x1 Flonum] [y2 Flonum] [y1 Flonum]) (Values Flonum Flonum)]
              @defproc[(fl2> [x2 Flonum] [x1 Flonum] [y2 Flonum] [y1 Flonum]) (Values Flonum Flonum)]
              @defproc[(fl2< [x2 Flonum] [x1 Flonum] [y2 Flonum] [y1 Flonum]) (Values Flonum Flonum)]
              @defproc[(fl2>= [x2 Flonum] [x1 Flonum] [y2 Flonum] [y1 Flonum]) (Values Flonum Flonum)]
              @defproc[(fl2<= [x2 Flonum] [x1 Flonum] [y2 Flonum] [y1 Flonum]) (Values Flonum Flonum)])]{
Comparison functions for @tech{double-double} flonums.
}

@deftogether[(@defproc[(fl2exp [x2 Flonum] [x1 Flonum]) (Values Flonum Flonum)]
              @defproc[(fl2log [x2 Flonum] [x1 Flonum]) (Values Flonum Flonum)]
              @defproc[(fl2expm1 [x2 Flonum] [x1 Flonum]) (Values Flonum Flonum)]
              @defproc[(fl2log1p [x2 Flonum] [x1 Flonum]) (Values Flonum Flonum)])]{
Like @racket[flexp], @racket[fllog], @racket[flexpm1] and @racket[fllog1p], but for @tech{double-double} flonums.

For @racket[fl2exp] and @racket[fl2expm1], error is less than 3 ulps. (See @racket[fl2ulp].)
For @racket[fl2log] and @racket[fl2log1p], error is less than 2 ulps.
}

@subsection{Debugging Double-Double Functions}

@deftogether[(@defproc[(fl2ulp [x2 Flonum] [x1 Flonum]) Flonum]
              @defproc[(fl2ulp-error [x2 Flonum] [x1 Flonum] [r Real]) Flonum])]{
Like @racket[flulp] and @racket[flulp-error], but for @tech{double-double} flonums.

The unit in last place of a double-double is that of the higher-order of the pair, shifted 52 bits right.

@examples[#:eval untyped-eval
                 (fl2ulp 1.0 0.0)
                 (let-values ([(x2 x1)  (fl2 1/7)])
                   (fl2ulp-error x2 x1 1/7))]
}

@deftogether[(@defthing[+max.hi Flonum]
              @defthing[+max.lo Flonum]
              @defthing[-max.hi Flonum]
              @defthing[-max.lo Flonum])]{
The maximum-magnitude, unboxed @tech{double-double} flonums.
}

@deftogether[(@defthing[+max-subnormal.hi Flonum]
              @defthing[-max-subnormal.hi Flonum])]{
The high-order flonum of the maximum-magnitude, subnormal @tech{double-double} flonums.
@interaction[#:eval untyped-eval
                    +max-subnormal.0
                    +max-subnormal.hi]
Try to avoid computing with double-doubles in the subnormal range in intermediate computations.
}

@;{Document these when they are finished:
   fl2step
   fl2next
   fl2prev}

@subsection{Low-Level Double-Double Operations}

The following syntactic forms are fast versions of functions like @racket[fl+/error].
They are fast because they make assumptions about the magnitudes of and relationships between their arguments,
and do not handle non-rational double-double flonums properly.

@deftogether[(@defform[(fast-mono-fl+/error x y)]
              @defform[(fast-mono-fl-/error x y)])]{
Return two values: @racket[(fl+ x y)] or @racket[(fl- x y)], and its rounding error.
Both assume @racket[(flabs x) > (flabs y)].
The values are unspecified when @racket[x] or @racket[y] is not rational.
}

@deftogether[(@defform[(fast-fl+/error x y)]
              @defform[(fast-fl-/error x y)])]{
Like @racket[fast-mono-fl+/error] and @racket[fast-mono-fl-/error], but do not assume @racket[(flabs x) > (flabs y)].
}

@deftogether[(@defform[(fast-fl*/error x y)]
              @defform[(fast-fl//error x y)]
              @defform[(fast-flsqr/error x)])]{
Like @racket[fl*/error], @racket[fl//error] and @racket[flsqr/error], but faster, and may return garbage when an argument is subnormal or nearly infinite.
}

@defform[(flsplit x)]{
Returns nonoverlapping @racket[(values y2 y1)], each with 26 bits precision, with @racket[(flabs y2) > (flabs y1)],
such that @racket[(fl+ y2 y1) = x].
For @racket[(flabs x) > 1.3393857490036326e+300], returns @racket[(values +nan.0 +nan.0)].

Used to implement double-double multiplication.
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
