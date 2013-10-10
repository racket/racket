#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/unsafe/ops
                     math plot
                     (only-in typed/racket/base
                              Flonum Real Integer Natural Zero Positive-Integer Exact-Rational
                              Boolean Any Listof U case-> -> :print-type))
          "utils.rkt")

@(define untyped-eval (make-untyped-math-eval))

@title[#:tag "special"]{Special Functions}
@(author-neil)

@defmodule[math/special-functions]

The term ``special function'' has no formal definition. However, for the purposes of the
@racketmodname[math] library, a @deftech{special function} is one that is not @tech{elementary}.

The special functions are split into two groups: @secref{real-functions} and
@secref{flonum-functions}. Functions that accept real arguments are usually defined
in terms of their flonum counterparts, but are different in two crucial ways:
@itemlist[
 @item{Many return exact values for certain exact arguments.}
 @item{When applied to exact arguments outside their domains, they raise an
       @racket[exn:fail:contract] instead of returning @racket[+nan.0].}
]

Currently, @racketmodname[math/special-functions] does not export any functions that accept
or return complex numbers. Mathematically, some of them could return complex numbers given
real numbers, such @racket[hurwitz-zeta] when given a negative second argument. In
these cases, they raise an @racket[exn:fail:contract] (for an exact argument) or return
@racket[+nan.0] (for an inexact argument).

Most real functions have more than one type, but they are documented as having only
one. The documented type is the most general type, which is used to generate a contract for
uses in untyped code. Use @racket[:print-type] to see all of a function's types.

A function's types state theorems about its behavior in a way that Typed Racket can understand
and check. For example, @racket[lambert] has these types:
@racketblock[(case-> (Zero -> Zero)
                     (Flonum -> Flonum)
                     (Real -> (U Zero Flonum)))]

Because @racket[lambert : Zero -> Zero], Typed Racket proves during typechecking that one
of its exact cases is @racket[(lambert 0) = 0].

Because the theorem @racket[lambert : Flonum -> Flonum] is stated as a type and proved
by typechecking, Typed Racket's optimizer can transform the expressions around its use
into bare-metal floating-point operations. For example, @racket[(+ 2.0 (lambert 3.0))] is
transformed into @racket[(unsafe-fl+ 2.0 (lambert 3.0))].

The most general type @racket[Real -> (U Zero Flonum)] is used to generate
@racket[lambert]'s contract when it is used in untyped code. Except for this discussion,
this the only type documented for @racket[lambert].

@section[#:tag "real-functions"]{Real Functions}

@defproc[(gamma [x Real]) (U Positive-Integer Flonum)]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Gamma_function"]{gamma function},
a generalization of the factorial function to the entire real line, except nonpositive integers.
When @racket[x] is an exact integer, @racket[(gamma x)] is exact.

@examples[#:eval untyped-eval
                 (plot (list (function (λ (x) (gamma (+ 1 x))) 0 4.5
                                       #:label "gamma(x+1)")
                             (function (λ (x) (factorial (truncate x))) #:color 2
                                       #:label "factorial(floor(x))")))
                 (plot (function gamma -2.5 5.5) #:y-min -50 #:y-max 50)
                 (gamma 5)
                 (gamma 5.0)
                 (factorial 4)
                 (gamma -1)
                 (gamma -1.0)
                 (gamma 0.0)
                 (gamma -0.0)
                 (gamma 172.0)
                 (eval:alts
                  (bf (gamma 172))
                  (eval:result @racketresultfont{(bf "1.241018070217667823424840524103103992618e309")}))]

Error is no more than 10 @tech{ulps} everywhere that has been tested, and is usually no more than 4
ulps.
}

@defproc[(log-gamma [x Real]) (U Zero Flonum)]{
Like @racket[(log (abs (gamma x)))], but more accurate and without unnecessary overflow.
The only exact cases are @racket[(log-gamma 1) = 0] and @racket[(log-gamma 2) = 0].

@examples[#:eval untyped-eval
                 (plot (list (function log-gamma -5.5 10.5 #:label "log-gamma(x)")
                             (function (λ (x) (log (abs (gamma x))))
                                       #:color 2 #:style 'long-dash #:width 2
                                       #:label "log(abs(gamma(x)))")))
                 (log-gamma 5)
                 (log (abs (gamma 5)))
                 (log-gamma -1)
                 (log-gamma -1.0)
                 (log-gamma 0.0)
                 (log (abs (gamma 172.0)))
                 (log-gamma 172.0)]

Error is no more than 11 @tech{ulps} everywhere that has been tested, and is usually no more than 2
ulps. Error reaches its maximum near negative roots.
}

@defproc[(psi0 [x Real]) Flonum]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Digamma_function"]{digamma function},
the logarithmic derivative of the gamma function.

@examples[#:eval untyped-eval
                 (plot (function psi0 -2.5 4.5) #:y-min -5 #:y-max 5)
                 (psi0 0)
                 (psi0 1)
                 (- gamma.0)]

Except near negative roots, maximum observed error is 2 @tech{ulps}, but is usually no more
than 1.

Near negative roots, which occur singly between each pair of negative integers, @racket[psi0]
exhibits @tech{catastrophic cancellation} from using the reflection formula, meaning that
relative error is effectively unbounded. However, maximum observed @racket[absolute-error]
is @racket[(* 5 epsilon.0)]. This is the best we can do for now, because there are currently
no reasonably fast algorithms for computing @racket[psi0] near negative roots with low relative
error.

If you need low relative error near negative roots, use @racket[bfpsi0].
}

@defproc[(psi [m Integer] [x Real]) Flonum]{
Computes a @hyperlink["http://en.wikipedia.org/wiki/Polygamma_function"]{polygamma function},
or the @racket[m]th logarithmic derivative of the gamma function. The order @racket[m] must be
a natural number, and @racket[x] may not be zero or a negative integer. Note that
@racket[(psi 0 x) = (psi0 x)].

@examples[#:eval untyped-eval
                 (plot (for/list ([m  (in-range 4)])
                         (function (λ (x) (psi m x)) -2.5 2.5
                                   #:color m #:style m #:label (format "psi~a(x)" m)))
                       #:y-min -300 #:y-max 300 #:legend-anchor 'top-right)
                 (psi -1 2.3)
                 (psi 0 -1.1)
                 (psi0 -1.1)]

From spot checks with @racket[m > 0], error appears to be as with @racket[psi0]: very low except
near negative roots. Near negative roots, relative error is apparently unbounded, but absolute error
is low.
}

@deftogether[(@defproc[(erf [x Real]) Real]
              @defproc[(erfc [x Real]) Real])]{
Compute the @hyperlink["http://en.wikipedia.org/wiki/Error_function"]{error function and
complementary error function}, respectively. The only exact cases are @racket[(erf 0) = 0]
and @racket[(erfc 0) = 1].

@examples[#:eval untyped-eval
                 (plot (list (function erf -2 2 #:label "erf(x)")
                             (function erfc #:color 2 #:label "erfc(x)")))
                 (erf 0)
                 (erf 1)
                 (- 1 (erfc 1))
                 (erf -1)
                 (- (erfc 1) 1)]

Mathematically, erfc(@italic{x}) = 1 - erf(@italic{x}), but having separate implementations
can help maintain accuracy. To compute an expression containing erf, use @racket[erf] for
@racket[x] near @racket[0.0]. For positive @racket[x] away from @racket[0.0],
manipulate @racket[(- 1.0 (erfc x))] and its surrounding expressions to avoid the subtraction:

@interaction[#:eval untyped-eval
                    (define x 5.2)
                    (bf-precision 128)
                    (eval:alts (define log-erf-x (bigfloat->rational (bflog (bferf (bf x)))))
                               (define log-erf-x (/ -288077151382475259515535085154389899879
                                                    1496577676626844588240573268701473812127674924007424)))
                    (flulp-error (log (erf x)) log-erf-x)
                    (flulp-error (log (- 1.0 (erfc x))) log-erf-x)
                    (flulp-error (fllog1p (- (erfc x))) log-erf-x)]
For negative @racket[x] away from @racket[0.0], do the same with @racket[(- (erfc (- x)) 1.0)].

For @racket[erf], error is no greater than 2 @tech{ulps} everywhere that has been tested, and
is almost always no greater than 1. For @racket[erfc], observed error is no greater than 4 ulps,
and is usually no greater than 2.
}

@deftogether[(@defproc[(lambert [x Real]) (U Zero Flonum)]
              @defproc[(lambert- [x Real]) Flonum])]{
Compute the @hyperlink["http://en.wikipedia.org/wiki/Lambert_W_function"]{Lambert W function},
or the inverse of @racket[x = (* y (exp y))].

This function has two real branches. The @racket[lambert] variant computes the upper branch,
and is defined for @racket[x >= (- (exp -1))]. The @racket[lambert-] variant computes the
lower branch, and is defined for @italic{negative} @racket[x >= (- (exp -1))].
The only exact case is @racket[(lambert 0) = 0].

@examples[#:eval untyped-eval
                 (plot (list (function lambert (- (exp -1)) 1)
                             (function lambert- (- (exp -1)) -min.0 #:color 2))
                       #:y-min -4)
                 (lambert 0)
                 (lambert (- (exp -1)))
                 (lambert -1/2)
                 (lambert- 0)
                 (define y0 (lambert -0.1))
                 (define y1 (lambert- -0.1))
                 y0
                 y1
                 (* y0 (exp y0))
                 (* y1 (exp y1))]

The Lambert W function often appears in solutions to equations that contain
@italic{n} log(@italic{n}), such as those that describe the running time of divide-and-conquer
algorithms.

For example, suppose we have a sort that takes @racket[t = (* c n (log n))] time, and we measure
the time it takes to sort an @racket[n = 10000]-element list at @racket[t = 0.245] ms. Solving for
@racket[c], we get
@interaction[#:eval untyped-eval
                    (define n 10000)
                    (define t 0.245)
                    (define c (/ t (* n (log n))))
                    c]
Now we would like to know how many elements we can sort in 100ms. We solve for @racket[n] and
use the solution to define a function @racket[time->sort-size]:
@interaction[#:eval untyped-eval
                    (define (time->sort-size t)
                      (exact-floor (exp (lambert (/ t c)))))
                    (time->sort-size 100)]
Testing the solution, we get
@interaction[#:eval untyped-eval
                    (eval:alts (define lst2 (build-list 2548516 values))
                               (eval:result ""))
                    (eval:alts (time (sort lst2 <))
                               (eval:result "" "cpu time: 80 real time: 93 gc time: 0"))]

For both branches, error is no more than 2 @tech{ulps} everywhere tested.
}

@defproc[(zeta [x Real]) Real]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Riemann_zeta_function"]{Riemann zeta function}.
If @racket[x] is a nonpositive exact integer, @racket[(zeta x)] is exact.

@examples[#:eval untyped-eval
                 (plot (function zeta -2 10) #:y-min -4 #:y-max 4)
                 (plot (function zeta -14 -2))
                 (zeta 0)
                 (zeta 1)
                 (zeta 1.0)
                 (zeta -1)
                 (eval:alts
                  (define num 1000000)
                  (eval:result ""))
                 (eval:alts
                  (define num-coprime
                    (for/sum ([_  (in-range num)])
                      (if (coprime? (random-bits 16) (random-bits 16)) 1 0)))
                  (eval:result ""))
                 (eval:alts (fl (/ num-coprime num))
                            (eval:result @racketresultfont{0.607901}))
                 (/ 1 (zeta 2))]

When @racket[s] is an odd, negative exact integer, @racket[(zeta s)] computes
@racket[(bernoulli (- 1 s))], which can be rather slow.

Maximum observed error is 6 @tech{ulps}, but is usually 3 or less.
}

@defproc[(eta [x Real]) Real]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Dirichlet_eta_function"]{Dirichlet eta function}.
If @racket[x] is a nonpositive exact integer, @racket[(eta x)] is exact.

@examples[#:eval untyped-eval
                 (plot (function eta -10 6))
                 (eta 0)
                 (eta -1)
                 (eta 1)
                 (log 2)]

When @racket[s] is an odd, negative exact integer, @racket[(eta s)] computes
@racket[(bernoulli (- 1 s))], which can be rather slow.

Maximum observed error is 11 @tech{ulps}, but is usually 4 or less.
}

@defproc[(hurwitz-zeta [s Real] [q Real]) Real]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Hurwitz_zeta_function"]{Hurwitz
zeta function} for @racket[s > 1] and @racket[q > 0]. When @racket[s = 1.0] or @racket[q = 0.0],
@racket[(hurwitz-zeta s q) = +inf.0].

@examples[#:eval untyped-eval
                 (plot (list (function zeta 1.5 5)
                             (function (λ (s) (hurwitz-zeta s 1))
                                       #:color 2 #:style 'long-dash #:width 2)))
                 (hurwitz-zeta 1 1)
                 (hurwitz-zeta 1.0 1.0)
                 (hurwitz-zeta 2 1/4)
                 (+ (sqr pi) (* 8 catalan.0))]

While @racket[hurwitz-zeta] currently raises an exception for @racket[s < 1], it may in the future
return real values.

Maximum observed error is 6 @tech{ulps}, but is usually 2 or less.
}

@defproc[(beta [x Real] [y Real]) (U Exact-Rational Flonum)]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Beta_function"]{beta function} for positive
real @racket[x] and @racket[y]. Like @racket[(/ (* (gamma x) (gamma y)) (gamma (+ x y)))],
but more accurate.

@examples[#:eval untyped-eval
                 (plot3d (contour-intervals3d beta 0.25 2 0.25 2) #:angle 250)
                 (beta 0 0)
                 (beta 1 5)
                 (beta 1.0 5.0)]
}

@defproc[(log-beta [x Real] [y Real]) (U Zero Flonum)]{
Like @racket[(log (beta x y))], but more accurate and without unnecessary overflow.
The only exact case is @racket[(log-beta 1 1) = 0].
}

@defproc[(gamma-inc [k Real] [x Real] [upper? Any #f] [regularized? Any #f]) Flonum]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Incomplete_gamma_function"]{incomplete
gamma integral} for @racket[k > 0] and @racket[x >= 0]. When @racket[upper? = #f], it integrates
from zero to @racket[x]; otherwise it integrates from @racket[x] to infinity.

If you are doing statistical work, you should probably use @racket[gamma-dist] instead, which
is defined in terms of @racket[gamma-inc] and is more flexible (e.g. it allows negative @racket[x]).

The following identities should hold:
@itemlist[
@item{@racket[(gamma-inc k 0) = 0]}
@item{@racket[(gamma-inc k +inf.0) = (gamma k)]}
@item{@racket[(+ (gamma-inc k x #f) (gamma-inc k x #t)) = (gamma k)] (approximately)}
@item{@racket[(gamma-inc k x upper? #t) = (/ (gamma-inc k x upper? #f) (gamma k))] (approximately)}
@item{@racket[(gamma-inc k +inf.0 #t #t) = 1.0]}
@item{@racket[(+ (gamma-inc k x #f #t) (gamma-inc k x #t #t)) = 1.0] (approximately)}
]
@examples[#:eval untyped-eval
                 (list
                  (plot3d (contour-intervals3d gamma-inc 0.1 4.5 0 10)
                          #:x-label "k" #:y-label "x" #:width 210 #:height 210)
                  (plot3d (contour-intervals3d
                           (λ (k x) (gamma-inc k x #t)) 0.1 4.5 0 10)
                          #:x-label "k" #:y-label "x" #:width 210 #:height 210))
                 (plot3d (contour-intervals3d
                          (λ (k x) (gamma-inc k x #f #t)) 0.1 20 0 20)
                         #:x-label "k" #:y-label "x")
                 (gamma 4.0)
                 (+ (gamma-inc 4.0 0.5 #f) (gamma-inc 4.0 0.5 #t))
                 (gamma-inc 4.0 +inf.0)
                 (/ (gamma-inc 200.0 50.0 #f) (gamma 200.0))
                 (gamma-inc 200.0 50.0 #f #t)
                 (gamma-inc 0 5.0)
                 (gamma-inc 0.0 5.0)]
}

@defproc[(log-gamma-inc [k Real] [x Real] [upper? Any #f] [regularized? Any #f]) Flonum]{
Like @racket[(log (gamma-inc k x upper? regularized?))], but more accurate and without unnecessary
overflow.
}

@defproc[(beta-inc [a Real] [b Real] [x Real] [upper? Any #f] [regularized? Any #f]) Flonum]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Beta_function#Incomplete_beta_function"]{
incomplete beta integral} for @racket[a > 0], @racket[b > 0] and @racket[0 <= x <= 1].
When @racket[upper? = #f], it integrates from zero to @racket[x]; otherwise, it integrates from
@racket[x] to one.

If you are doing statistical work, you should probably use @racket[beta-dist] instead, which
is defined in terms of @racket[beta-inc] and is more flexible (e.g. it allows negative @racket[x]).

Similar identities should hold as with @racket[gamma-inc].

@examples[#:eval untyped-eval
                 (plot3d (isosurfaces3d (λ (a b x) (beta-inc a b x #f #t))
                                        0.1 2.5 0.1 2.5 0 1 #:label "beta(a,b,x)")
                         #:x-label "a" #:y-label "b" #:z-label "x"
                         #:angle 20 #:altitude 20 #:legend-anchor 'top)]
}

@defproc[(log-beta-inc [a Real] [b Real] [x Real] [upper? Any #f] [regularized? Any #f]) Flonum]{
Like @racket[(log (beta-inc a b x upper? regularized?))], but more accurate and without unnecessary
overflow.

While most areas of this function have error less than @racket[5e-15], when @racket[a] and @racket[b]
have very dissimilar magnitudes (e.g. @racket[1e-16] and @racket[1e16]), it exhibits
@tech{catastrophic cancellation}. We are working on it.
}

@section[#:tag "flonum-functions"]{Flonum Functions}

@defproc[(flgamma [x Flonum]) Flonum]{}
@defproc[(fllog-gamma [x Flonum]) Flonum]{}
@defproc[(flpsi0 [x Flonum]) Flonum]{}
@defproc[(flpsi [m Integer] [x Flonum]) Flonum]{}
@defproc[(flerf [x Flonum]) Flonum]{}
@defproc[(flerfc [x Flonum]) Flonum]{}
@defproc[(fllambert [x Flonum]) Flonum]{}
@defproc[(fllambert- [x Flonum]) Flonum]{}
@defproc[(flzeta [x Flonum]) Flonum]{}
@defproc[(fleta [x Flonum]) Flonum]{}
@defproc[(flhurwitz-zeta [s Flonum] [q Flonum]) Flonum]{}
@defproc[(flbeta [x Flonum] [y Flonum]) Flonum]{}
@defproc[(fllog-beta [x Flonum] [y Flonum]) Flonum]{}
@defproc[(flgamma-inc [k Flonum] [x Flonum] [upper? Any] [regularized? Any]) Flonum]{}
@defproc[(fllog-gamma-inc [k Flonum] [x Flonum] [upper? Any] [regularized? Any]) Flonum]{}
@defproc[(flbeta-inc [a Flonum] [b Flonum] [x Flonum] [upper? Any] [regularized? Any]) Flonum]{}
@defproc[(fllog-beta-inc [a Flonum] [b Flonum] [x Flonum] [upper? Any] [regularized? Any]) Flonum]{
Flonum versions of the above functions. These return @racket[+nan.0] instead of raising errors and do
not have optional arguments. They can be a little faster to apply because they check fewer special
cases.
}

@(close-eval untyped-eval)
