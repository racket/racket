#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label math/flonum)
          "utils.rkt")

@(define the-eval (make-math-eval))
@(define untyped-eval (make-untyped-math-eval))

@title[#:tag "flonum"]{Flonums}

@defmodule[math/flonum]

For convenience, @racketmodname[math/flonum] re-exports @racketmodname[racket/flonum]
as well as providing the functions document below.

@section{Additional Flonum Functions}

@defproc[(fl [x Real]) Flonum]{
Equivalent to @racket[(real->double-flonum x)], but much easier to read and write.
@examples[#:eval the-eval
                 (fl 1/2)
                 (fl 0.5)
                 (fl 0.5f0)]
Note that @racket[exact->inexact] does not always convert a @racket[Real] to a @racket[Flonum]:
@interaction[#:eval the-eval
                    (exact->inexact 0.5f0)
                    (flabs (exact->inexact 0.5f0))]
You should prefer this function over @racket[exact->inexact], especially in Typed Racket code.
}

@deftogether[(@defproc[(flsgn [x Flonum]) Flonum]
              @defproc[(fleven? [x Flonum]) Boolean]
              @defproc[(flodd? [x Flonum]) Boolean])]{
Like @racket[sgn], @racket[even?] and @racket[odd?], but restricted to @racket[Flonum] input.
}

@defproc[(flhypot [x Flonum] [y Flonum]) Flonum]{
Computes @racket[(sqrt (+ (* x x) (* y y)))] in way that doesn't overflow unless the answer overflows.
@examples[#:eval the-eval
                 (sqrt (+ (* 1e200 1e200) (* 1e199 1e199)))
                 (flhypot 1e200 1e199)]
}

@defproc[(flprobability? [x Flonum] [log? Flonum #f]) Boolean]{
When @racket[log?] is @racket[#f], returns @racket[#t] when @racket[(<= 0.0 x 1.0)].
When @racket[log?] is @racket[#t], returns @racket[#t] when @racket[(<= -inf.0 x 0.0)].
@examples[#:eval the-eval
                 (flprobability? -0.1)
                 (flprobability? 0.5)
                 (flprobability? +nan.0 #t)]
}

@;{ (: fllog/base (Float Float -> Float)) }

@section{Debugging Flonum Operations}

The following functions and constants are useful in authoring and debugging flonum functions
that must be well-behaved and precise on the largest possible domain.

Suppose we approximate @racket[exp] using its Taylor series centered at @racket[1.0], truncated
after three terms (a second-order polynomial):
@racketblock+eval[#:eval untyped-eval
(define (exp-taylor-1 x)
  (let ([x  (- x 1.0)])
    (* (exp 1.0) (+ 1.0 x (* 0.5 x x)))))
]

We can use @racketmodname[plot] and @racket[flstep] (documented below) to compare its output
to that of @racket[exp] on very small intervals:
@interaction[#:eval untyped-eval
(plot (list (function exp-taylor-1 #:label "exp-taylor-1 x")
            (function exp #:color 2 #:label "exp x"))
      #:x-min (flstep 1.00002 -40)
      #:x-max (flstep 1.00002 40)
      #:width 480)
]
This is especially useful at a boundary between two different approximation methods.

For larger intervals, assuming the approximated function is fairly smooth,
we can get a better idea how well the approximation is doing using @racket[relative-error]:
@interaction[#:eval untyped-eval
(plot (function (λ (x) (relative-error (exp-taylor-1 x) (exp x))))
      #:x-min 0.99998
      #:x-max 1.00002
      #:y-label "Relative error")
]
We can infer from this plot that our Taylor series approximation is has close to
rounding error (no more than @racket[(* 0.5 epsilon.0)], about @racket[1.11e-16])
near @racket[1.0], but quickly becomes worse farther away.

To get a ground-truth function such as @racket[exp] to test against, either compute the
outputs as precisely as possible using a slow algorithm, or compute the outputs using
high-precision @racket[Bigfloat]s.

@subsection{Measuring Floating-Point Error}

@defproc[(relative-error [x Real] [r Real]) Flonum]{
TODO

A flonum function with relative error no greater than @racket[(* 0.5 epsilon.0)]
exhibits only rounding error; it is @italic{correct}. A flonum function with relative
error no greater than a few epsilons is @italic{very good}.
}

@defproc[(flulp [x Flonum]) Flonum]{
}

@defproc[(flulp-error [x Flonum] [r Real]) Flonum]{
}

@subsection{Flonum Constants}

@deftogether[(@defthing[-max.0 Flonum]
              @defthing[-min.0 Flonum]
              @defthing[+min.0 Flonum]
              @defthing[+max.0 Flonum])]{
The rational flonums with maximum and minimum magnitude.
@examples[#:eval the-eval (list -max.0 +max.0 -min.0 +min.0)]
}

@defthing[epsilon.0 Flonum]{
The difference between @racket[(flnext 1.0)] and @racket[1.0], or the smallest flonum
that can be added to @racket[1.0] to yield a larger number.
@examples[#:eval the-eval epsilon.0
                 (* 0.5 epsilon.0)]
}

@subsection{Low-Level Flonum Operations}

@defproc[(flonum->bit-field [x flonum?]) (integer-in 0 (- (expt 2 64) 1))]{
Returns the bits comprising @racket[x] as an integer.
A convenient shortcut for composing @racket[integer-bytes->integer] with
@racket[real->floating-point-bytes].
@examples[#:eval the-eval
                 (number->string (flonum->bit-field -inf.0) 16)
                 (number->string (flonum->bit-field +inf.0) 16)
                 (number->string (flonum->bit-field -0.0) 16)
                 (number->string (flonum->bit-field 0.0) 16)
                 (number->string (flonum->bit-field -1.0) 16)
                 (number->string (flonum->bit-field 1.0) 16)
                 (number->string (flonum->bit-field +nan.0) 16)]
}

@defproc[(bit-field->flonum [i (integer-in 0 (- (expt 2 64) 1))]) flonum?]{
The inverse of @racket[flonum->bit-field].
}

@defproc[(flonum->ordinal [x flonum?]) (integer-in (- (- (expt 2 63) 1)) (- (expt 2 63) 1))]{
Returns the signed ordinal index of @racket[x] in a total order over flonums.

When inputs are not @racket[+nan.0], this function is monotone and symmetric;
i.e. if @racket[(fl<= x y)] then @racket[(<= (flonum->ordinal x) (flonum->ordinal y))],
and @racket[(= (flonum->ordinal (- x)) (- (flonum->ordinal x)))].
@examples[#:eval the-eval
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

@defproc[(ordinal->flonum [i (integer-in (- (- (expt 2 63) 1)) (- (expt 2 63) 1))]) flonum?]{
The inverse of @racket[flonum->ordinal].
}

@defproc[(flonums-between [x flonum?] [y flonum?]) exact-integer?]{
Returns the number of flonums between @racket[x] and @racket[y], excluding one endpoint.
Equivalent to @racket[(- (flonum->ordinal y) (flonum->ordinal x))].
@examples[#:eval the-eval
                 (flonums-between 0.0 1.0)
                 (flonums-between 1.0 2.0)
                 (flonums-between 2.0 3.0)
                 (flonums-between 1.0 +inf.0)]
}

@defproc[(flstep [x flonum?] [n exact-integer?]) flonum?]{
Returns the flonum @racket[n] flonums away from @racket[x], according to @racket[flonum->ordinal].
If @racket[x] is @racket[+nan.0], returns @racket[+nan.0].
@examples[#:eval the-eval
                 (flstep 0.0 1)
                 (flstep (flstep 0.0 1) -1)
                 (flstep 0.0 -1)
                 (flstep +inf.0 1)
                 (flstep +inf.0 -1)
                 (flstep -inf.0 -1)
                 (flstep -inf.0 1)
                 (flstep +nan.0 1000)]
}

@defproc[(flnext [x flonum?]) flonum?]{
Equivalent to @racket[(flstep x 1)].
}

@defproc[(flprev [x flonum?]) flonum?]{
Equivalent to @racket[(flstep x -1)].
}

@subsection{Subnormal Flonums}

@defproc[(flsubnormal? [x Flonum]) Boolean]{
Returns @racket[#t] when @racket[x] is a
@hyperlink["http://en.wikipedia.org/wiki/Denormal_number"]{subnormal number}.

Though floating-point operations on subnormal numbers are still often implemented
in software, the situation is improving. Robust floating-point functions should
handle subnormal inputs correctly.
}

@deftogether[(@defthing[-min-normal.0 Flonum]
              @defthing[-max-subnormal.0 Flonum]
              @defthing[+max-subnormal.0 Flonum]
              @defthing[+min-normal.0 Flonum])]{
The minimum normal and maximum subnormal flonums. A flonum @racket[x] is subnormal when
it is not zero and @racket[((abs x) . < . +min-normal.0)].
}

@(close-eval the-eval)
@(close-eval untyped-eval)
