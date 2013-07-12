#lang scribble/manual

@(require scribble/eval
          unstable/scribblings/utils
          (for-label racket unstable/flonum racket/flonum))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/flonum
                     (rename-in (except-in plot plot plot3d)
                                [plot-bitmap  plot]
                                [plot3d-bitmap  plot3d])))

@unstable-title[#:tag "flonum"]{Flonums}
@unstable[@author+email["Neil Toronto" "ntoronto@racket-lang.org"]]

@defmodule[unstable/flonum]

@defproc[(flonum->bit-field [x flonum?]) (integer-in 0 (- (expt 2 64) 1))]{
Returns the bits comprising @racket[x] as an integer.
A convenient shortcut for composing @racket[integer-bytes->integer] with @racket[real->floating-point-bytes].
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
These properties mean that @racket[flonum->ordinal] does not distinguish @racket[-0.0] and @racket[0.0].

The following plot demonstrates how the density of floating-point numbers decreases with magnitude:
@examples[#:eval the-eval
                 (parameterize ([y-axis-ticks? #f])
                   (plot (list (function (compose flonum->ordinal exact->inexact) 1/4 8)
                               (y-axis 1/2) (y-axis 1) (y-axis 2) (y-axis 4))))]
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
Returns the flonum @racket[n] flonums away from @racket[x], according to @racket[flonum->ordinal]. If @racket[x] is @racket[+nan.0], returns @racket[+nan.0].
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

@deftogether[(@defthing[-max.0 flonum?]
              @defthing[-min.0 flonum?]
              @defthing[+min.0 flonum?]
              @defthing[+max.0 flonum?])]{
The rational flonums with maximum and minimum magnitude.
@examples[#:eval the-eval
                 (list -max.0 +max.0 -min.0 +min.0)
                 (plot (function sqrt 0 (* 20 +min.0)))]
}

@(close-eval the-eval)
