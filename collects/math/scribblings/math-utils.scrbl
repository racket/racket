#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/future
                     math
                     (only-in typed/racket/base
                              Real Boolean Integer Natural Number Listof
                              Positive-Flonum Float-Complex Any List Positive-Integer))
          "utils.rkt")

@(define untyped-eval (make-untyped-math-eval))

@title[#:tag "utils"]{Stuff That Doesn't Belong Anywhere Else}
@(author-neil)

@defmodule[math/utils]

@defparam[max-math-threads num Positive-Integer]{
The maximum number of threads a parallelized @racketmodname[math] function
will use. The default value is @racket[(max 1 (processor-count))].
}

@defparam[dft-convention lst (List Real Real)]{
A parameter controlling the convention used for scaling discrete Fourier transforms, such as those
performed by @racket[array-fft]. The default value is @racket['(1 -1)], which represents the convention
used in signal processing.

In general, if @racket[lst] is @racket[(list a b)] and @racket[n] is the length of a transformed
array axis or vector, then
@itemlist[@item{Each sum is scaled by @racket[(expt n (/ (- a 1) 2))].}
          @item{Each exponential in the sum has its argument scaled by @racket[b].}]
Conveniently, a Fourier transform with convention @racket[(list (- a) (- b))] is the inverse
of a Fourier transform with convention @racket[(list a b)].

See Mathematica's
@hyperlink["http://reference.wolfram.com/mathematica/tutorial/FourierTransforms.html"]{documentation
on @tt{Fourier}}, from which this excellent idea was stolen.
}

@defproc[(dft-inverse-convention) (List Real Real)]{
Returns the convention used for inverse Fourier transforms, given the current convention.
}

@(close-eval untyped-eval)
