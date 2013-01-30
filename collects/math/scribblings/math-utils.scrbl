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

@section[#:tag "utils:parallel"]{Parallelization}

@defparam[max-math-threads num Positive-Integer]{
The maximum number of threads a parallelized @racketmodname[math] function
will use. The default value is @racket[(max 1 (processor-count))].
}

@section[#:tag "utils:dft"]{Discrete Fourier Transform Conventions}

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

@section[#:tag "utils:fpu-test"]{Floating-Point Compliance Testing}

@defproc[(test-floating-point [n Natural]) (Listof (List Any Any))]{
Runs a comprehensive test of the system's IEEE 754 (floating-point) compliance, and reports
unexpected inaccuracies and errors.

In each test, a function is applied to some carefully chosen values, as well as @racket[n] additional
random values.
Its corresponding @tech{bigfloat} function is applied to the same values, and the answers are
compared.
Each test returns a list of failures, which are appended and returned.

Each failure in a failure list is formatted
@racketblock[(list (list name args ...) reason)]
where @racket[name] is the name of a function, such as @racket['fl+], @racket[args ...] are the
arguments it was applied to, and @racket[reason] is the reason for the failure.

If @racket[reason] is a flonum, the failure was due to inaccuracy. For example,
@racketblock[(list (list 'fl+ 4.5 2.3) 0.76)]
means the result of @racket[(fl+ 4.5 2.3)] was off by @racket[0.76] @tech{ulps}.

The threshold for reporting unexpected inaccuracy depends on the function tested.
All the arithmetic and irrational functions exported by @racketmodname[racket/flonum], for example,
must have no more than @racket[0.5] ulps error in order to be compliant.

Two other possible failure reasons are
@racketblock[(list 'different-zero 0.0 -0.0)
             (list 'different-zero -0.0 0.0)]
The first zero is the answer returned by the function, and the second zero is the expected answer.

Other possible failure reasons have the form
@racketblock[(list 'not-fl2? x y)]
meaning that the result @racket[(values x y)] is not a valid flonum expansion.
Such reasons are only given for failures of functions whose names begin with @tt{fl2} or contain
@tt{/error}.
These functions are currently undocumented, but are used to implement many
@racketmodname[math/flonum], @racketmodname[math/special-functions], and
@racketmodname[math/distributions] functions.

Tests of functions that operate on and return flonum expansions are the strictest tests, requiring
hardware arithmetic to be perfectly IEEE 754 compliant.
They reliably fail on seemingly innocuous noncompliant behavior, such as computing intermediate
results with 80-bit precision.
}

@defparam[print-fp-test-progress? print? Boolean]{
When @racket[(print-fp-test-progress?)] is @racket[#t], floating-point tests print and flush a
representation of their progress as they run. The default value is @racket[#t].
}

@(close-eval untyped-eval)
