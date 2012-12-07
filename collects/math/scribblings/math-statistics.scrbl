#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/promise racket/list
                     math plot
                     (only-in typed/racket/base
                              Flonum Real Boolean Any Listof Integer case-> -> U
                              Sequenceof Positive-Flonum Nonnegative-Flonum
                              Nonnegative-Real))
          "utils.rkt")

@(define typed-eval (make-math-eval))
@interaction-eval[#:eval typed-eval (require)]

@title[#:tag "stats"]{Statistics Functions}
@(author-neil)

@defmodule[math/statistics]

This module exports functions that compute summary values for collections of data, or
@deftech{statistics}, such as means, standard devations, medians, and @italic{k}th order
statistics. It also exports functions for managing collections of sample values.

Most of the functions that compute statistics also accept a sequence of nonnegative reals
that correspond one-to-one with the data values.
These are used as weights; equivalently counts, pseudocounts or proportions.
While this makes it easy to work with weighted samples, it introduces some subtleties
in bias correction.
In particular, central moments must be computed without bias correction by default.
See @secref{stats:expected-values} for a discussion.

@local-table-of-contents[]

@section{Counting}

@defthing[samples->hash Any]{
This stub represents forthcoming documentation.
}

@defthing[count-samples Any]{
This stub represents forthcoming documentation.
}

@section[#:tag "stats:expected-values"]{Expected Values}

Functions documented in this section that compute higher central moments, such as @racket[variance],
@racket[stddev] and @racket[skewness], can optionally apply bias correction to their estimates.
For example, when @racket[variance] is given the argument @racket[#:bias #t], it
multiplies the result by @racket[(/ n (- n 1))], where @racket[n] is the number of samples.

The meaning of ``bias correction'' becomes less clear with weighted samples, however. Often, the
weights represent counts, so when moment-estimating functions receive @racket[#:bias #t], they
interpret it as ``use the sum of @racket[ws] for @racket[n].''
In the following example, the sample @racket[4] is first counted twice and then given weight
@racket[2]; therefore @racket[n = 5] in both cases:
@interaction[#:eval typed-eval
                    (variance '(1 2 3 4 4) #:bias #t)
                    (variance '(1 2 3 4) '(1 1 1 2) #:bias #t)]

However, sample weights often do not represent counts. For these cases, the @racket[#:bias]
keyword can be followed by a real-valued pseudocount, which is used for @racket[n]:
@interaction[#:eval typed-eval
                    (variance '(1 2 3 4) '(1/2 1/2 1/2 1) #:bias 5)]

Because the magnitude of the bias correction for weighted samples cannot be known without user
guidance, in all cases, the bias argument defaults to @racket[#f].

@defproc[(mean [xs (Sequenceof Real)] [ws (U #f (Sequenceof Real)) #f]) Real]{
When @racket[ws] is @racket[#f] (the default), returns the sample mean of the values in @racket[xs].
Otherwise, returns the weighted sample mean of the values in @racket[xs] with corresponding weights
@racket[ws].
@examples[#:eval typed-eval
                 (mean '(1 2 3 4 5))
                 (mean '(1 2 3 4 5) '(1 1 1 1 10.0))
                 (define d (normal-dist))
                 (mean (sample d 10000))
                 (define arr (array-strict (build-array #(5 1000) (λ (_) (sample d)))))
                 (array-map mean (array->list-array arr 1))]
}

@deftogether[(@defproc[(variance [xs (Sequenceof Real)]
                                 [ws (U #f (Sequenceof Real)) #f]
                                 [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real]
              @defproc[(stddev [xs (Sequenceof Real)]
                               [ws (U #f (Sequenceof Real)) #f]
                               [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real]
              @defproc[(skewness [xs (Sequenceof Real)]
                                 [ws (U #f (Sequenceof Real)) #f]
                                 [#:bias bias (U #t #f Real) #f])
                       Real]
              @defproc[(kurtosis [xs (Sequenceof Real)]
                                 [ws (U #f (Sequenceof Real)) #f]
                                 [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real])]{
If @racket[ws] is @racket[#f], these compute the sample variance, standard deviation, skewness
and excess kurtosis the samples in @racket[xs].
If @racket[ws] is not @racket[#f], they compute weighted variations of the same.
@examples[#:eval typed-eval
                 (stddev '(1 2 3 4 5))
                 (stddev '(1 2 3 4 5) '(1 1 1 1 10))]
See @secref{stats:expected-values} for the meaning of the @racket[bias] keyword argument.
}

@deftogether[(@defproc[(variance/mean [mean Real]
                                      [xs (Sequenceof Real)]
                                      [ws (U #f (Sequenceof Real)) #f]
                                      [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real]
              @defproc[(stddev/mean [mean Real]
                                    [xs (Sequenceof Real)]
                                    [ws (U #f (Sequenceof Real)) #f]
                                    [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real]
              @defproc[(skewness/mean [mean Real]
                                      [xs (Sequenceof Real)]
                                      [ws (U #f (Sequenceof Real)) #f]
                                      [#:bias bias (U #t #f Real) #f])
                       Real]
              @defproc[(kurtosis/mean [mean Real]
                                      [xs (Sequenceof Real)]
                                      [ws (U #f (Sequenceof Real)) #f]
                                      [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real])]{
Like @racket[variance], @racket[stddev], @racket[skewness] and @racket[kurtosis], but computed
using known mean @racket[mean].
}

@section[#:tag "stats:running"]{Running Expected Values}

The @racket[statistics] object allows computing the sample minimum, maximum, count, mean, variance,
skewness, and excess kurtosis of any number of samples in O(1) space.

To use it, start with @racket[empty-statistics], then use @racket[update-statistics] to obtain a
new statistics object with updated values. Use @racket[statistics-min], @racket[statistics-mean],
and similar functions to get the current estimates.
@examples[#:eval typed-eval
                 (let* ([s  empty-statistics]
                        [s  (update-statistics s 1)]
                        [s  (update-statistics s 2)]
                        [s  (update-statistics s 3)]
                        [s  (update-statistics s 4 2)])
                   (values (statistics-mean s)
                           (statistics-stddev s #:bias #t)))]

@defstruct*[statistics ([min Flonum]
                        [max Flonum]
                        [count Nonnegative-Flonum])]{
Represents running statistics.

The @racket[min] and @racket[max] fields are the minimum and maximum
value observed so far, and the @racket[count] field is the total weight of the samples (which is the
number of samples if all samples are unweighted).
The remaining, hidden fields are used to compute moments, and their number and meaning may change in
future releases.
}

@defthing[empty-statistics statistics]{
The empty statistics object.
@examples[#:eval typed-eval
                 (statistics-min empty-statistics)
                 (statistics-max empty-statistics)
                 (statistics-range empty-statistics)
                 (statistics-count empty-statistics)
                 (statistics-mean empty-statistics)
                 (statistics-variance empty-statistics)
                 (statistics-skewness empty-statistics)
                 (statistics-kurtosis empty-statistics)]
}

@defproc[(update-statistics [s statistics] [x Real] [w Real 1.0]) statistics]{
Returns a new statistics object that includes @racket[x] in the computed statistics. If @racket[w]
is given, @racket[x] is weighted by @racket[w] in the moment computations.
}

@defproc[(update-statistics* [s statistics]
                             [xs (Sequenceof Real)]
                             [ws (U #f (Sequenceof Real)) #f])
         statistics]{
Like @racket[update-statistics], but includes all of @racket[xs], possibly weighted by corresponding
elements in @racket[ws], in the returned statistics object.
@examples[#:eval typed-eval
                 (define s (update-statistics* empty-statistics '(1 2 3 4) '(1 1 1 2)))
                 (statistics-mean s)
                 (statistics-stddev s #:bias #t)]
}

@deftogether[(@defproc[(statistics-range [s statistics]) Nonnegative-Flonum]
              @defproc[(statistics-mean [s statistics]) Flonum]
              @defproc[(statistics-variance [s statistics] [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Flonum]
              @defproc[(statistics-stddev [s statistics] [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Flonum]
              @defproc[(statistics-skewness [s statistics] [#:bias bias (U #t #f Real) #f])
                       Flonum]
              @defproc[(statistics-kurtosis [s statistics] [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Flonum])]{
Compute the range, mean, variance, standard deviation, skewness, and excess kurtosis of the
observations summarized in @racket[s].

See @secref{stats:expected-values} for the meaning of the @racket[bias] keyword argument.
}

@section{Correlation}

@section{Order Statistics}

@(close-eval typed-eval)
