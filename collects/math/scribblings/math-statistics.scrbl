#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/promise racket/list
                     math plot
                     (only-in typed/racket/base
                              Flonum Real Boolean Any Listof Integer case-> -> U
                              Sequenceof Positive-Flonum Nonnegative-Flonum
                              HashTable Positive-Integer Nonnegative-Real Values))
          "utils.rkt")

@(define typed-eval (make-math-eval))
@interaction-eval[#:eval typed-eval (require)]

@title[#:tag "stats"]{Statistics Functions}
@(author-neil)

@defmodule[math/statistics]

This module exports functions that compute summary values for collections of samples, or
@deftech{statistics}, such as means, standard devations, medians, and @italic{k}th order
statistics.
It also exports functions for managing collections of samples.

Most of the functions that compute statistics also accept a sequence of nonnegative reals
that correspond one-to-one with samples.
These are used as weights; equivalently counts, pseudocounts or proportions.
While this makes it easy to work with weighted samples, it introduces some subtleties
in bias correction.
In particular, central moments must be computed without bias correction by default.
See @secref{stats:expected-values} for a discussion.

@local-table-of-contents[]

@section{Counting and Binning}

@defproc*[([(samples->hash [xs (Sequenceof A)]) (HashTable A Positive-Integer)]
           [(samples->hash [xs (Sequenceof A)] [ws (U #f (Sequenceof Real))])
            (HashTable A Nonnegative-Real)])]{
@examples[#:eval typed-eval
                 (samples->hash '(1 2 3 4 4))
                 (samples->hash '(1 1 2 3 4) '(1/2 1/2 1 1 2))]
}

@defproc*[([(count-samples [xs (Sequenceof A)]) (Values (Listof A) (Listof Positive-Integer))]
           [(count-samples [xs (Sequenceof A)] [ws (U #f (Sequenceof Real))])
            (Values (Listof A) (Listof Nonnegative-Real))])]{
@examples[#:eval typed-eval
                 (count-samples '(1 2 3 4 4))
                 (count-samples '(1 1 2 3 4) '(1/2 1/2 1 1 2))]
}

@defstruct*[sample-bin ([values (Listof A)]
                        [weights (U #f (Listof Nonnegative-Real))]
                        [min B] [max B])]{
Represents a @italic{bin}, or a group of samples within an interval in a total order.
The values and bounds have a different type to allow @racket[bin-samples] and
@racket[bin-weighted-samples] to group elements based on a function of their values (a @racket[key],
like in @racket[sort]).
}

@defproc*[([(bin-samples [xs (Sequenceof A)] [bounds (Sequenceof A)] [lte? (A A -> Any)])
            (Listof (sample-bin A A))]
           [(bin-samples [xs (Sequenceof A)]
                         [bounds (Sequenceof B)]
                         [lte? (B B -> Any)]
                         [key (A -> B)])
            (Listof (sample-bin A B))])]{
Like @racket[(sort xs lte? #:key key)], but additionally groups samples into bins.
Keys are always cached, and @racket[bounds] is sorted before binning.

If @racket[n = (length bounds)], then @racket[bin-samples] returns @italic{at least} @racket[(- n 1)]
bins, one for each pair of adjacent (sorted) bounds.
If some values in @racket[xs] are less than the smallest bound, they are grouped into a single bin in
front.
If some are greater than the largest bound, they are grouped into a single bin at the end.

@examples[#:eval typed-eval
                 (bin-samples '(0 1 2 3 4 5 6) '() <=)
                 (bin-samples '(0 1 2 3 4 5 6) '(3) <=)
                 (bin-samples '(0 1 2 3 4 5 6) '(2 4) <=)]

Note that @racket[bin-samples] always returns bins with @racket[#f] weights, or bins containing
unweighted samples.
}

@defproc*[([(bin-weighted-samples [xs (Sequenceof A)]
                                  [ws (Sequenceof Real)]
                                  [bounds (Sequenceof A)]
                                  [lte? (A A -> Any)])
            (Listof (sample-bin A A))]
           [(bin-weighted-samples [xs (Sequenceof A)]
                                  [ws (Sequenceof Real)]
                                  [bounds (Sequenceof B)]
                                  [lte? (B B -> Any)]
                                  [key (A -> B)])
            (Listof (sample-bin A B))])]{
Like @racket[bin-samples], but for weighted samples.
}

@defproc[(sample-bin-compact [bin (sample-bin A B)]) (sample-bin A B)]{
Compacts @racket[bin] by applying @racket[count-samples] to its values and weights.
@examples[#:eval typed-eval
                 (sample-bin-compact (sample-bin '(1 2 3 4 4) #f 1 4))]
}

@defproc[(sample-bin-total [bin (sample-bin A B)]) Nonnegative-Real]{
If @racket[(sample-bin-weights bin)] is @racket[#f], returns the number of samples in @racket[bin];
otherwise, returns the sum of their weights.
@examples[#:eval typed-eval
                 (sample-bin-total (sample-bin '(1 2 3 4 4) #f 1 4))
                 (sample-bin-total (sample-bin-compact (sample-bin '(1 2 3 4 4) #f 1 4)))]
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
