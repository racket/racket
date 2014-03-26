#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base
                     racket/promise
                     racket/sequence
                     (except-in racket/list permutations) ; FIXME
                     math plot
                     (only-in typed/racket/base
                              ann inst
                              Flonum Real Boolean Any Listof Integer case-> -> U
                              Sequenceof Positive-Flonum Nonnegative-Flonum Symbol
                              HashTable Positive-Integer Nonnegative-Real Values
                              String))
          "utils.rkt")

@(define typed-eval (make-math-eval))
@interaction-eval[#:eval typed-eval (require)]

@title[#:tag "stats"]{Statistics Functions}
@(author-neil)

@defmodule[math/statistics]

This module exports functions that compute @deftech{statistics}, meaning summary values for
collections of samples, and functions for managing sequences of weighted or unweighted samples.

Most of the functions that compute statistics accept a sequence of nonnegative reals that
correspond one-to-one with sample values.
These are used as weights; equivalently counts, pseudocounts or unnormalized probabilities.
While this makes it easy to work with weighted samples, it introduces some subtleties
in bias correction.
In particular, central moments must be computed without bias correction by default.
See @secref{stats:expected-values} for a discussion.

@local-table-of-contents[]

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

@deftogether[(@defproc[(variance/mean [m Real]
                                      [xs (Sequenceof Real)]
                                      [ws (U #f (Sequenceof Real)) #f]
                                      [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real]
              @defproc[(stddev/mean [m Real]
                                    [xs (Sequenceof Real)]
                                    [ws (U #f (Sequenceof Real)) #f]
                                    [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real]
              @defproc[(skewness/mean [m Real]
                                      [xs (Sequenceof Real)]
                                      [ws (U #f (Sequenceof Real)) #f]
                                      [#:bias bias (U #t #f Real) #f])
                       Real]
              @defproc[(kurtosis/mean [m Real]
                                      [xs (Sequenceof Real)]
                                      [ws (U #f (Sequenceof Real)) #f]
                                      [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real])]{
Like @racket[variance], @racket[stddev], @racket[skewness] and @racket[kurtosis], but computed
using known mean @racket[m].
}

@section[#:tag "stats:running"]{Running Expected Values}

The @racket[statistics] object allows computing the sample minimum, maximum, count, mean, variance,
skewness, and excess kurtosis of a sequence of samples in O(1) space.

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
This function uses O(1) space regardless of the length of @racket[xs].
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

@deftogether[(@defproc[(covariance [xs (Sequenceof Real)]
                                   [ys (Sequenceof Real)]
                                   [ws (U #f (Sequenceof Real)) #f]
                                   [#:bias bias (U #t #f Real) #f])
                       Real]
              @defproc[(correlation [xs (Sequenceof Real)]
                                    [ys (Sequenceof Real)]
                                    [ws (U #f (Sequenceof Real)) #f]
                                    [#:bias bias (U #t #f Real) #f])
                       Real])]{
Compute the sample covariance and correlation of @racket[xs] and @racket[ys], optionally
weighted by @racket[ws].

@examples[#:eval typed-eval
                 (define xs (sample (normal-dist) 10000))
                 (define ys (map (λ: ([x : Real]) (sample (normal-dist x))) xs))
                 (correlation xs ys)]
Removing the correlation using importance weights:
@interaction[#:eval typed-eval
                    (define ws (map (λ: ([x : Real] [y : Real])
                                      (/ (pdf (normal-dist) y)
                                         (pdf (normal-dist x) y)))
                                    xs ys))
                    (correlation xs ys (ann ws (Sequenceof Real)))]

See @secref{stats:expected-values} for the meaning of the @racket[bias] keyword argument.
}

@deftogether[(@defproc[(covariance/means [mx Real]
                                         [my Real]
                                         [xs (Sequenceof Real)]
                                         [ys (Sequenceof Real)]
                                         [ws (U #f (Sequenceof Real)) #f]
                                         [#:bias bias (U #t #f Real) #f])
                       Real]
              @defproc[(correlation/means [mx Real]
                                          [my Real]
                                          [xs (Sequenceof Real)]
                                          [ys (Sequenceof Real)]
                                          [ws (U #f (Sequenceof Real)) #f]
                                          [#:bias bias (U #t #f Real) #f])
                       Real])]{
Like @racket[covariance] and @racket[correlation], but computed using known means
@racket[mx] and @racket[my].
}

@section{Counting and Binning}

@defproc*[([(samples->hash [xs (Sequenceof A)]) (HashTable A Positive-Integer)]
           [(samples->hash [xs (Sequenceof A)] [ws (U #f (Sequenceof Real))])
            (HashTable A Nonnegative-Real)])]{
Returns a hash table mapping each unique element in @racket[xs] (under @racket[equal?]) to its
count, or, if @racket[ws] is not @racket[#f], to its total weight.
@examples[#:eval typed-eval
                 (samples->hash '(1 2 3 4 4))
                 (samples->hash '(1 1 2 3 4) '(1/2 1/2 1 1 2))]
}

@defproc*[([(count-samples [xs (Sequenceof A)]) (Values (Listof A) (Listof Positive-Integer))]
           [(count-samples [xs (Sequenceof A)] [ws (U #f (Sequenceof Real))])
            (Values (Listof A) (Listof Nonnegative-Real))])]{
Like @racket[samples->hash], but returns two lists.
The elements in the returned @racket[(Listof A)] are in order of first appearance in @racket[xs].
@examples[#:eval typed-eval
                 (count-samples '(1 2 3 4 4))
                 (count-samples '(1 1 2 3 4) '(1/2 1/2 1 1 2))]
}

@defstruct*[sample-bin ([min B]
                        [max B]
                        [values (Listof A)]
                        [weights (U #f (Listof Nonnegative-Real))])]{
Represents a @italic{bin}, or a group of samples within an interval in a total order.
The values and bounds have a different type to allow @racket[bin-samples/key]
to group elements based on a function of their values.
}

@defproc[(bin-samples [bounds (Sequenceof A)]
                      [lte? (A A -> Any)]
                      [xs (Sequenceof A)]
                      [ws (U #f (Sequenceof Real))])
         (Listof (sample-bin A A))]{
Similar to @racket[(sort xs lte?)], but additionally groups samples into bins.
The bins' @racket[bounds] are sorted before binning @racket[xs].

If @racket[n = (length bounds)], then @racket[bin-samples] returns @italic{at least} @racket[(- n 1)]
bins, one for each pair of adjacent (sorted) bounds.
If some values in @racket[xs] are less than the smallest bound, they are grouped into a single bin in
front.
If some are greater than the largest bound, they are grouped into a single bin at the end.

@examples[#:eval typed-eval
                 (bin-samples '() <= '(0 1 2 3 4 5 6))
                 (bin-samples '(3) <= '(0 1 2 3 4 5 6))
                 (bin-samples '(2 4) <= '(0 1 2 3 4 5 6))
                 (bin-samples '(2 4) <=
                              '(0 1 2 3 4 5 6)
                              '(10 20 30 40 50 60 70))]

If @racket[lte?] is a less-than-or-equal relation, the bins represent half-open intervals
(@racket[min], @racket[max]] (except possibly the first, which may be closed).
If @racket[lte?] is a less-than relation, the bins represent half-open intervals
[@racket[min], @racket[max]) (except possibly the last, which may be closed).
In either case, the sorts applied to @racket[bounds] and @racket[xs] are stable.

Because intervals used in probability measurements are normally open on the left, prefer to use
less-than-or-equal relations for @racket[lte?].

If @racket[ws] is @racket[#f], @racket[bin-samples] returns bins with @racket[#f] weights.
}

@defproc[(bin-samples/key [bounds (Sequenceof B)]
                          [lte? (B B -> Any)]
                          [key (A -> B)]
                          [xs (Sequenceof A)]
                          [ws (U #f (Sequenceof Real))])
         (Listof (sample-bin A B))]{
Similar to @racket[(sort xs lte? #:key key #:cache-keys? #t)], but additionally groups samples into
bins.
@examples[#:eval typed-eval
                 (bin-samples/key '(2 4) <= (inst car Real String)
                                  '((1 . "1") (2 . "2") (3 . "3") (4 . "4") (5 . "5")))]
See @racket[bin-samples] for the simpler, one-type variant.
}

@defproc[(sample-bin-compact [bin (sample-bin A B)]) (sample-bin A B)]{
Compacts @racket[bin] by applying @racket[count-samples] to its values and weights.
@examples[#:eval typed-eval
                 (sample-bin-compact (sample-bin 1 4 '(1 2 3 4 4) #f))]
}

@defproc[(sample-bin-total [bin (sample-bin A B)]) Nonnegative-Real]{
If @racket[(sample-bin-weights bin)] is @racket[#f], returns the number of samples in @racket[bin];
otherwise, returns the sum of their weights.
@examples[#:eval typed-eval
                 (sample-bin-total (sample-bin 1 4 '(1 2 3 4 4) #f))
                 (sample-bin-total (sample-bin-compact (sample-bin 1 4 '(1 2 3 4 4) #f)))]
}

@section{Order Statistics}

@defproc*[([(sort-samples [lt? (A A -> Any)] [xs (Sequenceof A)]) (Listof A)]
           [(sort-samples [lt? (A A -> Any)]
                          [xs (Sequenceof A)]
                          [ws (U #f (Sequenceof Real))])
            (Values (Listof A) (Listof Nonnegative-Real))])]{
Sorts possibly weighted samples according to @racket[lt?], which is assumed to define a total
order over the elements in @racket[xs].
@examples[#:eval typed-eval
                 (sort-samples < '(5 2 3 1))
                 (sort-samples < '(5 2 3 1) '(50 20 30 10))
                 (sort-samples < '(5 2 3 1) #f)]
Because @racket[sort-samples] is defined in terms of @racket[sort], the sort is only guaranteed
to be stable if @racket[lt?] is strictly a less-than relation.
}

@defproc[(median [lt? (A A -> Any)] [xs (Sequenceof A)] [ws (U #f (Sequenceof Real)) #f]) A]{
Equivalent to @racket[(quantile 1/2 lt? xs ws)].
}

@defproc[(quantile [p Real]
                   [lt? (A A -> Any)]
                   [xs (Sequenceof A)]
                   [ws (U #f (Sequenceof Real)) #f])
         A]{
Computes the inverse of the empirical @tech{cdf} represented by the samples @racket[xs],
which are optionally weighted by @racket[ws].

@examples[#:eval typed-eval
                 (quantile 0 < '(1 3 5))
                 (quantile 0.5 < '(1 2 3 4))
                 (quantile 0.5 < '(1 2 3 4) '(0.25 0.20 0.20 0.35))]

If @racket[p = 0], @racket[quantile] returns the smallest element of @racket[xs] under the
ordering relation @racket[lt?]. If @racket[p = 1], it returns the largest element.

For weighted samples, @racket[quantile] sorts @racket[xs] and @racket[ws] together
(using @racket[sort-samples]), then finds the least @racket[x] for which the proportion of its
cumulative weight is greater than or equal to @racket[p].

For unweighted samples, @racket[quantile] uses the quickselect algorithm to find the element that
would be at index @racket[(ceiling (- (* p n) 1))] if @racket[xs] were sorted, where @racket[n]
is the length of @racket[xs].
}

@defproc[(absdev [xs (Sequenceof Real)] [ws (U #f (Sequenceof Real)) #f]) Nonnegative-Real]{
Computes the average absolute difference between the elements in @racket[xs] and
@racket[(median < xs ws)]. If @racket[ws] is not @racket[#f], it is a weighted average.
}

@defproc[(absdev/median [median Real] [xs (Sequenceof Real)] [ws (U #f (Sequenceof Real)) #f])
         Nonnegative-Real]{
Like @racket[(absdev xs ws)], but computed using known median @racket[median].
}

@deftogether[(@defproc[(hpd-interval [lt? (A A -> Any)]
                                     [δ (A A -> Real)]
                                     [p Real]
                                     [xs (Sequenceof A)]
                                     [ws (U #f (Sequenceof Real)) #f])
                       (Values A A)]
              @defproc[(hpd-interval/sorted [δ (A A -> Real)]
                                            [p Real]
                                            [xs (Sequenceof A)]
                                            [ws (U #f (Sequenceof Real)) #f])
                       (Values A A)])]{
Estimates the smallest interval for which the distribution that produced @racket[xs] (optionally
weighted by @racket[ws]) assigns probability @racket[p], which must be positive.
The type @racket[A] represents an ordered metric space with order @racket[lt?] and metric @racket[δ].

To compute an HPD interval from sorted samples, use @racket[hpd-interval/sorted].

You almost certainly want to use @racket[real-hpd-interval] or @racket[real-hpd-interval/sorted]
instead, which are defined in terms of these.
}

@deftogether[(@defproc[(real-hpd-interval [p Real]
                                          [xs (Sequenceof Real)]
                                          [ws (U #f (Sequenceof Real)) #f])
                       (Values Real Real)]
              @defproc[(real-hpd-interval/sorted [p Real]
                                                 [xs (Sequenceof Real)]
                                                 [ws (U #f (Sequenceof Real)) #f])
                       (Values Real Real)])]{
Equivalent to @racket[(hpd-interval < - p xs ws)] and @racket[(hpd-interval/sorted - p xs ws)].
@examples[#:eval typed-eval
                 (define beta32 (beta-dist 3 2))
                 (real-dist-hpd-interval beta32 0.8)
                 (real-hpd-interval 0.8 (sample beta32 10000))]
}

@section{Simulations}

The functions in this section support Monte Carlo simulation; for example, quantifying uncertainty
about statistics estimated from samples.

@deftogether[(@defproc[(mc-variance [xs (Sequenceof Real)]
                                    [ws (U #f (Sequenceof Real)) #f]
                                    [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real]
              @defproc[(mc-stddev [xs (Sequenceof Real)]
                                  [ws (U #f (Sequenceof Real)) #f]
                                  [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real])]{
Estimate the variance and standard deviation of expected values computed from random samples.

If @racket[xs] are random variable samples, then
@racketblock[(define θ (mean xs ws))]
is also a random variable sample.
These two values:
@racketblock[(mc-variance xs ws #:bias bias)
             (mc-stddev xs ws #:bias bias)]
estimate the variance and standard deviation of @racket[θ].
The latter is simply the square root of the variance, and bias correction is applied as described in
@secref{stats:expected-values}.


Two different ways to estimate the standard deviation of a mean computed from 1000 samples are
@interaction[#:eval typed-eval
                    (mc-stddev (sample (normal-dist 0 1) 1000))
                    (stddev (for/list : (Listof Real) ([_  (in-range 100)])
                              (mean (sample (normal-dist 0 1) 1000))))]
Using @racket[mc-stddev] is 100 times faster in this case, and in most statistical applications,
replicating a sampling process 100 times is infeasible.
}

@deftogether[(@defproc[(mc-stddev/mean [m Real]
                                       [xs (Sequenceof Real)]
                                       [ws (U #f (Sequenceof Real)) #f]
                                       [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real]
              @defproc[(mc-variance/mean [m Real]
                                         [xs (Sequenceof Real)]
                                         [ws (U #f (Sequenceof Real)) #f]
                                         [#:bias bias (U #t #f Real) #f])
                       Nonnegative-Real]
              )]{
Use these in the exceedingly rare cases in which you know the mean @racket[m] but are estimating
uncertainty in an estimate of the mean anyway.
}

@defproc[(indicator [pred? (A -> Any)]) (A -> (U 0 1))]{
Converts a predicate into an indicator function.
@examples[#:eval typed-eval                 
                 (fl (mean (map (indicator (λ ([x : Real]) (< -inf.0 x -1)))
                                (sample (normal-dist 0 1) 5000))))
                 (real-dist-prob (normal-dist 0 1) -inf.0 -1)]
}

@defproc[(mc-probability [pred? (A -> Any)]
                         [xs (Sequenceof A)]
                         [ws (U #f (Sequenceof Real)) #f])
         Nonnegative-Real]{
Estimates the probability of @racket[pred?] from possibly weighted samples.
Equivalent to @racket[(mean (sequence-map (indicator pred?) xs) ws)].
@examples[#:eval typed-eval
                 (fl (mc-probability (λ ([x : Real]) (< -inf.0 x -1))
                                     (sample (normal-dist 0 1) 5000)))]
}

@defproc[(mc-prob-dist [pred? (A -> Any)]
                       [xs (Sequenceof A)]
                       [ws (U #f (Sequenceof Real)) #f])
         Beta-Dist]{
Returns a beta distribution estimated from possibly weighted samples whose mean is
@racket[(mc-probability pred? xs ws)].

Computing a confidence interval for a probability whose endpoints are guaranteed to be
between @racket[0] and @racket[1]:
@interaction[#:eval typed-eval
                    (real-dist-hpd-interval
                     (mc-prob-dist (λ ([x : Real]) (< -inf.0 x -1))
                                   (sample (normal-dist 0 1) 5000))
                     0.95)]
}

@(close-eval typed-eval)
