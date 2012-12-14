#lang scribble/manual

@(require scribble/eval
          (for-label data/integer-set
                     racket/contract
                     (except-in racket/base
                                foldr)))
@(define (racket-tech pre)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") pre))

@title[#:tag "integer-set"]{Integer Sets}

@(define the-eval (make-base-eval))
@(the-eval '(require data/integer-set))

@defmodule[data/integer-set]

This library provides functions for
working with finite sets of integers.  This module is designed for
sets that are compactly represented as groups of intervals, even when
their cardinality is large.  For example, the set of integers from
@math{-1000000} to @math{1000000} except for @math{0}, can be represented as
@math{{[-1000000, -1], [1, 1000000]}}.  This data structure would not be
a good choice for the set of all odd integers between @math{0} and
@math{1000000}, which would be @math{{[1, 1], [3, 3], ... [999999,
999999]}}.

In addition to the @defterm{integer set} abstract type, a
@defterm{well-formed set} is a list of pairs of exact integers, where
each pair represents a closed range of integers, and the entire set is
the union of the ranges.  The ranges must be disjoint and increasing.
Further, adjacent ranges must have at least one integer between them.
For example: @racket['((-1 . 2) (4 . 10))] is a well-formed-set as is
@racket['((1 . 1) (3 . 3))], but @racket['((1 . 5) (6 . 7))],
@racket['((1 . 5) (-3 . -1))], @racket['((5 . 1))], and @racket['((1
. 5) (3 . 6))] are not.

An integer set implements the @racket-tech{stream} and
@racket-tech{sequence} generic interfaces.

@defproc[(make-integer-set [wfs well-formed-set?]) integer-set?]{

Creates an integer set from a well-formed set.}


@defproc[(integer-set-contents [s integer-set?]) well-formed-set?]{

Produces a well-formed set from an integer set.}


@defproc[(set-integer-set-contents! [s integer-set?][wfs well-formed-set?]) void?]{

Mutates an integer set.}


@defproc[(integer-set? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an integer set, @racket[#f]
otherwise.}

@defproc[(well-formed-set? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a well-formed set, @racket[#f]
otherwise.}

@defproc*[([(make-range) integer-set?]
           [(make-range [elem exact-integer?]) integer-set?]
           [(make-range [start exact-integer?]
                        [end exact-integer?]) integer-set?])]{

Produces, respectively, an empty integer set, an integer set
containing only @racket[elem], or an integer set containing the
integers from @racket[start] to @racket[end] inclusive, where
@racket[(start . <= . end)].}


@defproc[(intersect [x integer-set?][y integer-set?]) integer-set?]{

Returns the intersection of the given sets.}


@defproc[(subtract [x integer-set?][y integer-set?]) integer-set?]{

Returns the difference of the given sets (i.e., elements in @racket[x]
that are not in @racket[y]).}


@defproc[(union [x integer-set?][y integer-set?]) integer-set?]{

Returns the union of the given sets.}


@defproc[(split [x integer-set?][y integer-set?])
         (values integer-set? integer-set? integer-set?)]{

Produces three values: the first is the intersection of @racket[x] and
@racket[y], the second is the difference @racket[x] remove @racket[y],
and the third is the difference @racket[y] remove @racket[x].}


@defproc[(complement [s integer-set?]
                     [start exact-integer?]
                     [end exact-integer?]) integer-set?]{

Returns a set containing the elements between @racket[start] to
@racket[end] inclusive that are not in @racket[s], where
@racket[(start-k . <= . end-k)].}


@defproc[(symmetric-difference [x integer-set?][y integer-set?]) integer-set?]{

Returns an integer set containing every member of @racket[x]
and @racket[y] that is not in both sets.}


@defproc[(member? [k exact-integer?][s integer-set?]) boolean?]{

Returns @racket[#t] if @racket[k] is in @racket[s], @racket[#f]
otherwise.}


@defproc[(get-integer [set integer-set?]) (or/c exact-integer? #f)]{

Returns a member of @racket[set], or @racket[#f] if @racket[set] is empty.}


@defproc[(foldr [proc (exact-integer? any/c . -> . any/c)]
                [base-v any/c]
                [s integer-set?])
         any/c]{

Applies @racket[proc] to each member of @racket[s] in ascending order,
where the first argument to @racket[proc] is the set member, and the
second argument is the fold result starting with @racket[base-v]. For
example, @racket[(foldr cons null s)] returns a list of all the
integers in @racket[s], sorted in increasing order.}


@defproc[(partition [s (listof integer-set?)]) (listof integer-set?)]{

Returns the coarsest refinement of the sets in @racket[s] such that
the sets in the result list are pairwise disjoint.  For example,
partitioning the sets that represent @racket['((1 . 2) (5 . 10))] and
@racket['((2 . 2) (6 . 6) (12 . 12))] produces the a list containing
the sets for @racket['((1 . 1) (5 . 5) (7 . 10))] @racket['((2 . 2) (6
. 6))], and @racket['((12 . 12))].}


@defproc[(count [s integer-set?]) exact-nonnegative-integer?]{

Returns the number of integers in the given integer set.}


@defproc[(subset? [x integer-set?][y integer-set?]) boolean?]{

Returns true if every integer in @racket[x] is also in
@racket[y], otherwise @racket[#f].}


@close-eval[the-eval]
