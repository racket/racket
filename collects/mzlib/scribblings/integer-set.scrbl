#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/integer-set))

@mzlib[#:mode title integer-set]

The @schememodname[mzlib/integer-set] library provides functions for
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
For example: @scheme['((-1 . 2) (4 . 10))] is a well-formed-set as is
@scheme['((1 . 1) (3 . 3))], but @scheme['((1 . 5) (6 . 7))],
@scheme['((1 . 5) (-3 . -1))], @scheme['((5 . 1))], and @scheme['((1
. 5) (3 . 6))] are not.


@defproc[(make-integer-set [wfs well-formed-set?]) integer-set?]{

Creates an integer set from a well-formed set.}


@defproc[(integer-set-contents [s integer-set?]) well-formed-set?]{

Produces a well-formed set from an integer set.}


@defproc[(set-integer-set-contents! [s integer-set?][wfs well-formed-set?]) void?]{

Mutates an integer set.}


@defproc[(integer-set? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an integer set, @scheme[#f]
otherwise.}

@defproc[(well-formed-set? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a well-formed set, @scheme[#f]
otherwise.}

@defproc*[([(make-range) integer-set?]
           [(make-range [elem exact-integer?]) integer-set?]
           [(make-range [start exact-integer?]
                        [end exact-integer?]) integer-set?])]{

Produces, respectively, an empty integer set, an integer set
containing only @scheme[elem], or an integer set containing the
integers from @scheme[start] to @scheme[end] inclusive, where
@scheme[(start . <= . end)].}


@defproc[(intersect [x integer-set?][y integer-set?]) integer-set?]{

Returns the intersection of the given sets.}


@defproc[(difference [x integer-set?][y integer-set?]) integer-set?]{

Returns the difference of the given sets (i.e., elements in @scheme[x]
that are not in @scheme[y]).}


@defproc[(union [x integer-set?][y integer-set?]) integer-set?]{

Returns the union of the given sets.}


@defproc[(split [x integer-set?][y integer-set?]) integer-set?]{

Produces three values: the first is the intersection of @scheme[x] and
@scheme[y], the second is the difference @scheme[x] remove @scheme[y],
and the third is the difference @scheme[y] remove @scheme[x].}


@defproc[(complement [s integer-set?]
                     [start exact-integer?]
                     [end exact-integer?]) any]

Returns the a set containing the elements between @scheme[start] to
@scheme[end] inclusive that are not in @scheme[s], where
@scheme[(start-k . <= . end-k)].}


@defproc[(xor [x integer-set?][y integer-set?]) integer-set?]{

Returns an integer set containing every member of @scheme[x]
and @scheme[y] that is not in both sets.}


@defproc[(member? [k exact-integer?][s integer-set?]) boolean?]{

Returns @scheme[#t] if @scheme[k] is in @scheme[s], @scheme[#f]
otherwise.}


@defproc[(get-integer [integer-set any/c]) (or/c exact-integer? false/c)]{

Returns a member of @scheme[integer-set], or @scheme[#f] if
@scheme[integer-set] is empty.}


@defproc[(foldr [proc (exact-integer? any/c . -> . any/c)]
                [base-v any/c]
                [s integer-set?])
         any/c]{

Applies @scheme[proc] to each member of @scheme[s] in ascending order,
where the first argument to @scheme[proc] is the set member, and the
second argument is the fold result starting with @scheme[base-v]. For
example, @scheme[(foldr cons null s)] returns a list of all the
integers in @scheme[s], sorted in increasing order.}


@defproc[(partition [s integer-set-list?]) (listof integer-set?)]{

Returns the coarsest refinement of the sets in @scheme[s] such that
the sets in the result list are pairwise disjoint.  For example,
partitioning the sets that represent @scheme['((1 . 2) (5 . 10))] and
@scheme['((2 . 2) (6 . 6) (12 . 12))] produces the a list containing
the sets for @scheme['((1 . 1) (5 . 5) (7 . 10))] @scheme['((2 . 2) (6
. 6))], and @scheme['((12 . 12))].}


@defproc[(card [s integer-set?]) exact-nonnegative-integer?]{

Returns the number of integers in the given integer set.}


@defproc[(subset? [x integer-set?][y integer-set?]) boolean?]{

Returns true if every integer in @scheme[x] is also in
@scheme[y], otherwise @scheme[#f].}

