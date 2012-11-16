#lang scribble/doc
@(require "mz.rkt" (for-label racket/set))

@title[#:tag "sets"]{Sets}
@(define set-eval (make-base-eval))
@(interaction-eval #:eval set-eval (require racket/set))

A @deftech{set} represents a set of distinct elements. For a given
set, elements are equivalent via @racket[equal?], @racket[eqv?], or
@racket[eq?]. Two sets are @racket[equal?] when they use the same
element-comparison procedure (@racket[equal?], @racket[eqv?], or
@racket[eq?]) and have equivalent elements.

A set can be used as a @tech{stream} (see @secref["streams"]) and thus
as a single-valued @tech{sequence} (see @secref["sequences"]). The
elements of the set serve as elements of the stream or sequence. See
also @racket[in-set].

Operations on sets that contain elements that are mutated are
unpredictable in much the same way that @tech{hash table} operations are
unpredictable when keys are mutated.

@note-lib[racket/set]

@deftogether[(
@defproc[(set [v any/c] ...) set?]
@defproc[(seteqv [v any/c] ...) set?]
@defproc[(seteq [v any/c] ...) set?]
)]{

Creates a set that uses @racket[equal?], @racket[eqv?], or
@racket[eq?], respectively, to compare elements.  The given
@racket[v]s are added to the set. The elements are added in the order
that they appear as @racket[v]s, so in the first two cases, an earlier
element that is @racket[equal?] or @racket[eqv?] but not @racket[eq?]
to a later element takes precedence over the later element.}


@defproc[(set-empty? [st set?]) boolean?]{

Returns @racket[#t] if @racket[st] has no members, @racket[#f]
otherwise.}

@defproc[(set-count [st set?]) exact-nonnegative-integer?]{

Returns the number of elements in @racket[st].}

@defproc[(set-member? [st set?] [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is in @racket[st], @racket[#f]
otherwise.}


@defproc[(set-first [st (and/c set? (not/c set-empty?))]) any/c]{

Produces an unspecified element of @racket[st]. Multiple uses of
@racket[set-first] on @racket[st] produce the same result.}


@defproc[(set-rest [st (and/c set? (not/c set-empty?))]) set?]{

Produces a set that includes all elements of @racket[st] except
@racket[(set-first st)].}


@defproc[(set-add [st set?] [v any/c]) set?]{

@margin-note{Like operations on immutable hash tables, ``constant
time'' set operations actually require @math{O(log N)} time for a set
of size @math{N}.}

Produces a set that includes @racket[v] plus all elements of
@racket[st]. This operation runs in constant time.}


@defproc[(set-remove [st set?] [v any/c]) set?]{

Produces a set that includes all elements of @racket[st] except
@racket[v]. This operation runs in constant time.}


@defproc[(set-union [st set?] ...+) set?]{

Produces a set that includes all elements of all given @racket[st]s,
which must all use the same equivalence predicate (@racket[equal?],
@racket[eq?], or @racket[eqv?]). This operation runs in time
proportional to the total size of all given @racket[st]s except for
the largest.

At least one set must be provided to @racket[set-union] even though
mathematically @racket[set-union] could accept zero arguments. Since
there are multiple types of sets (@racket[eq?], @racket[eqv?], and 
@racket[equal?]) there is no obvious choice for a default empty set
to be returned. If there is a case where @racket[set-union] may be
applied to zero arguments, instead pass an empty set of the type
you desire.

@examples[#:eval set-eval
(set-union (set))
(set-union (seteq))
(set-union (set 1) (set 2))
(set-union (set 1) (seteq 2)) (code:comment "Sets of different types cannot be unioned")
]}


@defproc[(set-intersect [st set?] ...+) set?]{

Produces a set that includes only the elements in all of the given
@racket[st]s, which must all use the same equivalence predicate
(@racket[equal?], @racket[eq?], or @racket[eqv?]). This operation
runs in time proportional to the total size of all given
@racket[st]s except for the largest.}


@defproc[(set-subtract [st set?] ...+) set?]{

Produces a set that includes all elements the first @racket[st]s that
are not present in any of the other given @racket[sts]s.  All of the
given @racket[st]s must use the same equivalence predicate
(@racket[equal?], @racket[eq?], or @racket[eqv?]).  This operation
runs in time proportional to the total size of all given
@racket[st]s except the first one.}


@defproc[(set-symmetric-difference [st set?] ...+) set?]{

Produces a set containing only those elements found in each
@racket[st] an odd number of times. All of the given @racket[st]s must
use the same equivalence predicate (@racket[equal?], @racket[eq?], or
@racket[eqv?]). This operation runs in time proportional to the total
size of all given @racket[st]s except the first one.

@examples[#:eval set-eval
(set-symmetric-difference (set 1) (set 1 2) (set 1 2 3))
]}


@defproc[(set=? [st set?] [st2 set?]) boolean?]{

Returns @racket[#t] if @racket[st] and @racket[st2] contain the same
members, @racket[#f] otherwise. The @racket[st] and @racket[st2] must
use the same equivalence predicate (@racket[equal?], @racket[eq?], or
@racket[eqv?]).  This operation runs in time proportional to the size
of @racket[st].

Equivalent to @racket[(equal? st st2)].

@examples[#:eval set-eval
(set=? (set 1) (set 1 2 3))
(set=? (set 1 2 3) (set 1))
(set=? (set 1 2 3) (set 1 2 3))
]}

@defproc[(subset? [st set?] [st2 set?]) boolean?]{

Returns @racket[#t] if every member of @racket[st] is in
@racket[st2], @racket[#f] otherwise. The @racket[st] and
@racket[st2] must use the same equivalence predicate
(@racket[equal?], @racket[eq?], or @racket[eqv?]).  This operation
runs in time proportional to the size of @racket[st].

@examples[#:eval set-eval
(subset? (set 1) (set 1 2 3))
(subset? (set 1 2 3) (set 1))
(subset? (set 1 2 3) (set 1 2 3))
]}


@defproc[(proper-subset? [st set?] [st2 set?]) boolean?]{

Returns @racket[#t] if every member of @racket[st] is in @racket[st2]
and there is some member of @racket[st2] that is not a member of
@racket[st], @racket[#f] otherwise. The @racket[st] and @racket[st2]
must use the same equivalence predicate (@racket[equal?],
@racket[eq?], or @racket[eqv?]).  This operation runs in time
proportional to the size of @racket[st].

@examples[#:eval set-eval
(proper-subset? (set 1) (set 1 2 3))
(proper-subset? (set 1 2 3) (set 1))
(proper-subset? (set 1 2 3) (set 1 2 3))
]}


@defproc[(set-map [st set?]
                  [proc (any/c . -> . any/c)])
         (listof any/c)]{

Applies the procedure @racket[proc] to each element in
@racket[st] in an unspecified order, accumulating the results
into a list.}


@defproc[(set-for-each [st set?]
                       [proc (any/c . -> . any)])
         void?]{

Applies @racket[proc] to each element in @racket[st] (for the
side-effects of @racket[proc]) in an unspecified order.}

               
@defproc[(set? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{set}, @racket[#f]
otherwise.}

@defproc[(set-equal? [st set?]) boolean?]{

Returns @racket[#t] if @racket[st] compares elements with @racket[equal?],
@racket[#f] if it compares with @racket[eqv?] or @racket[eq?].}


@defproc[(set-eqv? [st set?]) boolean?]{

Returns @racket[#t] if @racket[st] compares elements with @racket[eqv?],
@racket[#f] if it compares with @racket[equal?] or @racket[eq?].}

@defproc[(set-eq? [st set?]) boolean?]{

Returns @racket[#t] if @racket[st] compares elements with @racket[eq?],
@racket[#f] if it compares with @racket[equal?] or @racket[eqv?].}

@defproc[(set/c [contract chaperone-contract?] [#:cmp cmp (or/c 'dont-care 'equal 'eqv 'eq) 'dont-care]) contract?]{
  Constructs a contract that recognizes sets whose elements match @racket[contract].

  If @racket[cmp] is @racket['dont-care], then the equality notion of the set is not considered
  when checking the contract. Otherwise, the contract accepts only sets with the corresponding
  notion of equality.
  
  If @racket[cmp] is @racket['eq] or @racket['eqv], then @racket[contract] must be a flat contract.
  If @racket[contract] is not a flat contract, then @racket[cmp] cannot be @racket['eq] or @racket['eqv]
  and the resulting contract can only be applied to sets that use @racket[equal?] for equality.
}

@defproc[(in-set [st set?]) sequence?]{

Explicitly converts a set to a sequence for use with @racket[for] and
other forms.}

@deftogether[(
@defform[(for/set (for-clause ...) body ...+)]
@defform[(for/seteq (for-clause ...) body ...+)]
@defform[(for/seteqv (for-clause ...) body ...+)]
@defform[(for*/set (for-clause ...) body ...+)]
@defform[(for*/seteq (for-clause ...) body ...+)]
@defform[(for*/seteqv (for-clause ...) body ...+)]
)]{

Analogous to @racket[for/list] and @racket[for*/list], but to
construct a set instead of a list.}


@deftogether[(
@defproc[(list->set [lst list?]) set?]
@defproc[(list->seteq [lst list?]) set?]
@defproc[(list->seteqv [lst list?]) set?]
)]{

Produces the appropriate type of set containing the elements of the
given list. Equivalent to @racket[(apply set lst)], @racket[(apply
seteq lst)], and @racket[(apply seteqv lst)], respectively.
}

@defproc[(set->list [st set?]) list?]{

Produces a list containing the elements of @racket[st].}


@close-eval[set-eval]

@(define i #'set)
@(define i2 #'set-union)



