#lang scribble/doc
@(require "mz.ss"
          (for-label scheme/set))

@title[#:tag "sets"]{Sets}

A @deftech{set} represents a set of distinct elements. For a given
set, elements are equivalent via @scheme[equal?], @scheme[eqv?], or
@scheme[eq?]. Two sets are @scheme[equal?] when they use the same
element-comparison procedure (@scheme[equal?], @scheme[eqv?], or
@scheme[eq?]) and have equivalent elements. A set can be used as a
@tech{sequence} (see @secref["sequences"]).

Operations on sets that contain elements that are mutated are
unpredictable in much the same way that @tech{hash table} operations are
unpredictable when keys are mutated.


@note-lib-only[scheme/set]

@defproc[(set? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{set}, @scheme[#f]
otherwise.}

@defproc[(set-eqv? [set set?]) boolean?]{

Returns @scheme[#t] if @scheme[set] compares elements with @scheme[eqv?],
@scheme[#f] if it compares with @scheme[equal?] or @scheme[eq?].}

@defproc[(set-eq? [set set?]) boolean?]{

Returns @scheme[#t] if @scheme[set] compares elements with @scheme[eq?],
@scheme[#f] if it compares with @scheme[equal?] or @scheme[eqv?].}

@deftogether[(
@defproc[(set [v any/c] ...) set?]
@defproc[(seteqv [v any/c] ...) set?]
@defproc[(seteq [v any/c] ...) set?]
)]{

Creates a set that uses @scheme[equal?], @scheme[eq?], or
@scheme[eqv?], respectively, to compare elements.  The given
@scheme[v]s are added to the set. The elements are added in the order
that they appear as @scheme[v]s, so in the first two cases, an earlier
element that is @scheme[equal?] or @scheme[eqv?] but not @scheme[eq?]
to a later element takes precedence over the later element.}


@defproc[(set-empty? [set set?]) boolean?]{

Returns @scheme[#t] if @scheme[set] has no members, @scheme[#f]
otherwise.}

@defproc[(set-member? [set set?] [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is in @scheme[set], @scheme[#f]
otherwise.}

@defproc[(set-add [set set?] [v any/c]) set?]{

@margin-note{Like operations on immutable hash tables, ``constant
time'' set operations actually require @math{O(log N)} time for a set
of size @math{N}.}

Produces a set that includes @scheme[v] plus all elements of
@scheme[set]. This operation runs in constant time.}


@defproc[(set-remove [set set?] [v any/c]) set?]{

Produces a set that includes all elements of @scheme[set] except
@scheme[v]. This operation runs in constant time.}


@defproc[(set-union [set set?] ...+) set?]{

Produces a set that includes all elements of all given @scheme[set]s,
which must all use the same equivalence predicate (@scheme[equal?],
@scheme[eq?], or @scheme[eqv?]). This operation runs in time
proportional to the total size of all given @scheme[set]s except for
the largest.}


@defproc[(set-intersect [set set?] ...+) set?]{

Produces a set that includes only the elements in all of the given
@scheme[set]s, which must all use the same equivalence predicate
(@scheme[equal?], @scheme[eq?], or @scheme[eqv?]). This operation
runs in time proportional to the total size of all given
@scheme[set]s except for the largest.}


@defproc[(set-subtract [set set?] ...+) set?]{

Produces a set that includes all elements the first @scheme[set]s that
are not present in any of the other given @scheme[sets]s.  All of the
given @scheme[set]s must use the same equivalence predicate
(@scheme[equal?], @scheme[eq?], or @scheme[eqv?]).  This operation
runs in time proportional to the total size of all given
@scheme[set]s except the first one.}


@defproc[(set-map [set set?]
                  [proc (any/c . -> . any/c)])
         (listof any/c)]{

Applies the procedure @scheme[proc] to each element in
@scheme[set] in an unspecified order, accumulating the results
into a list.}

@defproc[(set-for-each [set set?]
                       [proc (any/c . -> . any)])
         void?]{

Applies @scheme[proc] to each element in @scheme[set] (for the
side-effects of @scheme[proc]) in an unspecified order.}

@defproc[(in-set [set set?]) sequence?]{

Explicitly converts a set to a sequence for use with @scheme[for] and
other forms.}
