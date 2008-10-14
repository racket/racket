#lang scribble/doc
@(require "mz.ss")

@title[#:tag "booleans"]{Booleans and Equality}

True and false @deftech{booleans} are represented by the values
@scheme[#t] and @scheme[#f], respectively, though operations that
depend a boolean value typically treat anything other than @scheme[#f]
as true.

See also: @scheme[and], @scheme[or], @scheme[andmap], @scheme[ormap].


@defproc[(boolean? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is @scheme[#t] or @scheme[#f],
@scheme[#f] otherwise.}


@defproc[(not [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is @scheme[#f], @scheme[#f] otherwise.
}


@defproc[(equal? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @scheme[equal?] if and only if they are @scheme[eqv?],
unless otherwise specified for a particular datatype.

Datatypes with further specification of @scheme[equal?] include
strings, byte strings, numbers, pairs, mutable pairs, vectors, hash
tables, and inspectable structures. In the last five cases, equality
is recursively defined; if both @scheme[v1] and @scheme[v2] contain
reference cycles, they are equal when the infinite unfoldings of the
values would be equal. See also @scheme[prop:equal+hash].}


@defproc[(eqv? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @scheme[eqv?] if and only if they are @scheme[eq?],
unless otherwise specified for a particular datatype.

The number and character datatypes are the only ones for which
@scheme[eqv?] differs from @scheme[eq?].}


@defproc[(eq? [v1 any/c] [v2 any/c]) boolean?]{

Return @scheme[#t] if @scheme[v1] and @scheme[v2] refer to the same
object, @scheme[#f] otherwise. See also @secref["model-eq"].}


@defproc[(immutable? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an immutable @tech{string},
@tech{byte string}, @tech{vector}, @tech{hash table}, or box,
@scheme[#f] otherwise.}

@defthing[prop:equal+hash struct-type-property?]{

A @tech{structure type property} (see @secref["structprops"]) that
supplies an equality predicate and hashing functions for a structure
type. The property value must be a list of three procedures:

@itemize{

 @item{@scheme[_equal-proc : (any/c any/c (any/c any/c . ->
        . boolean?)  . -> . any/c)] --- tests whether the first two
        arguments are equal, where both values are instances of the
        structure type to which the property is associated (or a
        subtype of the structure type).

        The third argument is an @scheme[equal?]  predicate to use for
        recursive equality checks; use the given predicate instead of
        @scheme[equal?] to ensure that data cycles are handled
        properly.

        The @scheme[_equal-proc] is called for a pair of structures
        only when they are not @scheme[eq?], and only when they both
        have a @scheme[prop:equal+hash] value inherited from the same
        structure type. With this strategy, the order in which
        @scheme[equal?] receives two structures does not matter. It
        also means that, by default, a structure sub-type inherits the
        equality predicate of its parent, if any.}

 @item{@scheme[_hash-proc : (any/c (any/c . -> . exact-integer?) . ->
       . exact-integer?)] --- computes a hash code for the given
       structure, like @scheme[equal-hash-code]. The first argument is
       an instance of the structure type (or one of its subtypes) to
       which the property is associated.

       The second argument is a @scheme[equal-hash-code]-like
       procedure to use for recursive hash-code computation; use the
       given procedure instead of @scheme[equal-hash-code] to ensure
       that data cycles are handled properly.}

 @item{@scheme[_hash2-proc : (any/c (any/c . -> . exact-integer?) . ->
       . exact-integer?)] --- computes a secondary hash code for the
       given structure. This procedure is like @scheme[_hash-proc],
       but analogous to @scheme[equal-secondary-hash-code].}

}

Take care to ensure that @scheme[_hash-proc] and @scheme[_hash2-proc]
are consistent with @scheme[_equal-proc]. Specifically,
@scheme[_hash-proc] and @scheme[_hash2-proc] should produce the same
value for any two structures for which @scheme[_equal-proc] produces a
true value.

When a structure type has no @scheme[prop:equal+hash] property, then
transparent structures (i.e., structures with an @tech{inspector} that
is controlled by the current @tech{inspector}) are @scheme[equal?]
when they are instances of the same structure type (not counting
sub-types), and when they have @scheme[equal?] field values.  For
transparent structures, @scheme[equal-hash-code] and
@scheme[equal-secondary-hash-code] derive hash code using the field
values. For opaque structure types, @scheme[equal?] is the same as
@scheme[eq?], and @scheme[equal-hash-code] and
@scheme[equal-secondary-hash-code] results are based only on
@scheme[eq-hash-code].}

@section{Boolean Synonyms}

@note-lib[scheme/bool]

@defthing[true boolean?]{A synonym for @scheme[#t].}

@defthing[false boolean?]{A synonym for @scheme[#f].}

@defproc[(symbol=? [a symbol?] [b symbol?]) boolean?]{

Returns @scheme[(equal? a b)].}

@defproc[(boolean=? [a boolean?] [b boolean?]) boolean?]{

Returns @scheme[(equal? a b)].}

@defproc[(false? [v any/c]) boolean?]{

Returns @scheme[(not v)].}
