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
@scheme[#f] otherwise.

@examples[
(boolean? #f)
(boolean? #t)
(boolean? 'true)
]}


@defproc[(not [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is @scheme[#f], @scheme[#f] otherwise.

@examples[
(not #f)
(not #t)
(not 'we-have-no-bananas)
]}


@defproc[(equal? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @scheme[equal?] if and only if they are @scheme[eqv?],
unless otherwise specified for a particular datatype.

Datatypes with further specification of @scheme[equal?] include
strings, byte strings, numbers, pairs, mutable pairs, vectors, boxes, hash
tables, and inspectable structures. In the last five cases, equality
is recursively defined; if both @scheme[v1] and @scheme[v2] contain
reference cycles, they are equal when the infinite unfoldings of the
values would be equal. See also @scheme[prop:equal+hash] and @racket[prop:proxy-of].

@examples[
(equal? 'yes 'yes)
(equal? 'yes 'no)
(equal? (expt 2 100) (expt 2 100))
(equal? 2 2.0)
(equal? (make-string 3 #\z) (make-string 3 #\z))
]}


@defproc[(eqv? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @scheme[eqv?] if and only if they are @scheme[eq?],
unless otherwise specified for a particular datatype.

The @tech{number} and @tech{character} datatypes are the only ones for which
@scheme[eqv?] differs from @scheme[eq?].

@examples[
(eqv? 'yes 'yes)
(eqv? 'yes 'no)
(eqv? (expt 2 100) (expt 2 100))
(eqv? 2 2.0)
(eqv? (integer->char #x3BB) (integer->char #x3BB))
(eqv? (make-string 3 #\z) (make-string 3 #\z))
]}


@defproc[(eq? [v1 any/c] [v2 any/c]) boolean?]{

Return @scheme[#t] if @scheme[v1] and @scheme[v2] refer to the same
object, @scheme[#f] otherwise. See also @secref["model-eq"].

@examples[
(eq? 'yes 'yes)
(eq? 'yes 'no)
(let ([v (mcons 1 2)]) (eq? v v))
(eq? (mcons 1 2) (mcons 1 2))
(eq? (make-string 3 #\z) (make-string 3 #\z))
]}


@defproc[(equal?/recur [v1 any/c] [v2 any/c] [recur-proc (any/c any/c -> any/c)]) boolean?]{

Like @scheme[equal?], but using @scheme[recur-proc] for recursive
comparisons (which means that reference cycles are not handled
automatically). Non-@scheme[#f] results from @scheme[recur-proc] are
converted to @scheme[#t] before being returned by
@scheme[equal?/recur].

@examples[
(equal?/recur 1 1 (lambda (a b) #f))
(equal?/recur '(1) '(1) (lambda (a b) #f))
(equal?/recur '#(1 1 1) '#(1 1.2 3/4)
              (lambda (a b) (<= (abs (- a b)) 0.25)))
]}


@defproc[(immutable? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an immutable @tech{string},
@tech{byte string}, @tech{vector}, @tech{hash table}, or box,
@scheme[#f] otherwise.

@examples[
(immutable? 'hello)
(immutable? "a string")
(immutable? (box 5))
(immutable? #(0 1 2 3))
(immutable? (make-hash))
(immutable? (make-immutable-hash '([a b])))
]}

@defthing[prop:equal+hash struct-type-property?]{

A @tech{structure type property} (see @secref["structprops"]) that
supplies an equality predicate and hashing functions for a structure
type. The property value must be a list of three procedures:

@itemize[

 @item{@scheme[_equal-proc : (any/c any/c (any/c any/c . ->
        . boolean?)  . -> . any/c)] --- tests whether the first two
        arguments are equal, where both values are instances of the
        structure type to which the property is associated (or a
        subtype of the structure type).

        The third argument is an @scheme[equal?]  predicate to use for
        recursive equality checks; use the given predicate instead of
        @scheme[equal?] to ensure that data cycles are handled
        properly and to work with @scheme[equal?/recur] (but beware
        that an arbitrary function can be provided to
        @scheme[equal?/recur] for recursive checks, which means that
        arguments provided to the predicate might be exposed to
        arbitrary code).

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

]

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
@scheme[eq-hash-code]. If a structure has a @racket[prop:proxy-of]
property, then the @racket[prop:proxy-of] property takes precedence over
@racket[prop:equal+hash] if the property value's procedure returns a
non-@racket[#f] value when applied to the structure.

@examples[
(define (farm=? farm1 farm2 recursive-equal?)
  (and (= (farm-apples farm1)
          (farm-apples farm2))
       (= (farm-oranges farm1)
          (farm-oranges farm2))
       (= (farm-sheep farm1)
          (farm-sheep farm2))))
(define (farm-hash-1 farm recursive-equal-hash)
  (+ (* 10000 (farm-apples farm))
     (* 100 (farm-oranges farm))
     (* 1 (farm-sheep farm))))

(define (farm-hash-2 farm recursive-equal-hash)
  (+ (* 10000 (farm-sheep farm))
     (* 100 (farm-apples farm))
     (* 1 (farm-oranges farm))))

(define-struct farm (apples oranges sheep)
               #:property prop:equal+hash
               (list farm=? farm-hash-1 farm-hash-2))
(define east (make-farm 5 2 20))
(define west (make-farm 18 6 14))
(define north (make-farm 5 20 20))
(define south (make-farm 18 6 14))

(equal? east west)
(equal? east north)
(equal? west south)
]}

@section{Boolean Synonyms}

@note-lib[racket/bool]

@defthing[true boolean?]{A synonym for @scheme[#t].}

@defthing[false boolean?]{A synonym for @scheme[#f].}

@defproc[(symbol=? [a symbol?] [b symbol?]) boolean?]{

Returns @scheme[(equal? a b)].}

@defproc[(boolean=? [a boolean?] [b boolean?]) boolean?]{

Returns @scheme[(equal? a b)].}

@defproc[(false? [v any/c]) boolean?]{

Returns @scheme[(not v)].}
