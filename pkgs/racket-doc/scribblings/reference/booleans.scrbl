#lang scribble/doc
@(require "mz.rkt")

@(define bool-eval (make-base-eval))
@(bool-eval '(require racket/bool))

@title[#:tag "booleans"]{Booleans and Equality}

True and false @deftech{booleans} are represented by the values
@racket[#t] and @racket[#f], respectively, though operations that
depend on a boolean value typically treat anything other than
@racket[#f] as true. The @racket[#t] value is always @racket[eq?] to
itself, and @racket[#f] is always @racket[eq?] to itself.

@see-read-print["boolean" #:print "booleans"]{booleans}

See also @racket[and], @racket[or], @racket[andmap], and @racket[ormap].


@defproc[(boolean? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is @racket[#t] or @racket[#f],
@racket[#f] otherwise.

@examples[
(boolean? #f)
(boolean? #t)
(boolean? 'true)
]}


@defproc[(not [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is @racket[#f], @racket[#f] otherwise.

@examples[
(not #f)
(not #t)
(not 'we-have-no-bananas)
]}


@defproc[(equal? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @racket[equal?] if and only if they are @racket[eqv?],
unless otherwise specified for a particular datatype.

Datatypes with further specification of @racket[equal?] include
strings, byte strings, pairs, mutable pairs, vectors, boxes, hash
tables, and inspectable structures. In the last six cases, equality
is recursively defined; if both @racket[v1] and @racket[v2] contain
reference cycles, they are equal when the infinite unfoldings of the
values would be equal. See also @racket[gen:equal+hash] and @racket[prop:impersonator-of].

@examples[
(equal? 'yes 'yes)
(equal? 'yes 'no)
(equal? (expt 2 100) (expt 2 100))
(equal? 2 2.0)
(equal? (make-string 3 #\z) (make-string 3 #\z))
]}


@defproc[(eqv? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @racket[eqv?] if and only if they are @racket[eq?],
unless otherwise specified for a particular datatype.

The @tech{number} and @tech{character} datatypes are the only ones for which
@racket[eqv?] differs from @racket[eq?].

@examples[
(eqv? 'yes 'yes)
(eqv? 'yes 'no)
(eqv? (expt 2 100) (expt 2 100))
(eqv? 2 2.0)
(eqv? (integer->char #x3BB) (integer->char #x3BB))
(eqv? (make-string 3 #\z) (make-string 3 #\z))
]}


@defproc[(eq? [v1 any/c] [v2 any/c]) boolean?]{

Return @racket[#t] if @racket[v1] and @racket[v2] refer to the same
object, @racket[#f] otherwise. See also @secref["model-eq"].

@examples[
(eq? 'yes 'yes)
(eq? 'yes 'no)
(let ([v (mcons 1 2)]) (eq? v v))
(eq? (mcons 1 2) (mcons 1 2))
(eq? (make-string 3 #\z) (make-string 3 #\z))
]}


@defproc[(equal?/recur [v1 any/c] [v2 any/c] [recur-proc (any/c any/c -> any/c)]) boolean?]{

Like @racket[equal?], but using @racket[recur-proc] for recursive
comparisons (which means that reference cycles are not handled
automatically). Non-@racket[#f] results from @racket[recur-proc] are
converted to @racket[#t] before being returned by
@racket[equal?/recur].

@examples[
(equal?/recur 1 1 (lambda (a b) #f))
(equal?/recur '(1) '(1) (lambda (a b) #f))
(equal?/recur '#(1 1 1) '#(1 1.2 3/4)
              (lambda (a b) (<= (abs (- a b)) 0.25)))
]}


@defproc[(immutable? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an immutable @tech{string},
@tech{byte string}, @tech{vector}, @tech{hash table}, or box,
@racket[#f] otherwise.

Note that @racket[immutable?] is not a general predicate for
immutability (despite its name). It works only for a handful of
datatypes for which a single predicate---@racket[string?],
@racket[vector?], @|etc|---recognizes both mutable and immutable variants
of the datatype. In particular, @racket[immutable?] produces
@racket[#f] for a @tech{pair}, even though pairs are immutable, since
@racket[pair?] implies immutability.

@examples[
(immutable? 'hello)
(immutable? "a string")
(immutable? (box 5))
(immutable? #(0 1 2 3))
(immutable? (make-hash))
(immutable? (make-immutable-hash '([a b])))
]}

@defthing[gen:equal+hash any/c]{
A @tech{generic interface} (see @secref["struct-generics"]) that
supplies an equality predicate and hashing functions for a structure
type. The following methods must be implemented:

@itemize[

 @item{@racket[_equal-proc : (any/c any/c (any/c any/c . ->
        . boolean?)  . -> . any/c)] --- tests whether the first two
        arguments are equal, where both values are instances of the
        structure type to which the generic interface is associated
        (or a subtype of the structure type).

        The third argument is an @racket[equal?]  predicate to use for
        recursive equality checks; use the given predicate instead of
        @racket[equal?] to ensure that data cycles are handled
        properly and to work with @racket[equal?/recur] (but beware
        that an arbitrary function can be provided to
        @racket[equal?/recur] for recursive checks, which means that
        arguments provided to the predicate might be exposed to
        arbitrary code).

        The @racket[_equal-proc] is called for a pair of structures
        only when they are not @racket[eq?], and only when they both
        have a @racket[gen:equal+hash] value inherited from the same
        structure type. With this strategy, the order in which
        @racket[equal?] receives two structures does not matter. It
        also means that, by default, a structure sub-type inherits the
        equality predicate of its parent, if any.}

 @item{@racket[_hash-proc : (any/c (any/c . -> . exact-integer?) . ->
       . exact-integer?)] --- computes a hash code for the given
       structure, like @racket[equal-hash-code]. The first argument is
       an instance of the structure type (or one of its subtypes) to
       which the generic interface is associated.

       The second argument is an @racket[equal-hash-code]-like
       procedure to use for recursive hash-code computation; use the
       given procedure instead of @racket[equal-hash-code] to ensure
       that data cycles are handled properly.}

 @item{@racket[_hash2-proc : (any/c (any/c . -> . exact-integer?) . ->
       . exact-integer?)] --- computes a secondary hash code for the
       given structure. This procedure is like @racket[_hash-proc],
       but analogous to @racket[equal-secondary-hash-code].}

]

Take care to ensure that @racket[_hash-proc] and @racket[_hash2-proc]
are consistent with @racket[_equal-proc]. Specifically,
@racket[_hash-proc] and @racket[_hash2-proc] should produce the same
value for any two structures for which @racket[_equal-proc] produces a
true value.

When a structure type has no @racket[gen:equal+hash] implementation, then
transparent structures (i.e., structures with an @tech{inspector} that
is controlled by the current @tech{inspector}) are @racket[equal?]
when they are instances of the same structure type (not counting
sub-types), and when they have @racket[equal?] field values.  For
transparent structures, @racket[equal-hash-code] and
@racket[equal-secondary-hash-code] derive hash code using the field
values. For opaque structure types, @racket[equal?] is the same as
@racket[eq?], and @racket[equal-hash-code] and
@racket[equal-secondary-hash-code] results are based only on
@racket[eq-hash-code]. If a structure has a @racket[prop:impersonator-of]
property, then the @racket[prop:impersonator-of] property takes precedence over
@racket[gen:equal+hash] if the property value's procedure returns a
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
               #:methods gen:equal+hash
	       [(define equal-proc farm=?)
                (define hash-proc  farm-hash-1)
		(define hash2-proc farm-hash-2)])
(define east (make-farm 5 2 20))
(define west (make-farm 18 6 14))
(define north (make-farm 5 20 20))
(define south (make-farm 18 6 14))

(equal? east west)
(equal? east north)
(equal? west south)
]}

@defthing[prop:equal+hash struct-type-property?]{

A deprecated @tech{structure type property} (see @secref["structprops"])
that supplies an equality predicate and hashing functions for a structure
type. The @racket[gen:equal+hash] @tech{generic interface} should be used,
instead. A @racket[prop:equal+hash] property value is a list of
three procedures that correspond to the methods of @racket[gen:equal+hash].
}

@section{Boolean Aliases}

@note-lib[racket/bool]

@defthing[true boolean?]{An alias for @racket[#t].}

@defthing[false boolean?]{An alias for @racket[#f].}

@defproc[(symbol=? [a symbol?] [b symbol?]) boolean?]{

Returns @racket[(equal? a b)] (if @racket[a] and @racket[b] are symbols).}

@defproc[(boolean=? [a boolean?] [b boolean?]) boolean?]{

Returns @racket[(equal? a b)] (if @racket[a] and @racket[b] are booleans).}

@defproc[(false? [v any/c]) boolean?]{

Returns @racket[(not v)].}

@defform[(nand expr ...)]{
  Same as @racket[(not (and expr ...))].

  @examples[#:eval 
            bool-eval
            (nand #f #t)
            (nand #f (error 'ack "we don't get here"))]
}

@defform[(nor expr ...)]{
  Same as @racket[(not (or expr ...))].

  In the two argument case, returns @racket[#t] if neither of the
  arguments is a true value.
  
  @examples[#:eval 
            bool-eval
            (nor #f #t)
            (nor #t (error 'ack "we don't get here"))]

          
}

@defform[(implies expr1 expr2)]{
  Checks to be sure that the first
  expression implies the second.
  
  Same as @racket[(if expr1 expr2 #t)].
  
  @examples[#:eval 
            bool-eval
            (implies #f #t)
            (implies #f #f)
            (implies #t #f)
            (implies #f (error 'ack "we don't get here"))]

}

@defproc[(xor [b1 any/c] [b2 any/c]) any]{
  Returns the exclusive or of @racket[b1] and @racket[b2].

  If exactly one of @racket[b1] and @racket[b2] is
  not @racket[#f], then return it. Otherwise, returns
  @racket[#f].

  @examples[#:eval
            bool-eval
            (xor 11 #f)
            (xor #f 22)
            (xor 11 22)
            (xor #f #f)]

}


@close-eval[bool-eval]
