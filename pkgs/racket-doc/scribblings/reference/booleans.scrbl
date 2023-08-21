#lang scribble/doc
@(require "mz.rkt")

@(define bool-eval (make-base-eval))
@(bool-eval '(require racket/bool))

@title[#:tag "booleans"]{Booleans}

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


@defproc[(immutable? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an immutable @tech{string},
@tech{byte string}, @tech{vector}, @tech{hash table}, or @tech{box},
@racket[#f] otherwise.

Note that @racket[immutable?] is not a general predicate for
immutability (despite its name). It works only for a handful of
datatypes for which a single predicate---@racket[string?],
@racket[vector?], @|etc|---recognizes both mutable and immutable variants
of the datatype. In particular, @racket[immutable?] produces
@racket[#f] for a @tech{pair}, even though pairs are immutable, since
@racket[pair?] implies immutability.

See also @racket[immutable-string?], @racket[mutable-string?], etc.

@examples[
(immutable? 'hello)
(immutable? "a string")
(immutable? (box 5))
(immutable? #(0 1 2 3))
(immutable? (make-hash))
(immutable? (make-immutable-hash '([a b])))
(immutable? #t)
]}

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


@section{Mutability Predicates}

@note-lib[racket/mutability]

@history[#:added "8.9.0.3"]

@deftogether[(
@defproc[(mutable-string? [v any/c]) boolean?]
@defproc[(immutable-string? [v any/c]) boolean?]
@defproc[(mutable-bytes? [v any/c]) boolean?]
@defproc[(immutable-bytes? [v any/c]) boolean?]
@defproc[(mutable-vector? [v any/c]) boolean?]
@defproc[(immutable-vector? [v any/c]) boolean?]
@defproc[(mutable-box? [v any/c]) boolean?]
@defproc[(immutable-box? [v any/c]) boolean?]
@defproc[(mutable-hash? [v any/c]) boolean?]
@defproc[(immutable-hash? [v any/c]) boolean?]
)]{

Predicates that combine @racket[string?], @racket[bytes?],
@racket[vector?], @racket[box?], and @racket[hash?] with
@racket[immutable?] or its inverse. The predicates are potentially
faster than using @racket[immutable?] and other predicates separately.

}

@close-eval[bool-eval]

