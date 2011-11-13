#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/contract))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/contract racket/dict unstable/contract))

@title[#:tag "contract"]{Contracts}

@defmodule[unstable/contract]

@unstable-header[]

@deftogether[[
@defproc[(non-empty-string? [x any/c]) boolean?]
@defproc[(non-empty-list? [x any/c]) boolean?]
@defproc[(non-empty-bytes? [x any/c]) boolean?]
@defproc[(non-empty-vector? [x any/c]) boolean?]]]{

Returns @racket[#t] if @racket[x] is of the appropriate data type
(string, list, bytes, or vector, respectively) and is not empty;
returns @racket[#f] otherwise.
}

@defproc[(singleton-list? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a list of one element; returns
@racket[#f] otherwise.
}

@defthing[port-number? contract?]{
Equivalent to @racket[(between/c 1 65535)].
}

@defthing[tcp-listen-port? contract?]{
Equivalent to @racket[(between/c 0 65535)].
}

@defthing[path-piece? contract?]{
Equivalent to @racket[(or/c path-string? (symbols 'up 'same))].
}

@addition{Ryan Culpepper}

@defproc[(if/c [predicate (-> any/c any/c)]
               [then-contract contract?]
               [else-contract contract?])
         contract?]{

Produces a contract that, when applied to a value, first tests the
value with @racket[predicate]; if @racket[predicate] returns true, the
@racket[then-contract] is applied; otherwise, the
@racket[else-contract] is applied. The resulting contract is a flat
contract if both @racket[then-contract] and @racket[else-contract] are
flat contracts.

For example, the following contract enforces that if a value is a
procedure, it is a thunk; otherwise it can be any (non-procedure)
value:
  @racketblock[(if/c procedure? (-> any) any/c)]
Note that the following contract is @bold{not} equivalent:
  @racketblock[(or/c (-> any) any/c) (code:comment "wrong!")]
The last contract is the same as @racket[any/c] because
@racket[or/c] tries flat contracts before higher-order contracts.
}

@defthing[failure-result/c contract?]{

A contract that describes the failure result arguments of procedures
such as @racket[hash-ref].

Equivalent to @racket[(if/c procedure? (-> any) any/c)].
}

@defproc[(rename-contract [contract contract?]
                          [name any/c])
         contract?]{

Produces a contract that acts like @racket[contract] but with the name
@racket[name].

The resulting contract is a flat contract if @racket[contract] is a
flat contract.
}

@addition{Asumu Takikawa}

@defproc[(option/c [contract contract?]) contract?]{

Creates a contract that acts like @racket[contract] but will also
accept @racket[#f]. Intended to describe situations where a failure
or default value may be used.
}

@addition[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@section{Flat Contracts}

@defthing[nat/c flat-contract?]{

This contract recognizes natural numbers that satisfy
@racket[exact-nonnegative-integer?].

}

@defthing[pos/c flat-contract?]{

This contract recognizes positive integers that satisfy
@racket[exact-positive-integer?].

}

@defthing[truth/c flat-contract?]{

This contract recognizes Scheme truth values, i.e., any value, but with a more
informative name and description.  Use it in negative positions for arguments
that accept arbitrary truth values that may not be booleans.

}

@section{Syntax Object Contracts}

@defproc[(syntax-datum/c [datum/c any/c]) flat-contract?]{

Recognizes syntax objects @racket[stx] such that @racket[(syntax->datum stx)]
satisfies @racket[datum/c].

}

@defproc[(syntax-listof/c [elem/c any/c]) flat-contract?]{

Recognizes syntax objects @racket[stx] such that @racket[(syntax->list stx)]
satisfies @racket[(listof elem/c)].

}

@defproc[(syntax-list/c [elem/c any/c] ...) flat-contract?]{

Recognizes syntax objects @racket[stx] such that @racket[(syntax->list stx)]
satisfies @racket[(list/c elem/c ...)].

}

@section{Higher-Order Contracts}

@deftogether[(
@defthing[thunk/c contract?]
@defthing[unary/c contract?]
@defthing[binary/c contract?]
)]{

These contracts recognize functions that accept 0, 1, or 2 arguments,
respectively, and produce a single result.

}

@deftogether[(
@defthing[predicate-like/c contract?]
)]{

This contract recognizes unary functions whose results satisfy @racket[truth/c].  Use
@racket[predicate-like/c] in negative position for predicates passed as
arguments that may return arbitrary values as truth values.

}

@deftogether[(
@defthing[comparison/c contract?]
@defthing[comparison-like/c contract?]
)]{

These contracts recognize comparisons: functions of two arguments that
produce a boolean result.

The first constrains its output to satisfy @racket[boolean?].  Use
@racket[comparison/c] in positive position for comparisons that guarantee a
result of @racket[#t] or @racket[#f].

The second constrains its output to satisfy @racket[truth/c].  Use
@racket[comparison-like/c] in negative position for comparisons passed as
arguments that may return arbitrary values as truth values.

}

@defproc[(sequence/c [elem/c contract?] ...) contract?]{

Wraps a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence},
obligating it to produce as many values as there are @racket[elem/c] contracts,
and obligating each value to satisfy the corresponding @racket[elem/c].  The
result is not guaranteed to be the same kind of sequence as the original value;
for instance, a wrapped list is not guaranteed to satisfy @racket[list?].

@defexamples[
#:eval the-eval
(define/contract predicates
  (sequence/c (-> any/c boolean?))
  (in-list (list integer?
                 string->symbol)))
(for ([P predicates])
  (printf "~s\n" (P "cat")))
(define/contract numbers&strings
  (sequence/c number? string?)
  (in-dict (list (cons 1 "one")
                 (cons 2 "two")
                 (cons 3 'three))))
(for ([(N S) numbers&strings])
  (printf "~s: ~a\n" N S))
]

}

@defproc[(dict/c [key/c contract?] [value/c contract?]) contract?]{

Wraps a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{dictionary},
obligating its keys to satisfy @racket[key/c] and their corresponding values to
satisfy @racket[value/c].  The result is not guaranteed to be the same kind of
dictionary as the original value; for instance, a wrapped hash table is not
guaranteed to satisfy @racket[hash?].

@defexamples[
#:eval the-eval
(define/contract table
  (dict/c symbol? string?)
  (make-immutable-hash (list (cons 'A "A") (cons 'B 2) (cons 3 "C"))))
(dict-ref table 'A)
(dict-ref table 'B)
(dict-ref table 3)
]

@emph{Warning:} Bear in mind that key and value contracts are re-wrapped on
every dictionary operation, and dictionaries wrapped in @racket[dict/c] multiple
times will perform the checks as many times for each operation.  Especially for
immutable dictionaries (which may be passed through a constructor that involves
@racket[dict/c] on each update), contract-wrapped dictionaries may be much less
efficient than the original dictionaries.

}

@(close-eval the-eval)
