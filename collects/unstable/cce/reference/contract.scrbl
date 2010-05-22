#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/contract))

@title[#:style 'quiet #:tag "cce-contract"]{Contracts}

@defmodule[unstable/cce/contract]

This module provides useful contracts and contract constructors.

@section{Flat Contracts}

@defthing[nat/c flat-contract?]{

This contract recognizes natural numbers that satisfy
@scheme[exact-nonnegative-integer?].

}

@defthing[pos/c flat-contract?]{

This contract recognizes positive integers that satisfy
@scheme[exact-positive-integer?].

}

@defthing[truth/c flat-contract?]{

This contract recognizes Scheme truth values, i.e., any value, but with a more
informative name and description.  Use it in negative positions for arguments
that accept arbitrary truth values that may not be booleans.

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
@defthing[predicate/c contract?]
@defthing[predicate-like/c contract?]
)]{

These contracts recognize predicates: functions of a single argument that
produce a boolean result.

The first constrains its output to satisfy @scheme[boolean?].  Use
@scheme[predicate/c] in positive position for predicates that guarantee a result
of @scheme[#t] or @scheme[#f].

The second constrains its output to satisfy @scheme[truth/c].  Use
@scheme[predicate-like/c] in negative position for predicates passed as
arguments that may return arbitrary values as truth values.

}

@deftogether[(
@defthing[comparison/c contract?]
@defthing[comparison-like/c contract?]
)]{

These contracts recognize comparisons: functions of two arguments that
produce a boolean result.

The first constrains its output to satisfy @scheme[boolean?].  Use
@scheme[comparison/c] in positive position for comparisons that guarantee a
result of @scheme[#t] or @scheme[#f].

The second constrains its output to satisfy @scheme[truth/c].  Use
@scheme[comparison-like/c] in negative position for comparisons passed as
arguments that may return arbitrary values as truth values.

}

@defproc[(sequence/c [elem/c contract?] ...) contract?]{

Wraps a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence},
obligating it to produce as many values as there are @scheme[elem/c] contracts,
and obligating each value to satisfy the corresponding @scheme[elem/c].  The
result is not guaranteed to be the same kind of sequence as the original value;
for instance, a wrapped list is not guaranteed to satisfy @scheme[list?].

@defexamples[
#:eval (evaluator 'unstable/cce/contract)
(define/contract predicates
  (sequence/c (-> any/c boolean?))
  (list integer? string->symbol))
(for ([P predicates])
  (printf "~s\n" (P "cat")))
]

}

@defproc[(dict/c [key/c contract?] [value/c contract?]) contract?]{

Wraps a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{dictionary},
obligating its keys to satisfy @scheme[key/c] and their corresponding values to
satisfy @scheme[value/c].  The result is not guaranteed to be the same kind of
dictionary as the original value; for instance, a wrapped hash table is not
guaranteed to satisfy @scheme[hash?].

@defexamples[
#:eval (evaluator 'unstable/cce/contract)
(define/contract table
  (dict/c symbol? string?)
  (make-immutable-hash (list (cons 'A "A") (cons 'B 2) (cons 3 "C"))))
(dict-ref table 'A)
(dict-ref table 'B)
(dict-ref table 3)
]

@emph{Warning:} Bear in mind that key and value contracts are re-wrapped on
every dictionary operation, and dictionaries wrapped in @scheme[dict/c] multiple
times will perform the checks as many times for each operation.  Especially for
immutable dictionaries (which may be passed through a constructor that involves
@scheme[dict/c] on each update), contract-wrapped dictionaries may be much less
efficient than the original dictionaries.

}
