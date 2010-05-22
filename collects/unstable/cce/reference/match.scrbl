#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/match))

@title[#:style 'quiet #:tag "cce-match"]{Pattern Matching}

@defmodule[unstable/cce/match]

This module provides tools for pattern matching with @scheme[match].

@defform[(match? val-expr pat ...)]{

Returns @scheme[#t] if the result of @scheme[val-expr] matches any of
@scheme[pat], and returns @scheme[#f] otherwise.

@defexamples[
#:eval (evaluator 'unstable/cce/match)
(match? (list 1 2 3)
  (list a b c)
  (vector x y z))
(match? (vector 1 2 3)
  (list a b c)
  (vector x y z))
(match? (+ 1 2 3)
  (list a b c)
  (vector x y z))
]

}

@defform[(define-struct-pattern pat-id struct-id)]{

Defines @scheme[pat-id] as a match expander that takes one pattern argument per
field of the structure described by @scheme[struct-id].  The resulting match
expander recognizes instances of the structure and matches their fields against
the corresponding patterns.

@defexamples[
#:eval (evaluator 'unstable/cce/match)
(define-struct pair [a b] #:transparent)
(define-struct-pattern both pair)
(match (make-pair 'left 'right)
  [(both a b) (list a b)])
]

}

@defform[(as ([lhs-id rhs-expr] ...) pat ...)]{

As a match expander, binds each @scheme[lhs-id] as a pattern variable with the
result value of @scheme[rhs-expr], and continues matching each subsequent
@scheme[pat].

@defexamples[
#:eval (evaluator 'unstable/cce/match)
(match (list 1 2 3)
  [(as ([a 0]) (list b c d)) (list a b c d)])
]

}

@defform*[[($ struct-id expr ...) ($ struct-id pat ...)]]{

As an expression, constructs an instance of the structure described by
@scheme[struct-id] with fields specified by each @scheme[expr].

As a match expander, matches instances of the structure described by
@scheme[struct-id] with fields matched by each @scheme[pat].

@defexamples[
#:eval (evaluator 'unstable/cce/match)
(define-struct pair [a b] #:transparent)
($ pair 1 2)
(match ($ pair 1 2)
  [($ pair a b) (list a b)])
]

}
