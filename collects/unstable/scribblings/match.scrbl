#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label unstable/match racket/match racket/contract racket/base))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/class racket/match unstable/match))

@title[#:tag "match"]{Match}
@unstable-header[]

@defmodule[unstable/match]

@addition[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defform[(match? val-expr pat ...)]{

Returns @racket[#t] if the result of @racket[val-expr] matches any of
@racket[pat], and returns @racket[#f] otherwise.

@defexamples[
#:eval the-eval
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

@defform[(as ([lhs-id rhs-expr] ...) pat ...)]{

As a match expander, binds each @racket[lhs-id] as a pattern variable with the
result value of @racket[rhs-expr], and continues matching each subsequent
@racket[pat].

@defexamples[
#:eval the-eval
(match (list 1 2 3)
  [(as ([a 0]) (list b c d)) (list a b c d)])
]

}

@addition[@author+email["Asumu Takikawa" "asumu@racket-lang.org"]]

@defform[(match*? (val-expr ...) (pat ...) ...)]{

Similar to @racket[match?], but uses @racket[match*] and accepts
multiple @racket[val-expr] and corresponding @racket[pat] in each
clause to match on.

@defexamples[
#:eval the-eval
(match*? (1 2 3)
  (a b c)
  (x #f z))
(match*? (1 2 3)
  (a (? odd?) c)
  (x y z))
(match*? (#f #f #f)
  (1 2 3)
  (4 5 6))
]}

@defform/subs[
  #:literals (field)
  (object maybe-class field-clause ...)
  ([maybe-class
    code:blank
    class-expr]
   [field-clause (field field-id maybe-pat)]
   [maybe-pat
    code:blank
    pat])]{

A match expander that checks if the matched value is an object
and contains the fields named by the @racket[field-id]s. If
@racket[pat]s are provided, the value in each field is matched to
its corresponding @racket[pat]. If a @racket[pat] is not provided,
it defaults to the name of the field.

If @racket[class-expr] is provided, the match expander will also
check that the supplied object is an instance of the class
that the given expression evaluates to.

@defexamples[
#:eval the-eval
(define point%
  (class object%
    (super-new)
    (init-field x y)))

(match (make-object point% 3 5)
  [(object point% (field x) (field y))
   (sqrt (+ (* x x) (* y y)))])

(match (make-object point% 0 0)
  [(object (field x (? zero?))
           (field y (? zero?)))
   'origin])

(match (make-object object%)
  [(object (field x) (field y))
   'ok]
  [_ 'fail])
]

}

@close-eval[the-eval]
