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

@defform/subs[
  (define/match (head args)
    match*-clause ...)
  ([head id (head args)]
   [args (code:line arg ...)
         (code:line arg ... @#,racketparenfont{.} rest-id)]
   [arg arg-id
        [arg-id default-expr]
        (code:line keyword arg-id)
        (code:line keyword [arg-id default-expr])]
   [match*-clause [(pat ...+) body ...+]
                  [(pat ...+) (=> id) body ...+]])
]{
  Binds @racket[id] to a procedure that is defined by pattern matching
  clauses using @racket[match*]. Each clause takes a sequence of
  patterns that correspond to the arguments in the function header.
  The arguments are ordered as they appear in the function header for
  matching purposes.

  The function header may contain optional or keyword arguments, or
  may be in curried form.

  @defexamples[#:eval the-eval
    (define/match (fact n)
      [(0) 1]
      [(n) (* n (fact (sub1 n)))])
    (fact 5)

    (define/match ((f x) #:y [y '(1 2 3)])
      [((regexp #rx"p+") `(,a 2 3)) a]
      [(_ _) #f])
    ((f "ape") #:y '(5 2 3))
    ((f "dog"))

    (define/match (g x y . rst)
      [(0 0 '()) #t]
      [(5 5 '(5 5)) #t]
      [(_ _ _) #f])
    (g 0 0)
    (g 5 5 5 5)
    (g 1 2)
  ]
}

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
