#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label racket/match))

@title[#:tag "case"]{Simple Dispatch: @racket[case]}

The @racket[case] form dispatches to a clause by matching the result
of an expression to the values for the clause:

@specform[(case expr
            [(datum ...+) expr ...+]
            ...)]

Each @racket[_datum] will be compared to the result of the first
@racket[_expr] using @racket[eqv?]. Since @racket[eqv?] doesn't work on
many kinds of values, notably strings and lists, each @racket[_datum]
is typically a number, symbol, or boolean.

Multiple @racket[_datum]s can be supplied for each clause, and the
corresponding @racket[_expr] is evaluated if any of the
@racket[_datum]s match.

@examples[
(let ([v (random 6)])
  (printf "~a\n" v)
  (case v
    [(0) 'zero]
    [(1) 'one]
    [(2) 'two]
    [(3 4 5) 'many]))
]

The last clause of a @racket[case] form can use @racket[else], just
like @racket[cond]:

@examples[
(case (random 6)
  [(0) 'zero]
  [(1) 'one]
  [(2) 'two]
  [else 'many])
]

For more general pattern matching, use @racket[match], which is
introduced in @secref["match"].
