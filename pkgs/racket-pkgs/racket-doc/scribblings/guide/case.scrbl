#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label racket/match))

@title[#:tag "case"]{Simple Dispatch: @racket[case]}

The @racket[case] form dispatches to a clause by matching the result
of an expression to the values for the clause:

@specform[(case expr
            [(datum ...+) body ...+]
            ...)]

Each @racket[_datum] will be compared to the result of @racket[_expr]
using @racket[equal?], and then the corresponding @racket[body]s are
evaluated. The @racket[case] form can dispatch to the correct clause
in @math{O(log N)} time for @math{N} @racket[datum]s.

Multiple @racket[_datum]s can be supplied for each clause, and the
corresponding @racket[_body]s are evaluated if any of the
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

For more general pattern matching (but without the dispatch-time
guarantee), use @racket[match], which is introduced in
@secref["match"].
