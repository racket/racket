#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"
          (for-label scheme/match))

@title{Simple Dispatch: @scheme[case]}

The @scheme[case] form dispatches to a clause by matching the result
of an expression to the values for the clause:

@specform[(case expr
            [(datum ...+) expr ...+]
            ...)]

Each @scheme[_datum] will be compared to the result of the first
@scheme[_expr] using @scheme[eqv?]. Since @scheme[eqv?] doesn't work on
many kinds of values, notably symbols and lists, each @scheme[_datum]
is typically a number, symbol, or boolean.

Multiple @scheme[_datum]s can be supplied for each clause, and the
corresponding @scheme[_expr] is evaluated of any of the
@scheme[_datum]s match.

@examples[
(let ([v (random 6)])
  (printf "~a\n" v)
  (case v
    [(0) 'zero]
    [(1) 'one]
    [(2) 'two]
    [(3 4 5) 'many]))
]

The last clause of a @scheme[case] form can use @scheme[else], just
like @scheme[cond]:

@examples[
(case (random 6)
  [(0) 'zero]
  [(1) 'one]
  [(2) 'two]
  [else 'many])
]

For more general pattern matching, use @scheme[match], which is
introduced in @secref["match"].
