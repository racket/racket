#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label racket/match))

@(begin
  (define match-eval (make-base-eval))
  (interaction-eval #:eval match-eval (require racket/match)))

@title[#:tag "match"]{Pattern Matching}

The @racket[match] form supports pattern matching on arbitrary Racket
values, as opposed to functions like @racket[regexp-match] that
compare regular expressions to byte and character sequences (see
@secref["regexp"]).

@specform[
(match target-expr
  [pattern expr ...+] ...)
]

The @racket[match] form takes the result of @racket[target-expr] and
tries to match each @racket[_pattern] in order. As soon as it finds a
match, it evaluates the corresponding @racket[_expr] sequence to
obtain the result for the @racket[match] form. If @racket[_pattern]
includes @deftech{pattern variables}, they are treated like wildcards,
and each variable is bound in the @racket[_expr] to the input
fragments that it matched.

Most Racket literal expressions can be used as patterns:

@interaction[
#:eval match-eval
(match 2
  [1 'one]
  [2 'two]
  [3 'three])
(match #f
  [#t 'yes]
  [#f 'no])
(match "apple"
  ['apple 'symbol]
  ["apple" 'string]
  [#f 'boolean])
]

Constructors like @racket[cons], @racket[list], and @racket[vector]
can be used to create patterns that match pairs, lists, and vectors:

@interaction[
#:eval match-eval
(match '(1 2)
  [(list 0 1) 'one]
  [(list 1 2) 'two])
(match '(1 . 2)
  [(list 1 2) 'list]
  [(cons 1 2) 'pair])
(match #(1 2)
  [(list 1 2) 'list]
  [(vector 1 2) 'vector])
]

A constructor bound with @racket[struct] also can be used as a pattern
constructor:

@interaction[
#:eval match-eval
(struct shoe (size color))
(struct hat (size style))
(match (hat 23 'bowler)
 [(shoe 10 'white) "bottom"]
 [(hat 23 'bowler) "top"])
]

Unquoted, non-constructor identifiers in a pattern are @tech{pattern
variables} that are bound in the result expressions, except @racket[_],
which does not bind (and thus is usually used as a catch-all):

@interaction[
#:eval match-eval
(match '(1)
  [(list x) (+ x 1)]
  [(list x y) (+ x y)])
(match '(1 2)
  [(list x) (+ x 1)]
  [(list x y) (+ x y)])
(match (hat 23 'bowler)
  [(shoe sz col) sz] 
  [(hat sz stl) sz])
(match (hat 11 'cowboy)
  [(shoe sz 'black) 'a-good-shoe] 
  [(hat sz 'bowler) 'a-good-hat]
  [_ 'something-else])
]


An ellipsis, written @litchar{...}, acts like a Kleene star within a
list or vector pattern: the preceding sub-pattern can be used to match
any number of times for any number of consecutive elements of the list
or vector. If a sub-pattern followed by an ellipsis includes a pattern
variable, the variable matches multiple times, and it is bound in the
result expression to a list of matches:

@interaction[
#:eval match-eval
(match '(1 1 1)
  [(list 1 ...) 'ones]
  [_ 'other])
(match '(1 1 2)
  [(list 1 ...) 'ones]
  [_ 'other])
(match '(1 2 3 4)
  [(list 1 x ... 4) x])
(match (list (hat 23 'bowler) (hat 22 'pork-pie))
  [(list (hat sz styl) ...) (apply + sz)])
]

Ellipses can be nested to match nested repetitions, and in that case,
pattern variables can be bound to lists of lists of matches:

@interaction[
#:eval match-eval
(match '((! 1) (! 2 2) (! 3 3 3))
  [(list (list '! x ...) ...) x])
]


The @racket[quasiquote] form  (see @secref["qq"] for more about it) can also be used to build patterns.
While unquoted portions of a normal quasiquoted form mean regular racket evaluation, here unquoted
portions mean go back to regular pattern matching.

So, in the example below, the with expression is the pattern and it gets rewritten into the
application expression, using quasiquote as a pattern in the first instance and quasiquote
to build an expression in the second.

@interaction[
#:eval match-eval
(match `{with {x 1} {+ x 1}}
  [`{with {,id ,rhs} ,body}
   `{{lambda {,id} ,body} ,rhs}])
]

For information on many more pattern forms, see @racketmodname[racket/match].

Forms like @racket[match-let] and @racket[match-lambda] support
patterns in positions that otherwise must be identifiers. For example,
@racket[match-let] generalizes @racket[let] to a @as-index{destructing
bind}:

@interaction[
#:eval match-eval
(match-let ([(list x y z) '(1 2 3)])
  (list z y x))
]

For information on these additional forms, see @racketmodname[racket/match].

@refdetails["match"]{pattern matching}

@close-eval[match-eval]
