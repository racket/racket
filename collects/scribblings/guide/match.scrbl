#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"
          (for-label scheme/match))

@(begin
  (define match-eval (make-base-eval))
  (interaction-eval #:eval match-eval (require scheme/match)))

@title[#:tag "match"]{Pattern Matching}

The @scheme[match] form supports pattern matching on arbitrary Scheme
values, as opposed to functions like @scheme[regexp-match] that
compare regular expressions to byte and character sequences (see
@secref["regexp"]).

@specform[
(match target-expr
  [pattern expr ...+] ...)
]

The @scheme[match] form takes the result of @scheme[target-expr] and
tries to match each @scheme[_pattern] in order. As soon as it finds a
match, it evaluates the corresponding @scheme[_expr] sequence to
obtain the result for the @scheme[match] form. If @scheme[_pattern]
includes @deftech{pattern variables}, they are treated like wildcards,
and each variable is bound in the @scheme[_expr] to the input
fragments that it matched.

Most Scheme literal expressions can be used as patterns:

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

Constructors like @scheme[cons], @scheme[list], and @scheme[vector]
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

The @scheme[struct] construct matches an instance of a particular
structure type:

@interaction[
#:eval match-eval
(define-struct shoe (size color))
(define-struct hat (size style))
(match (make-hat 23 'bowler)
 [(struct shoe (10 'white)) "bottom"]
 [(struct hat (23 'bowler)) "top"])
]

Unquoted, non-constructor identifiers in a pattern are @tech{pattern
variables} that are bound in the result expressions:

@interaction[
#:eval match-eval
(match '(1)
  [(list x) (+ x 1)]
  [(list x y) (+ x y)])
(match '(1 2)
  [(list x) (+ x 1)]
  [(list x y) (+ x y)])
(match (make-hat 23 'bowler)
  [(struct shoe (sz col)) sz] 
  [(struct hat (sz stl)) sz])
]

An ellipsis, written @litchar{...}, act like a Kleene star within a
list or vector pattern: the preceding sub-pattern can be used to match
any number of times for any number of consecutive elements of the list
of vector. If a sub-pattern followed by an ellipsis includes a pattern
variable, the variable matches multiple times, and it is bound in the
result expression to a list of matches:

@interaction[
#:eval match-eval
(match '(1 1 1)
  [(list 1 ...) 'ones]
  [else 'other])
(match '(1 1 2)
  [(list 1 ...) 'ones]
  [else 'other])
(match '(1 2 3 4)
  [(list 1 x ... 4) x])
(match (list (make-hat 23 'bowler) (make-hat 22 'pork-pie))
  [(list (struct hat (sz styl)) ...) (apply + sz)])
]

Ellipses can be nested to match nested repetitions, and in that case,
pattern variables can be bound to lists of lists of matches:

@interaction[
#:eval match-eval
(match '((! 1) (! 2 2) (! 3 3 3))
  [(list (list '! x ...) ...) x])
]

For information on many more pattern forms, see @schememodname[scheme/match].

Forms like @scheme[match-let] and @scheme[match-lambda] support
patterns in positions that otherwise must be identifiers. For example,
@scheme[match-let] generalizes @scheme[let] to a @as-index{destructing
bind}:

@interaction[
#:eval match-eval
(match-let ([(list x y z) '(1 2 3)])
  (list z y x))
]

For information on these additional forms, see @schememodname[scheme/match].

@refdetails["match"]{pattern matching}

@close-eval[match-eval]
