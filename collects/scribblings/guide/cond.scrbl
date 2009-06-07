#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "conditionals"]{Conditionals}

Most functions used for branching, such as @scheme[<] and
@scheme[string?], produce either @scheme[#t] or @scheme[#f]. Scheme's
branching forms, however, treat any value other than @scheme[#f] as
true. We say a @defterm{true value} to mean any value other than
@scheme[#f].

This convention for ``true value'' meshes well with protocols where
@scheme[#f] can serve as failure or to indicate that an optional value
is not supplied. (Beware of overusing this trick, and remember that an
exception is usually a better mechanism to report failure.)

For example, the @scheme[member] function serves double duty; it can
be used to find the tail of a list that starts with a particular item,
or it can be used to simply check whether an item is present in a
list:

@interaction[
(member "Groucho" '("Harpo" "Zeppo"))
(member "Groucho" '("Harpo" "Groucho" "Zeppo"))
(if (member "Groucho" '("Harpo" "Zeppo"))
    'yep
    'nope)
(if (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
    'yep
    'nope)
]

@;------------------------------------------------------------------------
@section{Simple Branching: @scheme[if]}

@refalso["if"]{@scheme[if]}

In an @scheme[if] form,

@specform[(if test-expr then-expr else-expr)]

the @scheme[_test-expr] is always evaluated. If it produces any value
other than @scheme[#f], then @scheme[_then-expr] is
evaluated. Otherwise, @scheme[_else-expr] is evaluated.

An @scheme[if] form must have both a @scheme[_then-expr] and an
@scheme[_else-expr]; the latter is not optional. To perform (or skip)
side-effects based on a @scheme[_test-expr], use @scheme[when] or
@scheme[unless], which we describe later in @secref["begin"].

@;------------------------------------------------------------------------
@section[#:tag "and+or"]{Combining Tests: @scheme[and] and @scheme[or]}

@refalso["if"]{@scheme[and] and @scheme[or]}

Scheme's @scheme[and] and @scheme[or] are syntactic forms, rather than
functions. Unlike a function, the @scheme[and] and @scheme[or] forms
can skip evaluation of later expressions if an earlier one determines
the answer.

@specform[(and expr ...)]

An @scheme[and] form produces @scheme[#f] if any of its @scheme[_expr]s
produces @scheme[#f]. Otherwise, it produces the value of its last
@scheme[_expr]. As a special case, @scheme[(and)] produces
@scheme[#t].

@specform[(or expr ...)]

The @scheme[or] form produces @scheme[#f] if all of its
@scheme[_expr]s produce @scheme[#f]. Otherwise, it produces the first
non-@scheme[#f] value from its @scheme[expr]s.  As a special case,
@scheme[(or)] produces @scheme[#f].

@examples[
(code:line
 (define (got-milk? lst)
   (and (not (null? lst))
        (or (eq? 'milk (car lst))
            (got-milk? (cdr lst))))) (code:comment @#,t{recurs only if needed}))
(got-milk? '(apple banana))
(got-milk? '(apple milk banana))
]

If evaluation reaches the last @scheme[_expr] of an @scheme[and] or
@scheme[or] form, then the @scheme[_expr]'s value directly determines
the @scheme[and] or @scheme[or] result. Therefore, the last
@scheme[_expr] is in tail position, which means that the above
@scheme[got-milk?] function runs in constant space.

@guideother{@secref["tail-recursion"] introduces tail calls and tail positions.}

@;------------------------------------------------------------------------
@section[#:tag "cond"]{Chaining Tests: @scheme[cond]}

The @scheme[cond] form chains a series of tests to select a result
expression. To a first approximation, the syntax of @scheme[cond] is
as follows:

@refalso["if"]{@scheme[cond]}

@specform[(cond [test-expr expr ...+]
                ...)]

Each @scheme[_test-expr] is evaluated in order. If it produces
@scheme[#f], the corresponding @scheme[_expr]s are ignored, and
evaluation proceeds to the next @scheme[_test-expr]. As soon as a
@scheme[_test-expr] produces a true value, its @scheme[_text-expr]s
are evaluated to produce the result for the @scheme[cond] form, and no
further @scheme[_test-expr]s are evaluated.

The last @scheme[_test-expr] in a @scheme[cond] can be replaced by
@scheme[else]. In terms of evaluation, @scheme[else] serves as a
synonym for @scheme[#t], but it clarifies that the last clause is
meant to catch all remaining cases. If @scheme[else] is not used, then
it is possible that no @scheme[_test-expr]s produce a true value; in
that case, the result of the @scheme[cond] expression is
@|void-const|.

@examples[
(cond
 [(= 2 3) (error "wrong!")]
 [(= 2 2) 'ok])
(cond
 [(= 2 3) (error "wrong!")])
(cond
 [(= 2 3) (error "wrong!")]
 [else 'ok])
]

@def+int[
(define (got-milk? lst)
  (cond
    [(null? lst) #f]
    [(eq? 'milk (car lst)) #t]
    [else (got-milk? (cdr lst))]))
(got-milk? '(apple banana))
(got-milk? '(apple milk banana))
]

The full syntax of @scheme[cond] includes two more kinds of clauses:

@specform/subs[#:literals (else =>)
               (cond cond-clause ...)
               ([cond-clause [test-expr then-expr ...+]
                             [else then-expr ...+]
                             [test-expr => proc-expr]
                             [test-expr]])]

The @scheme[=>] variant captures the true result of its
@scheme[_test-expr] and passes it to the result of the
@scheme[_proc-expr], which must be a function of one argument.

@examples[
(define (after-groucho lst)
  (cond
    [(member "Groucho" lst) => cdr]
    [else (error "not there")]))

(after-groucho '("Harpo" "Groucho" "Zeppo"))
(after-groucho '("Harpo" "Zeppo"))
]

A clause that includes only a @scheme[_test-expr] is rarely used. It
captures the true result of the @scheme[_test-expr], and simply
returns the result for the whole @scheme[cond] expression.
