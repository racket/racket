#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "conditionals"]{Conditionals}

Most functions used for branching, such as @racket[<] and
@racket[string?], produce either @racket[#t] or @racket[#f]. Racket's
branching forms, however, treat any value other than @racket[#f] as
true. We say a @defterm{true value} to mean any value other than
@racket[#f].

This convention for ``true value'' meshes well with protocols where
@racket[#f] can serve as failure or to indicate that an optional value
is not supplied. (Beware of overusing this trick, and remember that an
exception is usually a better mechanism to report failure.)

For example, the @racket[member] function serves double duty; it can
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
@section{Simple Branching: @racket[if]}

@refalso["if"]{@racket[if]}

In an @racket[if] form,

@specform[(if test-expr then-expr else-expr)]

the @racket[_test-expr] is always evaluated. If it produces any value
other than @racket[#f], then @racket[_then-expr] is
evaluated. Otherwise, @racket[_else-expr] is evaluated.

An @racket[if] form must have both a @racket[_then-expr] and an
@racket[_else-expr]; the latter is not optional. To perform (or skip)
side-effects based on a @racket[_test-expr], use @racket[when] or
@racket[unless], which we describe later in @secref["begin"].

@;------------------------------------------------------------------------
@section[#:tag "and+or"]{Combining Tests: @racket[and] and @racket[or]}

@refalso["if"]{@racket[and] and @racket[or]}

Racket's @racket[and] and @racket[or] are syntactic forms, rather than
functions. Unlike a function, the @racket[and] and @racket[or] forms
can skip evaluation of later expressions if an earlier one determines
the answer.

@specform[(and expr ...)]

An @racket[and] form produces @racket[#f] if any of its @racket[_expr]s
produces @racket[#f]. Otherwise, it produces the value of its last
@racket[_expr]. As a special case, @racket[(and)] produces
@racket[#t].

@specform[(or expr ...)]

The @racket[or] form produces @racket[#f] if all of its
@racket[_expr]s produce @racket[#f]. Otherwise, it produces the first
non-@racket[#f] value from its @racket[expr]s.  As a special case,
@racket[(or)] produces @racket[#f].

@examples[
(code:line
 (define (got-milk? lst)
   (and (not (null? lst))
        (or (eq? 'milk (car lst))
            (got-milk? (cdr lst))))) (code:comment @#,t{recurs only if needed}))
(got-milk? '(apple banana))
(got-milk? '(apple milk banana))
]

If evaluation reaches the last @racket[_expr] of an @racket[and] or
@racket[or] form, then the @racket[_expr]'s value directly determines
the @racket[and] or @racket[or] result. Therefore, the last
@racket[_expr] is in tail position, which means that the above
@racket[got-milk?] function runs in constant space.

@guideother{@secref["tail-recursion"] introduces tail calls and tail positions.}

@;------------------------------------------------------------------------
@section[#:tag "cond"]{Chaining Tests: @racket[cond]}

The @racket[cond] form chains a series of tests to select a result
expression. To a first approximation, the syntax of @racket[cond] is
as follows:

@refalso["if"]{@racket[cond]}

@specform[(cond [test-expr expr ...+]
                ...)]

Each @racket[_test-expr] is evaluated in order. If it produces
@racket[#f], the corresponding @racket[_expr]s are ignored, and
evaluation proceeds to the next @racket[_test-expr]. As soon as a
@racket[_test-expr] produces a true value, its @racket[_expr]s
are evaluated to produce the result for the @racket[cond] form, and no
further @racket[_test-expr]s are evaluated.

The last @racket[_test-expr] in a @racket[cond] can be replaced by
@racket[else]. In terms of evaluation, @racket[else] serves as a
synonym for @racket[#t], but it clarifies that the last clause is
meant to catch all remaining cases. If @racket[else] is not used, then
it is possible that no @racket[_test-expr]s produce a true value; in
that case, the result of the @racket[cond] expression is
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

The full syntax of @racket[cond] includes two more kinds of clauses:

@specform/subs[#:literals (else =>)
               (cond cond-clause ...)
               ([cond-clause [test-expr then-expr ...+]
                             [else then-expr ...+]
                             [test-expr => proc-expr]
                             [test-expr]])]

The @racket[=>] variant captures the true result of its
@racket[_test-expr] and passes it to the result of the
@racket[_proc-expr], which must be a function of one argument.

@examples[
(define (after-groucho lst)
  (cond
    [(member "Groucho" lst) => cdr]
    [else (error "not there")]))

(after-groucho '("Harpo" "Groucho" "Zeppo"))
(after-groucho '("Harpo" "Zeppo"))
]

A clause that includes only a @racket[_test-expr] is rarely used. It
captures the true result of the @racket[_test-expr], and simply
returns the result for the whole @racket[cond] expression.
