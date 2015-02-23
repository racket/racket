#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title{Named @racket[let]}

A named @racket[let] is an iteration and recursion form. It uses the
same syntactic keyword @racket[let] as for local binding, but an
identifier after the @racket[let] (instead of an immediate open
parenthesis) triggers a different parsing.

@specform[
(let proc-id ([arg-id init-expr] ...)
  body ...+)
]

A named @racket[let] form is equivalent to

@racketblock[
(letrec ([_proc-id (lambda (_arg-id ...)
                     _body ...+)])
  (_proc-id _init-expr ...))
]

That is, a named @racket[let] binds a function identifier that is
visible only in the function's body, and it implicitly calls the
function with the values of some initial expressions.

@defexamples[
(define (duplicate pos lst)
  (let dup ([i 0]
            [lst lst])
   (cond
    [(= i pos) (cons (car lst) lst)]
    [else (cons (car lst) (dup (+ i 1) (cdr lst)))])))
(duplicate 1 (list "apple" "cheese burger!" "banana"))
]

