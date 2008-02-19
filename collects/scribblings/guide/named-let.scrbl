#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title{Named @scheme[let]}

A named @scheme[let] is an iteration and recursion form. It uses the
same syntactic keyword @scheme[let] as for local binding, but an
identifier after the @scheme[let] (instead of an immediate open
parenthesis) triggers a different parsing.

@specform[
(let _proc-id ([_arg-id _init-expr] ...)
  _body ...+)
]

A named @scheme[let] form is equivalent to

@schemeblock[
(letrec ([_proc-id (lambda (_arg-id ...)
                     _body ...+)])
  (_proc-id _init-expr ...))
]

That is, a named @scheme[let] binds a function identifier that is
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

