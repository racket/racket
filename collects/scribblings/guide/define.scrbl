#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title{Definitions: @scheme[define]}

A basic definition has the form

@specform[(define id expr)]{}

in which case @scheme[_id] is bound to the result of
@scheme[_expr].

@defexamples[
(define salutation (list-ref '("Hi" "Hello") (random 2)))
salutation
]

@;------------------------------------------------------------------------
@section{Procedure Shorthand}

The @scheme[define] form also supports a shorthand for procedure
definitions:

@specform[(define (id arg ...) body ...+)]{}

which is a shorthand for

@schemeblock[
(define id (lambda (arg ...) _body ...+))
]

@defexamples[
(define (greet name)
  (string-append salutation ", " name))
(greet "John")
]

@def+int[
(define (greet first [surname "Smith"] #:hi [hi salutation])
  (string-append hi ", " first " " surname))
(greet "John")
(greet "John" #:hi "Hey")
(greet "John" "Doe")
]

The procedure shorthand via @scheme[define] also supports a final
argument to collect extra arguments in a list:

@specform[(define (id arg ... . rest-id) expr)]{}

which is a shorthand

@schemeblock[
(define id (lambda (id arg ... . rest-id) body-expr ...+))
]

@defexamples[
(define (avg . l)
  (/ (apply + l) (length l)))
(avg 1 2 3)
]

@;------------------------------------------------------------------------
@section[#:tag "guide:multiple-values"]{Multiple Values and @scheme[define-values]}

A Scheme expression normally produces a single result, but some
expressions can produce multiple results. For example,
@scheme[quotient] and @scheme[remainder] each produce a single value,
but @scheme[quotient/remainder] produces the same two values at once:

@interaction[
(quotient 13 3)
(remainder 13 3)
(quotient/remainder 13 3)
]

As shown above, the REPL prints each result value on its own line.

Multiple-valued procedures can be implemented in terms of the
@scheme[values] procedure, which takes any number of values and
returns them as the results:

@interaction[
(values 1 2 3)
]
@def+int[
(define (split-name name)
  (let ([m (regexp-match #rx"^([^ ]*) (.*)$" name)])
    (values (list-ref m 1) (list-ref m 2))))
(split-name "Adam Smith")
]

The @scheme[define-values] form binds multiple identifiers at once to
multiple results produced from a single expression:

@specform[(define-values (id ...) expr)]{}

The number of results produced by the @scheme[_expr] must match the
number of @scheme[_id]s.

@defexamples[
(define-values (given surname) (split-name "Adam Smith"))
given
surname
]

A @scheme[define] form (that is not a procedure shorthand) is
equivalent to a @scheme[define-values] form with a single @scheme[id].

@;------------------------------------------------------------------------
@section[#:tag "guide:intdefs"]{Internal Definitions}

When the grammar for a syntactic form specified @scheme[_body], then
the corresponding position in an instance of the grammar can be either
a definition or an expression.
