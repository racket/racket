#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@(define def-eval (make-base-eval))

@title[#:tag "define"]{Definitions: @scheme[define]}

A basic definition has the form

@specform[(define id expr)]{}

in which case @scheme[_id] is bound to the result of
@scheme[_expr].

@defexamples[
#:eval def-eval
(define salutation (list-ref '("Hi" "Hello") (random 2)))
salutation
]

@;------------------------------------------------------------------------
@section{Function Shorthand}

The @scheme[define] form also supports a shorthand for function
definitions:

@specform[(define (id arg ...) body ...+)]{}

which is a shorthand for

@schemeblock[
(define _id (lambda (_arg ...) _body ...+))
]

@defexamples[
#:eval def-eval
(define (greet name)
  (string-append salutation ", " name))
(greet "John")
]

@def+int[
#:eval def-eval
(define (greet first [surname "Smith"] #:hi [hi salutation])
  (string-append hi ", " first " " surname))
(greet "John")
(greet "John" #:hi "Hey")
(greet "John" "Doe")
]

The function shorthand via @scheme[define] also supports a ``rest''
argument (i.e., a final argument to collect extra arguments in a
list):

@specform[(define (id arg ... . rest-id) body ...+)]{}

which is a shorthand

@schemeblock[
(define _id (lambda (_arg ... . _rest-id) _body ...+))
]

@defexamples[
#:eval def-eval
(define (avg . l)
  (/ (apply + l) (length l)))
(avg 1 2 3)
]

@;------------------------------------------------------------------------
@section{Curried Function Shorthand}

Consider the following @scheme[make-add-suffix] function that takes a
string and returns another function that takes a string:

@def+int[
#:eval def-eval
(define make-add-suffix
  (lambda (s2)
    (lambda (s) (string-append s s2))))
]

Although it's not common, result of @scheme[make-add-suffix] could be
called directly, like this:

@interaction[
#:eval def-eval
((make-add-suffix "!") "hello")
]

In a sense, @scheme[make-add-suffix] is a function takes two
arguments, but it takes them one at a time. A function that takes some
of its arguments and returns a function to consume more is sometimes
called a @defterm{curried function}.

Using the function-shorthand form of @scheme[define],
@scheme[make-add-suffix] can be written equivalently as

@schemeblock[
(define (make-add-suffix s2)
  (lambda (s) (string-append s s2)))
]

This shorthand reflects the shape of the function call
@scheme[(make-add-suffix "!")]. The @scheme[define] form further
supports a shorthand for defining curried functions that reflects
nested function calls:

@def+int[
#:eval def-eval
(define ((make-add-suffix s2) s)
  (string-append s s2))
((make-add-suffix "!") "hello")
]
@defs+int[
#:eval def-eval
[(define louder (make-add-suffix "!"))
 (define less-sure (make-add-suffix "?"))]
(less-sure "really")
(louder "really")
]

The full syntax of the function shorthand for @scheme[define] is as follows:

@specform/subs[(define (head args) body ...+)
               ([head id
                      (head args)]
                [args (code:line arg ...)
                      (code:line arg ... @#,schemeparenfont{.} rest-id)])]{}

The expansion of this shorthand has one nested @scheme[lambda] form
for each @scheme[_head] in the definition, where the innermost
@scheme[_head] corresponds to the outermost @scheme[lambda].


@;------------------------------------------------------------------------
@section[#:tag "multiple-values"]{Multiple Values and @scheme[define-values]}

A Scheme expression normally produces a single result, but some
expressions can produce multiple results. For example,
@scheme[quotient] and @scheme[remainder] each produce a single value,
but @scheme[quotient/remainder] produces the same two values at once:

@interaction[
#:eval def-eval
(quotient 13 3)
(remainder 13 3)
(quotient/remainder 13 3)
]

As shown above, the @tech{REPL} prints each result value on its own
line.

Multiple-valued functions can be implemented in terms of the
@scheme[values] function, which takes any number of values and
returns them as the results:

@interaction[
#:eval def-eval
(values 1 2 3)
]
@def+int[
#:eval def-eval
(define (split-name name)
  (let ([parts (regexp-split " " name)])
    (if (= (length parts) 2)
        (values (list-ref parts 0) (list-ref parts 1))
        (error "not a <first> <last> name"))))
(split-name "Adam Smith")
]

The @scheme[define-values] form binds multiple identifiers at once to
multiple results produced from a single expression:

@specform[(define-values (id ...) expr)]{}

The number of results produced by the @scheme[_expr] must match the
number of @scheme[_id]s.

@defexamples[
#:eval def-eval
(define-values (given surname) (split-name "Adam Smith"))
given
surname
]

A @scheme[define] form (that is not a function shorthand) is
equivalent to a @scheme[define-values] form with a single @scheme[_id].

@refdetails["define"]{definitions}

@;------------------------------------------------------------------------
@section[#:tag "intdefs"]{Internal Definitions}

When the grammar for a syntactic form specifies @scheme[_body], then
the corresponding form can be either a definition or an expression.
A definition as a @scheme[_body] is an @defterm{internal definition}.

All internal definitions in a @scheme[_body] sequence must appear
before any expression, and the last @scheme[_body] must be an
expression.

For example, the syntax of @scheme[lambda] is

@specform[
(lambda gen-formals
  body ...+)
]

so the following are valid instances of the grammar:

@schemeblock[
(lambda (f)                (code:comment @#,elem{no definitions})
  (printf "running\n")
  (f 0))

(lambda (f)                (code:comment @#,elem{one definition})
  (define (log-it what)
    (printf "~a\n"))
  (log-it "running")
  (f 0)
  (log-it "done"))

(lambda (f n)              (code:comment @#,elem{two definitions})
  (define (call n)
    (if (zero? n)
        (log-it "done")
        (begin
          (log-it "running")
          (f 0)
          (call (- n 1)))))
  (define (log-it what)
    (printf "~a\n"))
  (call f n))
]

Internal definitions in a particular @scheme[_body] sequence are
mutually recursive; that is, any definition can refer to any other
definition---as long as the reference isn't actually evaluated before
its definition takes place. If a definition is referenced too early,
the result is a special value @|undefined-const|.

@defexamples[
(define (weird)
  (define x x)
  x)
(weird)
]

A sequence of internal definitions using just @scheme[define] is
easily translated to an equivalent @scheme[letrec] form (as introduced
in the next section). However, other definition forms can appear as a
@scheme[_body], including @scheme[define-values], @scheme[define-struct] (see
@secref["define-struct"]) or @scheme[define-syntax] (see
@secref["macros"]).

@refdetails/gory["intdef-body"]{internal definitions}

@; ----------------------------------------------------------------------

@close-eval[def-eval]
