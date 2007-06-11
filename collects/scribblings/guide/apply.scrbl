#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "guide:application"]{Procedure Applications}

An expression of the form

@specsubform[
(_proc-expr _arg-expr ...)
]

is a procedure application when @scheme[_proc-expr] is not an
identifier that is bound as a transformer.

@section{Evaluation Order and Arity}

A procedure application is evaluated by first evaluating the
@scheme[_proc-expr] and all @scheme[_arg-expr]s in order (left to
right). Then, if @scheme[_proc-expr] produced a procedure that accepts
as many arguments as supplied @scheme[_arg-expr]s, the procedure is
applied. Otherwise, an exception is raised.

@examples[
(cons 1 null)
(+ 1 2 3)
(cons 1 2 3)
(1 2 3)
]

Some procedures, such as @scheme[cons], accept a fixed number of
arguments. Some procedures, such as @scheme[list], accept any number
of arguments. Some procedures accept a range of argument counts; for
example @scheme[substring] accepts either two or three arguments. A
procedure's @idefterm{arity} is the number(s) of arguments that it
accepts.

@;------------------------------------------------------------------------
@section{Keyword Arguments}

Some procedures accept @defterm{keyword arguments} in addition to
by-position arguments. For that case, an @scheme[_arg] can be an
@scheme[_arg-keyword _arg-expr] sequence instead of just a
@scheme[_arg-expr]:

@specform/subs[
(_proc-expr _arg ...)
([arg arg-expr
      (code:line arg-keyword arg-expr)])
]

For example,

@schemeblock[(go "super.ss" #:mode 'fast)]

calls the procedure bound to @scheme[go] with @scheme["super.ss"] as a
by-position argument, and with @scheme['fast] as an argument associated
with the @scheme[#:mode] keyword. Thus, a keyword is implicitly paired
with the expression that follows it. Since a keyword by itself is not
an expression, then

@schemeblock[(go "super.ss" #:mode #:fast)]

is a syntax error---because @scheme[#:fast] is not an expression.

The order of keyword @scheme[_arg]s determines the order in which
@scheme[_arg-expr]s are evaluated, but a procedure accepts (or
declines) keyword arguments independent of their position in the
argument list. That is, keyword arguments are recognized by an applied
procedure using only the associated keyword. The above call to
@scheme[go] can be equivalently written

@schemeblock[(go #:mode #:fast "super.ss")]

@refdetails["mz:application"]{procedure applications}

@;------------------------------------------------------------------------
@section[#:tag "guide:apply"]{The @scheme[apply] Procedure}

The syntax for procedure applications supports any number of
arguments, but a specific application expression always specifies a
fixed number of arguments. As a result, a procedure that takes a list
of arguments cannot directly apply a procedure like @scheme[+] to all
of the items in the list:

@def+int[
(define (avg lst) (code:comment #, @elem{doesn't work...})
  (/ (+ lst) (length lst)))
(avg '(1 2 3))
]

@def+int[
(define (avg lst) (code:comment #, @elem{doesn't always work...})
 (/ (+ (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))
    (length lst)))
(avg '(1 2 3))
(avg '(1 2))
]

The @scheme[apply] procedure offers a way around this restriction, It
takes another procedure a @italic{list} arguments, and it applies the
procedure to the arguments:

@def+int[
(define (avg lst)
  (/ (apply + lst) (length lst)))
(avg '(1 2 3))
(avg '(1 2))
(avg '(1 2 3 4))
]
