#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "application"]{Function Calls@aux-elem{ (Procedure Applications)}}

An expression of the form

@specsubform[
(proc-expr arg-expr ...)
]

is a function call---also known as a @defterm{procedure
application}---when @scheme[_proc-expr] is not an identifier that is
bound as a syntax transformer (such as @scheme[if] or
@scheme[define]).

@section{Evaluation Order and Arity}

A function call is evaluated by first evaluating the
@scheme[_proc-expr] and all @scheme[_arg-expr]s in order (left to
right). Then, if @scheme[_proc-expr] produces a function that accepts
as many arguments as supplied @scheme[_arg-expr]s, the function is
called. Otherwise, an exception is raised.

@examples[
(cons 1 null)
(+ 1 2 3)
(cons 1 2 3)
(1 2 3)
]

Some functions, such as @scheme[cons], accept a fixed number of
arguments. Some functions, such as @scheme[+] or @scheme[list], accept
any number of arguments. Some functions accept a range of argument
counts; for example @scheme[substring] accepts either two or three
arguments. A function's @idefterm{arity} is the number of arguments
that it accepts.

@;------------------------------------------------------------------------
@section[#:tag "keyword-args"]{Keyword Arguments}

Some functions accept @defterm{keyword arguments} in addition to
by-position arguments. For that case, an @scheme[_arg] can be an
@scheme[_arg-keyword _arg-expr] sequence instead of just a
@scheme[_arg-expr]:

@guideother{@secref["keywords"] introduces keywords.}

@specform/subs[
(_proc-expr arg ...)
([arg arg-expr
      (code:line arg-keyword arg-expr)])
]

For example,

@schemeblock[(go "super.ss" #:mode 'fast)]

calls the function bound to @scheme[go] with @scheme["super.ss"] as a
by-position argument, and with @scheme['fast] as an argument
associated with the @scheme[#:mode] keyword. A keyword is implicitly
paired with the expression that follows it.

Since a keyword by itself is not an expression, then

@schemeblock[(go "super.ss" #:mode #:fast)]

is a syntax error. The @scheme[#:mode] keyword must be followed by an
expression to produce an argument value, and @scheme[#:fast] is not an
expression.

The order of keyword @scheme[_arg]s determines the order in which
@scheme[_arg-expr]s are evaluated, but a function accepts keyword
arguments independent of their position in the argument list. The
above call to @scheme[go] can be equivalently written

@schemeblock[(go #:mode 'fast "super.ss")]

@refdetails["application"]{procedure applications}

@;------------------------------------------------------------------------
@section[#:tag "apply"]{The @scheme[apply] Function}

The syntax for function calls supports any number of arguments, but a
specific call always specifies a fixed number of arguments. As a
result, a function that takes a list of arguments cannot directly
apply a function like @scheme[+] to all of the items in the list:

@def+int[
(define (avg lst) (code:comment @#,elem{doesn't work...})
  (/ (+ lst) (length lst)))
(avg '(1 2 3))
]

@def+int[
(define (avg lst) (code:comment @#,elem{doesn't always work...})
 (/ (+ (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))
    (length lst)))
(avg '(1 2 3))
(avg '(1 2))
]

The @scheme[apply] function offers a way around this restriction. It
takes a function and a @italic{list} arguments, and it applies the
function to the arguments:

@def+int[
(define (avg lst)
  (/ (apply + lst) (length lst)))
(avg '(1 2 3))
(avg '(1 2))
(avg '(1 2 3 4))
]

As a convenience, the @scheme[apply] function accepts additional
arguments between the function and the list. The additional arguments
are effectively @scheme[cons]ed onto the argument list:

@def+int[
(define (anti-sum lst)
  (apply - 0 lst))
(anti-sum '(1 2 3))
]

The @scheme[apply] function supports only by-position arguments. To
apply a function with keyword arguments, use the
@scheme[keyword-apply] function, which accepts a function to apply
and three lists. The first two lists are in parallel, where the first
list contains keywords (sorted by @scheme[keyword<]), and the second
list contains a corresponding argument for each keyword. The third
list contains by-position function arguments, as for @scheme[apply].

@schemeblock[
(keyword-apply go
               '(#:mode)
               '(fast)
               '("super.ss"))
]
