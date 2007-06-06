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
identifier that is bound as a transformer. The @scheme[...] in this
syntactic sketch means ``zero or more repetitions of the preceding
element.'' Specifically, it means zero or more @scheme[_arg-expr]s.

A procedure application is evaluated by first evaluating the
@scheme[_proc-expr] and all @scheme[_arg-expr]s in order (left to
right). Then, if @scheme[_proc-expr] produced a procedure that accepts
as many arguments as supplied @scheme[_arg-expr]s, the procedure is
aplied. Otherwise, an exception is raised.

@examples[
(cons 1 null)
((lambda (x y z) (+ x y z)) 1 2 3)
(cons 1 2 3)
(1 2 3)
]

Some procedures, such as @scheme[cons], accept a fixed number of
arguments. Some procedures, such as @scheme[list], accept any number
of arguments. Some procedures accept a range of argument counts; for
example @scheme[substring] accepts either two or three arguments.

Some procedures accept @defterm{keyword arguments} in addition to
by-position arguments. To supply a keyword argument, include a keyword
followed by the addociated argument value. The order of keyword
arguments does not matter, nor does their position relative to
non-keyword arguments (as long as they follow the procedure
expression); keyword arguments are recognized by an applied procedure
using only the associated keyword, not the order.

@examples[
(need-an-example 1 #:arg 2)
(need-an-example #:arg 2 1)
]

@refdetails["mz:application"]{procedure applications}
