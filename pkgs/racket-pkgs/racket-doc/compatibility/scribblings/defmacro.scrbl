#lang scribble/doc
@(require scribblings/reference/mz
          (for-label compatibility/defmacro
                     syntax/parse syntax/parse/define))

@title[#:tag "defmacro"]{Legacy macro support}

@defmodule[compatibility/defmacro]

This @racketmodname[compatibility/defmacro] library provides support for
writing legacy macros. Support for @racket[defmacro] is provided
primarily for porting code from other languages (e.g., some
implementations of Scheme or Common Lisp) that use symbol-based
macro systems.

Use of @racket[defmacro] for modern Racket code is @bold{@italic{strongly}}
discouraged. Instead, consider using @racket[syntax-parse] or
@racket[define-simple-macro].

@deftogether[(
@defform*[[(define-macro id expr)
           (define-macro (id . formals) body ...+)]]
@defform/subs[(defmacro id formals body ...+)
              ([formals (id ...)
                        id
                        (id ...+ . id)])]
)]{

Defines a (non-hygienic) macro @racket[id] through a procedure that
manipulates S-expressions, as opposed to
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax objects}.

In the first form, @racket[expr] must produce a procedure. In the
second form, @racket[formals] determines the formal arguments of the
procedure, as in @racket[lambda], and the @racket[expr]s are the
procedure body. The last form, with @racket[defmacro], is like the
second form, but with slightly different parentheses.

In all cases, the procedure is generated in the
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
transformer environment}, not the normal environment.

In a use of the macro,

@racketblock[
(id datum ...)
]

@racket[syntax->datum] is applied to the expression, and the
transformer procedure is applied to the @racket[cdr] of the resulting
list. If the number of @racket[datum]s does not match the procedure's
arity, or if @racket[id] is used in a context that does not match the
above pattern, then a syntax error is reported.

After the macro procedure returns, the result is compared to the
procedure's arguments. For each value that appears exactly once within
the arguments (or, more precisely, within the S-expression derived
from the original source syntax), if the same value appears in the
result, it is replaced with a syntax object from the original
expression. This heuristic substitution preserves source location
information in many cases, despite the macro procedure's operation on
raw S-expressions.

After substituting syntax objects for preserved values, the entire
macro result is converted to syntax with @racket[datum->syntax]. The
original expression supplies the lexical context and source location
for converted elements.

@bold{Important:} Although @racket[define-macro] is non-hygienic, it
is still restricted by Racket's phase separation rules.  This
means that a macro cannot access run-time bindings, because it is
executed in the syntax-expansion phase.  Translating code that
involves @racket[define-macro] or @racket[defmacro] from an
implementation without this restriction usually implies separating
macro related functionality into a @racket[begin-for-syntax] or a
module (that will be imported with @racket[for-syntax]) and
properly distinguishing syntactic information from run-time
information.}
