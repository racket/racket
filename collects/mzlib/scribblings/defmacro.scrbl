#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/defmacro
                     (only-in scheme/base syntax->datum datum->syntax)))

@(define ref '(lib "scribblings/reference/reference.scrbl"))

@mzlib[#:mode title defmacro]

@deftogether[(
@defform*[[(define-macro id expr)
           (define-macro (id . formals) body ...+)]]
@defform/subs[(defmacro id formals body ...+)
              ([formals (id ...)
                        id
                        (id ...+ . id)])]
)]{

Defines a (non-hygienic) macro @scheme[id] through a procedure that
manipulates S-expressions, as opposed to @techlink[#:doc ref]{syntax
objects}.

In the first form, @scheme[expr] must produce a procedure. In the
second form, @scheme[formals] determines the formal arguments of the
procedure, as in @scheme[lambda], and the @scheme[expr]s are the
procedure body. The last form, with @scheme[defmacro], is like the
second form, but with slightly different parentheses.

In all cases, the procedure is generated in the @techlink[#:doc
ref]{transformer environment}, not the normal environment.

In a use of the macro,

@schemeblock[
(id datum ...)
]

@scheme[syntax->datum] is applied to the expression, and the
transformer procedure is applied to the @scheme[cdr] of the resulting
list. If the number of @scheme[datum]s does not match the procedure's
arity, or if @scheme[id] is used in a context that does not match the
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
macro result is converted to syntax with @scheme[datum->syntax]. The
original expression supplies the lexical context and source location
for converted elements.

@bold{Important:} Although @scheme[define-macro] is non-hygienic, it
is still restricted by Racket's phase separation rules.  This
means that a macro cannot access run-time bindings, because it is
executed in the syntax-expansion phase.  Translating code that
involves @scheme[define-macro] or @scheme[defmacro] from an
implementation without this restriction usually implies separating
macro related functionality into a @scheme[begin-for-syntax] or a
module (that will be imported with @scheme[require-for-syntax]) and
properly distinguishing syntactic information from run-time
information.}
