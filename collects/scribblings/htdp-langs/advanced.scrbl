#lang scribble/doc
@(require "common.ss"
          "std-grammar.ss"
          "prim-ops.ss"
          (for-label lang/htdp-advanced))

@(define-syntax-rule (bd intm-define intm-define-struct intm-lambda intm-let)
   (begin
    (require (for-label lang/htdp-intermediate-lambda))
    (define intm-define (scheme define))
    (define intm-define-struct (scheme define-struct))
    (define intm-lambda (scheme lambda))
    (define intm-let (scheme let))))
@(bd intm-define intm-define-struct intm-lambda intm-let)


@title[#:style 'toc]{Advanced Student}

@schemegrammar*+qq[
#:literals (define define-struct lambda cond else if and or empty true false require lib planet
            local let let* letrec time begin begin0 set! delay shared recur when case unless)
[program def-or-expr]
[def-or-expr definition
             expr
             library-require]
[definition (define (id id id ...) expr)
            (define id expr)
            (define-struct id (id ...))]
[expr (begin expr expr ...)
      (begin0 expr expr ...)
      (set! id expr)
      (delay expr)
      (lambda (id id ...) expr)
      (local [definition ...] expr)
      (letrec ([id expr] ...) expr)
      (shared ([id expr] ...) expr)
      (let ([id expr] ...) expr)
      (let id ([id expr] ...) expr)
      (let* ([id expr] ...) expr)
      (recur id ([id expr] ...) expr)
      (code:line (expr expr expr ...) (code:comment #, @seclink["intermediate-lambda-call"]{function call}))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (case expr [(choice choice ...) expr] ... 
                 [(choice choice ...) expr])
      (case expr [(choice choice ...) expr] ... 
                 [else expr])
      (if expr expr expr)
      (when expr expr)
      (unless expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      (time expr)
      empty
      (code:line id (code:comment #, @seclink["intermediate-id"]{identifier}))
      (code:line prim-op (code:comment #, @seclink["intermediate-lambda-prim-op"]{primitive operation}))
      'id
      (code:line #, @elem{@schemevalfont{'}@scheme[quoted]} (code:comment #, @seclink["beginner-abbr-quote"]{quoted value}))
      (code:line #, @elem{@schemevalfont{`}@scheme[quasiquoted]} (code:comment #, @seclink["beginner-abbr-quasiquote"]{quasiquote}))
      number
      true
      false
      string
      character]
[choice (code:line id (code:comment #, @t{treated as a symbol}))
        number]
]

@|prim-nonterms|

@prim-ops['(lib "htdp-advanced.ss" "lang") #'here]

@; ----------------------------------------------------------------------

@section[#:tag "advanced-define"]{@scheme[define]}

@deftogether[(
@defform[(define (id id ...) expr)]
@defform/none[#:literals (define)
              (define id expr)]
)]{

The same as Intermediate with Lambda's @|intm-define|, except that a
function is allowed to accept zero arguments.}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-define-struct"]{@scheme[define-struct]}

@defform[(define-struct structid (fieldid ...))]{

The same as Intermediate's @|intm-define-struct|, but defines an
additional set of operations:

@itemize{

 @item{@schemeidfont{make-}@scheme[structid] : takes a number of
       arguments equal to the number of fields in the structure type,
       and creates a new instance of the structure type.}

 @item{@schemeidfont{set-}@scheme[structid]@schemeidfont{-}@scheme[fieldid]@schemeidfont{!}
       : takes an instance of the structure and a value, and changes
       the instance's field to the given value.}

}}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-lambda"]{@scheme[lambda]}

@defform[(lambda (id ...) expr)]{

The same as Intermediate with Lambda's @|intm-lambda|, except that a
function is allowed to accept zero arguments.}

@; ----------------------------------------------------------------------

@section{@scheme[begin]}

@defform[(begin expr expr ...)]{

Evaluates the @scheme[expr]s in order from left to right. The value of
the @scheme[begin] expression is the value of the last @scheme[expr].}

@; ----------------------------------------------------------------------

@section{@scheme[begin0]}

@defform[(begin0 expr expr ...)]{

Evaluates the @scheme[expr]s in order from left to right. The value of
the @scheme[begin] expression is the value of the first @scheme[expr].}

@; ----------------------------------------------------------------------

@section{@scheme[set!]}

@defform[(set! id expr)]{

Evaluates @scheme[expr], and then changes the definition @scheme[id]
to have @scheme[expr]'s value. The @scheme[id] must be defined or
bound by @scheme[letrec], @scheme[let], or @scheme[let*].}

@; ----------------------------------------------------------------------

@section{@scheme[delay]}

@defform[(delay expr)]{

Produces a ``promise'' to evaluate @scheme[expr]. The @scheme[expr] is
not evaluated until the promise is forced through the @scheme[force]
operator; when the promise is forced, the result is recorded, so that
any further @scheme[force] of the promise always produces the
remembered value.}

@; ----------------------------------------------------------------------

@section{@scheme[shared]}

@defform[(shared ([id expr] ...) expr)]{

Like @scheme[letrec], but when an @scheme[expr] next to an @scheme[id]
is a @scheme[cons], @scheme[list], @scheme[vector], quasiquoted
expression, or @schemeidfont{make-}@scheme[_structid] from a
@scheme[define-struct], the @scheme[expr] can refer directly to any
@scheme[id], not just @scheme[id]s defined earlier. Thus,
@scheme[shared] can be used to create cyclic data structures.}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-let"]{@scheme[let]}

@defform*[[(let ([id expr] ...) expr)
           (let id ([id expr] ...) expr)]]{

The first form of @scheme[let] is the same as Intermediate's
@|intm-let|.

The second form is equivalent to a @scheme[recur] form.}


@; ----------------------------------------------------------------------

@section{@scheme[recur]}

@defform[(recur id ([id expr] ...) expr)]{

A short-hand recursion construct. The first @scheme[id] corresponds to
the name of the recursive function. The parenthesized @scheme[id]s are
the function's arguments, and each corresponding @scheme[expr] is a
value supplied for that argument in an initial starting call of the
function. The last @scheme[expr] is the body of the function.

More precisely, a @scheme[recur] form 

@schemeblock[
(recur func-id ([arg-id arg-expr] (unsyntax @schemeidfont{...}))
  body-expr)
]

is equivalent to

@schemeblock[
((local [(define (func-id arg-id (unsyntax @schemeidfont{...}))
           body-expr)]
   func-id)
  arg-expr (unsyntax @schemeidfont{...}))
]}

@; ----------------------------------------------------------------------

@section{@scheme[case]}

@defform[(case expr [(choice ...) expr] ... [(choice ...) expr])]{

A @scheme[case] form contains one or more ``lines'' that are
surrounded by parentheses or square brackets. Each line contains a
sequence of choices---numbers and names for symbols---and an answer
@scheme[expr]. The initial @scheme[expr] is evaluated, and the
resulting value is compared to the choices in each line, where the
lines are considered in order. The first line that contains a matching
choice provides an answer @scheme[expr] whose value is the result of
the whole @scheme[case] expression. If none of the lines contains a
matching choice, it is an error.}

@defform/none[#:literals (cond else)
              (cond expr [(choice ...) expr] ... [else expr])]{

This form of @scheme[case] is similar to the prior one, except that
the final @scheme[else] clause is always taken if no prior line
contains a choice matching the value of the initial @scheme[expr]. In
other words, so there is no possibility to ``fall off them end'' of
the @scheme[case] form.}

@; ----------------------------------------------------------------------

@section{@scheme[when] and @scheme[unless]}

@defform[(when expr expr)]{

The first @scheme[expr] (known as the ``test'' expression) is
evaluated. If it evaluates to @scheme[true], the result of the
@scheme[when] expression is the result of evaluating the second
@scheme[expr], otherwise the result is @scheme[(void)] and the second
@scheme[expr] is not evaluated. If the result of evaluating the test
@scheme[expr] is neither @scheme[true] nor @scheme[false], it is an
error.}

@defform[(unless expr expr)]{

Like @scheme[when], but the second @scheme[expr] is evaluated when the
first @scheme[expr] produces @scheme[false] instead of @scheme[true].}

@; ----------------------------------------

@section[#:tag "advanced-prim-ops"]{Primitive Operations}

The following primitives extend the set available though
@seclink["intermediate-prim-op"]{Intermediate}.

@prim-op-defns['(lib "htdp-advanced.ss" "lang") 
               #'here
               '((lib "htdp-beginner.ss" "lang") (lib "htdp-intermediate.ss" "lang"))]
