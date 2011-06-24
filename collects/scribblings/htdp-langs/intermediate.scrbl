#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-intermediate))

@(define-syntax-rule (bd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)
   (begin
    (require (for-label lang/htdp-beginner))
    (define beg-define (racket define))
    (define beg-define-struct (racket define-struct))
    (define beg-cond (racket cond))
    (define beg-if (racket if))
    (define beg-and (racket and))
    (define beg-or (racket or))
    (define beg-check-expect (racket check-expect))
    (define beg-require (racket require))))
@(bd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)

@title[#:style 'toc #:tag "intermediate"]{Intermediate Student}

@declare-exporting[lang/htdp-intermediate]

@racketgrammar*+qq[
#:literals (define define-struct lambda cond else if and or empty true false require lib planet
            local let let* letrec time check-expect check-within check-error)
(check-expect check-within check-error require)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case
             library-require]
[definition (define (id id id ...) expr)
            (define id expr)
            (define id (lambda (id id ...) expr))
            (define-struct id (id ...))]
[expr (local [definition ...] expr)
      (letrec ([id expr-for-let] ...) expr)
      (let ([id expr-for-let] ...) expr)
      (let* ([id expr-for-let] ...) expr)
      (code:line (id expr expr ...) (code:comment @#,seclink["intermediate-call"]{function call}))
      (code:line (prim-op expr ...) (code:comment @#,seclink["beginner-prim-call"]{primitive operation call}))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (if expr expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      (time expr)
      empty
      (code:line id (code:comment @#,seclink["intermediate-id"]{identifier}))
      (code:line prim-op (code:comment @#,seclink["intermediate-prim-op"]{primitive operation}))
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]} (code:comment @#,seclink["beginner-abbr-quote"]{quoted value}))
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]} (code:comment @#,seclink["beginner-abbr-quasiquote"]{quasiquote}))
      number
      true
      false
      string
      character]
[expr-for-let (lambda (id id ...) expr)
              expr]
]

@|prim-nonterms|

@prim-ops['(lib "htdp-intermediate.rkt" "lang") #'here]

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-define"]{@racket[define]}

@deftogether[(
@defform[(define (id id id ...) expr)]
@defform/none[#:literals (define)
              (define id expr)]
@defform/none[#:literals (define lambda)
              (define id (lambda (id id ...) expr))]
)]{
Besides working in @racket[local], definition forms are
the same as Beginning's @|beg-define|.}

@defidform[lambda]{

As in Beginning, @racket[lambda] keyword can only be used with
@racket[define] in the alternative function-definition syntax.}

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-define-struct"]{@racket[define-struct]}

@defform[(define-struct structid (fieldid ...))]{

Besides working in @racket[local], this form is the same as Beginning's
@|beg-define-struct|.}

@; ----------------------------------------------------------------------

@section{@racket[local]}

@defform[(local [definition ...] expr)]{

Groups related definitions for use in @racket[expr]. Each
@racket[definition] is evaluated in order, and finally the body
@racket[expr] is evaluated. Only the expressions within the
@racket[local] form (including the right-hand-sides of the
@racket[definition]s and the @racket[expr]) may refer to the names
defined by the @racket[definition]s. If a name defined in the
@racket[local] form is the same as a top-level binding, the inner one
``shadows'' the outer one. That is, inside the @racket[local] form,
any references to that name refer to the inner one.

Since @racket[local] is an expression and may occur anywhere an
expression may occur, it introduces the notion of lexical
scope. Expressions within the local may ``escape'' the scope of the
local, but these expressions may still refer to the bindings
established by the local.}

@; ----------------------------------------------------------------------

@section{@racket[letrec], @racket[let], and @racket[let*]}

@defform[(letrec ([id expr-for-let] ...) expr)]{

Similar to @racket[local], but essentially omitting the
@racket[define] for each definition.

A @racket[expr-for-let] can be either an expression for a constant
definition or a @racket[lambda] form for a function definition.}

@defform[(let ([id expr-for-let] ...) expr)]{

Like @racket[letrec], but the defined @racket[id]s can be used only in
the last @racket[expr], not the @racket[expr-for-let]s next to the
@racket[id]s.}

@defform[(let* ([id expr-for-let] ...) expr)]{

Like @racket[let], but each @racket[id] can be used in any subsequent
@racket[expr-for-let], in addition to @racket[expr].}

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-call"]{Function Calls}

@defform/none[(id expr expr ...)]{

A function call in Intermediate is the same as a Beginning
@seclink["beginner-call"]{function call}, except that it can also call
@racket[local]ly defined functions or functions passed as
arguments. That is, @racket[id] can be a function defined in
@racket[local] or an argument name while in a function.}

@defform[(#%app id expr expr ...)]{

A function call can be written with @racket[#%app], though it's
practically never written that way.}

@; ----------------------------------------------------------------------

@section{@racket[time]}

@defform[(time expr)]{

This form is used to measure the time taken to evaluate
@racket[expr]. After evaluating @racket[expr], Racket prints out the
time taken by the evaluation (including real time, time taken by the
cpu, and the time spent collecting free memory) and returns the result
of the expression.

(The reported time is measured as the number of milliseconds of CPU time
required to obtain this result, the number of “real” milliseconds required
for the result, and the number of milliseconds of CPU time (included in the
first result) spent on garbage collection. The reliability of the timing
numbers depends on the platform.) 
}

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-id"]{Identifiers}

@defform/none[id]{

An @racket[id] refers to a defined constant (possibly local), defined
function (possibly local), or argument within a function body. If no
definition or argument matches the @racket[id] name, an error is
reported.}

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-prim-op"]{Primitive Operations}

@defform/none[prim-op]{

The name of a primitive operation can be used as an expression. If it
is passed to a function, then it can be used in a function call within
the function's body.}

@prim-op-defns['(lib "htdp-intermediate.rkt" "lang") #'here '()]

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-unchanged"]{Unchanged Forms}

@deftogether[(
@defform[(cond [expr expr] ... [expr expr])]
@defidform[else]
)]{

The same as Beginning's @|beg-cond|.}


@defform[(if expr expr expr)]{

The same as Beginning's @|beg-if|.}

@deftogether[(
@defform[(and expr expr expr ...)]
@defform[(or expr expr expr ...)]
)]{

The same as Beginning's @|beg-and| and @|beg-or|.}


@deftogether[(
@defform[(check-expect expr expr)]
@defform[(check-within expr expr expr)]
@defform*[[(check-error expr expr)
           (check-error expr)]]
@defform[(check-member-of expr expr expr ...)]
@defform[(check-range expr expr expr)]
)]{

The same as Beginning's @|beg-check-expect|, etc.}


@deftogether[(
@defthing[empty empty?]
@defthing[true boolean?]
@defthing[false boolean?]
)]{

Constants for the empty list, true, and false.}

@defform[(require module-path)]{

The same as Beginning's @|beg-require|.}
