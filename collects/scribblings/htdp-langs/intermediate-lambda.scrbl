#lang scribble/doc
@(require "common.ss"
          "std-grammar.ss"
          "prim-ops.ss"
          (for-label lang/htdp-intermediate-lambda))

@(define-syntax-rule (bd intm-define intm-define-struct intm-local intm-letrec intm-let intm-let* intm-time)
   (begin
    (require (for-label lang/htdp-intermediate))
    (define intm-define (scheme define))
    (define intm-define-struct (scheme define-struct))
    (define intm-local (scheme local))
    (define intm-letrec (scheme letrec))
    (define intm-let (scheme let))
    (define intm-let* (scheme let*))
    (define intm-time (scheme time))))
@(bd intm-define intm-define-struct intm-local intm-letrec intm-let intm-let* intm-time)

@(define-syntax-rule (bbd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)
   (begin
    (require (for-label lang/htdp-beginner))
    (define beg-define (scheme define))
    (define beg-define-struct (scheme define-struct))
    (define beg-cond (scheme cond))
    (define beg-if (scheme if))
    (define beg-and (scheme and))
    (define beg-or (scheme or))
    (define beg-check-expect (scheme check-expect))
    (define beg-require (scheme require))))
@(bbd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)


@title[#:style 'toc #:tag "intermediate-lam"]{Intermediate Student with Lambda}

@declare-exporting[lang/htdp-intermediate-lambda]

@schemegrammar*+qq[
#:literals (define define-struct lambda λ cond else if and or empty true false require lib planet
            local let let* letrec time check-expect check-within check-error)
(check-expect check-within check-error require)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case
             library-require]
[definition (define (id id id ...) expr)
            (define id expr)
            (define-struct id (id ...))]
[expr (lambda (id id ...) expr)
      (λ (id id ...) expr)
      (local [definition ...] expr)
      (letrec ([id expr] ...) expr)
      (let ([id expr] ...) expr)
      (let* ([id expr] ...) expr)
      (code:line (expr expr expr ...) (code:comment @#,seclink["intermediate-lambda-call"]{function call}))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (if expr expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      (time expr)
      empty
      (code:line id (code:comment @#,seclink["intermediate-id"]{identifier}))
      (code:line prim-op (code:comment @#,seclink["intermediate-lambda-prim-op"]{primitive operation}))
      (code:line @#,elem{@schemevalfont{'}@scheme[_quoted]} (code:comment @#,seclink["beginner-abbr-quote"]{quoted value}))
      (code:line @#,elem{@schemevalfont{`}@scheme[_quasiquoted]} (code:comment @#,seclink["beginner-abbr-quasiquote"]{quasiquote}))
      number
      true
      false
      string
      character]
]

@|prim-nonterms|

@prim-ops['(lib "htdp-intermediate-lambda.ss" "lang") #'here]

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-lambda-define"]{@scheme[define]}

@deftogether[(
@defform[(define (id id id ...) expr)]
@defform/none[#:literals (define)
              (define id expr)]
)]{

The same as Intermediate's @|intm-define|. No special case is needed
for @scheme[lambda], since a @scheme[lambda] form is an expression.}

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-lambda"]{@scheme[lambda]}

@defform[(lambda (id id ...) expr)]{

Creates a function that takes as many arguments as given @scheme[id]s,
and whose body is @scheme[expr].}

@defform[(λ (id id ...) expr)]{

The Greek letter @scheme[λ] is a synonym for @scheme[lambda].}

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-lambda-call"]{Function Calls}

@defform/none[(expr expr expr ...)]{

Like a Beginning @seclink["beginner-call"]{function call}, except that
the function position can be an arbitrary expression---perhaps a
@scheme[lambda] expression or a @scheme[_prim-op].}

@defform[(#%app expr expr expr ...)]{

A function call can be written with @scheme[#%app], though it's
practically never written that way.}

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-lambda-prim-op"]{Primitive Operation Names}

@defform/none[prim-op]{

The name of a primitive operation can be used as an expression. It
produces a function version of the operation.}

@prim-op-defns['(lib "htdp-intermediate-lambda.ss" "lang") #'here '()]


@; ----------------------------------------------------------------------

@section[#:tag "intermediate-lambda-unchanged"]{Unchanged Forms}

@defform[(define-struct structid (fieldid ...))]{

The same as Intermediate's @|intm-define-struct|.}


@deftogether[(
@defform[(local [definition ...] expr)]
@defform[(letrec ([id expr-for-let] ...) expr)]
@defform[(let ([id expr-for-let] ...) expr)]
@defform[(let* ([id expr-for-let] ...) expr)]
)]{

The same as Intermediate's @|intm-local|, @|intm-letrec|, @|intm-let|,
and @|intm-let*|.}


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


@defform[(time expr)]{

The same as Intermediate's @|intm-time|.}


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
