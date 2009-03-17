#lang scribble/doc
@(require "mz.ss"
          (for-label scheme/splicing
                     scheme/stxparam))

@(define splice-eval (make-base-eval))
@interaction-eval[#:eval splice-eval (require scheme/splicing 
                                              scheme/stxparam
                                              (for-syntax scheme/base))]

@title[#:tag "splicing"]{Local Binding with Splicing Body}

@note-lib-only[scheme/splicing]

@deftogether[(
@defidform[splicing-let]
@defidform[splicing-letrec]
@defidform[splicing-let-values]
@defidform[splicing-letrec-values]
@defidform[splicing-let-syntax]
@defidform[splicing-letrec-syntax]
@defidform[splicing-let-syntaxes]
@defidform[splicing-letrec-syntaxes]
)]{

Like @scheme[let], @scheme[letrec], @scheme[let-values],
@scheme[letrec-values], @scheme[let-syntax], @scheme[letrec-syntax],
@scheme[let-syntaxes], and @scheme[letrec-syntaxes], except that in a
definition context, the body forms are spliced into the enclosing
definition context (in the same as as for @scheme[begin]). Also, for
@scheme[splicing-letrec] and @scheme[splicing-letrec-values], a
reference to a bound identifiers before is initialized is treated in
the same way as definition in the enclosing context, which may be
different than for @scheme[letrec] and @scheme[letrec-values].

@examples[
#:eval splice-eval
(splicing-let-syntax ([one (lambda (stx) #'1)])
  (define o one))
o
one
]}

@defidform[splicing-syntax-parameterize]{

Like @scheme[syntax-parameterize], except that in a definition
context, the body forms are spliced into the enclosing definition
context (in the same as as for @scheme[begin]), as long as the body
forms are valid in an internal-definition context. In particular,
@scheme[require] and @scheme[provide] forms cannot appear in the body
of @scheme[splicing-syntax-parameterize], even if
@scheme[splicing-syntax-parameterize] is used in a @scheme[module]
body.

@examples[
#:eval splice-eval
(define-syntax-parameter place (lambda (stx) #'"Kansas"))
(define-syntax-rule (where) `(at ,(place)))
(where)
(splicing-syntax-parameterize ([place (lambda (stx) #'"Oz")])
  (define here (where)))
here
]}

@; ----------------------------------------------------------------------

@close-eval[splice-eval]
