#lang scribble/doc
@(require "mz.rkt" (for-label racket/splicing racket/stxparam racket/local))

@(define splice-eval (make-base-eval))
@examples[#:hidden #:eval splice-eval (require racket/splicing 
                                               racket/stxparam
                                               (for-syntax racket/base))]

@title[#:tag "splicing"]{Local Binding with Splicing Body}

@note-lib-only[racket/splicing]

@deftogether[(
@defidform[splicing-let]
@defidform[splicing-letrec]
@defidform[splicing-let-values]
@defidform[splicing-letrec-values]
@defidform[splicing-let-syntax]
@defidform[splicing-letrec-syntax]
@defidform[splicing-let-syntaxes]
@defidform[splicing-letrec-syntaxes]
@defidform[splicing-letrec-syntaxes+values]
@defidform[splicing-local]
@defidform[splicing-parameterize]
)]{

Like @racket[let], @racket[letrec], @racket[let-values],
@racket[letrec-values], @racket[let-syntax], @racket[letrec-syntax],
@racket[let-syntaxes], @racket[letrec-syntaxes],
@racket[letrec-syntaxes+values], @racket[local], and
@racket[parameterize], except that in a definition context, the body
forms are spliced into the enclosing definition context (in the same
way as for @racket[begin]).

@examples[
#:eval splice-eval
(splicing-let-syntax ([one (lambda (stx) #'1)])
  (define o one))
o
(eval:error one)
]

When a splicing binding form occurs in a @tech{top-level context} or
@tech{module context}, its local bindings are treated similarly to
definitions. In particular, syntax bindings are
evaluated every time the module is @tech{visit}ed, instead of only
once during compilation as in @racket[let-syntax], etc.

@examples[
#:eval splice-eval
(eval:error
 (splicing-letrec ([x bad]
                   [bad 1])
   x))]

If a definition within a splicing form is intended to be local to the
splicing body, then the identifier should have a true value for the
@racket['definition-intended-as-local] @tech{syntax property}. For
example, @racket[splicing-let] itself adds the property to
locally-bound identifiers as it expands to a sequence of definitions,
so that nesting @racket[splicing-let] within a splicing form works as
expected (without any ambiguous bindings).

@history[
 #:changed "6.12.0.2" @elem{Added @racket[splicing-parameterize].}]}


@defidform[splicing-syntax-parameterize]{

Like @racket[syntax-parameterize], except that in a definition context, the body
forms are spliced into the enclosing definition context (in the same way as for
@racket[begin]). In a definition context, the body of
@racket[splicing-syntax-parameterize] can be empty.

Note that @tech{require transformers} and @tech{provide transformers} are not
affected by syntax parameterization.  While all uses of @racket[require] and
@racket[provide] will be spliced into the enclosing context, derived import or
export specifications will expand as if they had not been inside of the
@racket[splicing-syntax-parameterize].

Additionally, @tech{submodules} defined with @racket[module*] that specify
@racket[#f] in place of a @tech{module path} @emph{are} affected by syntax
parameterization, but other submodules (those defined with @racket[module] or
@racket[module*] with a @tech{module path}) are @emph{not}.

@examples[
#:eval splice-eval
(define-syntax-parameter place (lambda (stx) #'"Kansas"))
(define-syntax-rule (where) `(at ,(place)))
(where)
(splicing-syntax-parameterize ([place (lambda (stx) #'"Oz")])
  (define here (where)))
here
]

@history[
 #:changed "6.11.0.1"
 @elem{Modified to syntax parameterize @racket[module*] submodules that
       specify @racket[#f] in place of a @tech{module path}.}]}

@; ----------------------------------------------------------------------

@close-eval[splice-eval]
