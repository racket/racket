#lang scribble/doc
@(require "common.rkt"
          "match-grammar.rkt"
          (for-label mzlib/match))

@(begin
  (define-syntax-rule (bind id)
    (begin
     (require scheme/match)
     (define id (scheme match))))
  (bind scheme-match))

@mzlib[#:mode title match]

The @schememodname[mzlib/match] library provides a @scheme[match] form
similar to that of @schememodname[scheme/match], but with an different
(older and less extensible) syntax of patterns.

@defform/subs[(match val-expr clause ...)
              ([clause [pat expr ...+]
                       [pat (=> id) expr ...+]])]{

See @scheme-match from @schememodname[scheme/match] for a description
of matching. The grammar of @scheme[pat] for this @scheme[match] is as
follows:

@|match-grammar|}

@; ------------------------------------------------------------

@deftogether[(
@defform[(match-lambda clause ...)]
@defform[(match-lambda* clause ...)]
@defform[(match-let ([pat expr] ...) body ...+)]
@defform[(match-let* ([pat expr] ...) body ...+)]
@defform[(match-letrec ([pat expr] ...) body ...+)]
@defform[(match-define pat expr)]
)]{

Analogous to the combined forms from @scheme[scheme/match].}

@; ------------------------------------------------------------

@deftogether[(
@defform*[((define-match-expander id proc-expr)
           (define-match-expander id proc-expr proc-expr)
           (define-match-expander id proc-expr proc-expr proc-expr))]
@defparam[match-equality-test comp-proc (any/c any/c . -> . any)]
)]{

Analogous to the form and parameter from @scheme[scheme/match]. The
@scheme[define-match-expander] form, however, supports an extra
@scheme[proc-expr] as the middle one: an expander for use with
@scheme[match] from @schememodname[mzlib/match].}
