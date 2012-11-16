#lang scribble/doc
@(require "common.rkt"
          "match-grammar.rkt"
          (for-label mzlib/match))

@(begin
  (define-syntax-rule (bind id)
    (begin
      (require racket/match)
      (define id (racket match))))
  (bind racket-match))

@mzlib[#:mode title match]

@deprecated[@racketmodname[racket/match]]{}

The @racketmodname[mzlib/match] library provides a @racket[match] form
similar to that of @racketmodname[racket/match], but with an different
(older and less extensible) syntax of patterns.

@defform/subs[(match val-expr clause ...)
              ([clause [pat expr ...+]
                       [pat (=> id) expr ...+]])]{

See @racket-match from @racketmodname[racket/match] for a description
of matching. The grammar of @racket[pat] for this @racket[match] is as
follows:

@|match-grammar|}

@; ------------------------------------------------------------

@deftogether[(
@defform[(define/match (head args) match*-clause ...)]
@defform[(match-lambda clause ...)]
@defform[(match-lambda* clause ...)]
@defform[(match-let ([pat expr] ...) body ...+)]
@defform[(match-let* ([pat expr] ...) body ...+)]
@defform[(match-letrec ([pat expr] ...) body ...+)]
@defform[(match-define pat expr)]
)]{

Analogous to the combined forms from @racket[racket/match].}

@; ------------------------------------------------------------

@deftogether[(
@defform*[((define-match-expander id proc-expr)
           (define-match-expander id proc-expr proc-expr)
           (define-match-expander id proc-expr proc-expr proc-expr))]
@defparam[match-equality-test comp-proc (any/c any/c . -> . any)]
)]{

Analogous to the form and parameter from @racket[racket/match]. The
@racket[define-match-expander] form, however, supports an extra
@racket[proc-expr] as the middle one: an expander for use with
@racket[match] from @racketmodname[mzlib/match].}
