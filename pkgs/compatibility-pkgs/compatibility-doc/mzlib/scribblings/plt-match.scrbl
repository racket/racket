#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/plt-match))

@mzlib[#:mode title plt-match]

@deprecated[@racketmodname[racket/match]]{}

The @racketmodname[mzlib/plt-match] library mostly re-provides
@racket[scheme/match].

@deftogether[(
@defform*[((define-match-expander id proc-expr)
           (define-match-expander id proc-expr proc-expr)
           (define-match-expander id proc-expr proc-expr proc-expr))]
)]{

The same as the form from @racketmodname[mzlib/match].}
