#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/plt-match))

@mzlib[#:mode title plt-match]

The @schememodname[mzlib/plt-match] library mostly re-provides
@scheme[scheme/match].

@deftogether[(
@defform*[((define-match-expander id proc-expr)
           (define-match-expander id proc-expr proc-expr)
           (define-match-expander id proc-expr proc-expr proc-expr))]
)]{

The same as the form from @schememodname[mzlib/match].}
