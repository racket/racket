#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/unit))

@(begin
   (define-syntax-rule (bind id)
     (begin
       (require (for-label racket/base))
       (define id (racket struct))))
   (bind racket-struct)
   (define-syntax-rule (bindc id)
     (begin
       (require (for-label racket/unit))
       (define id (racket struct/ctc))))
   (bindc racket-struct/ctc))

@mzlib[#:mode title unit #:use-sources ((submod racket/unit compat))]

@deprecated[@racketmodname[racket/unit]]{}

The @racketmodname[mzlib/unit] library mostly re-provides
@racketmodname[racket/unit], except for @racket-struct and
@racket-struct/ctc from @racketmodname[racket/unit].

@defform/subs[(struct id (field-id ...) omit-decl ...)
              ([omit-decl -type
                          -selectors
                          -setters
                          -constructor])]{

A signature form like @racket-struct from @racketmodname[racket/base],
but with a different syntax for options that limit exports.}

@defform/subs[(struct/ctc id ([field-id contract-expr] ...) omit-decl ...)
              ([omit-decl -type
                          -selectors
                          -setters
                          -constructor])]{

A signature form like @racket-struct/ctc from @racketmodname[racket/unit],
but with a different syntax for the options that limit exports.}

@deftogether[(
@defidform[struct~r]
@defidform[struct~r/ctc]
)]{

The same as @|racket-struct| from @racketmodname[racket/base] and @|racket-struct/ctc| from
@racketmodname[racket/unit].}

@deftogether[(
@defidform[struct~s]
@defidform[struct~s/ctc]
)]{

Like @racket[struct~r] and @racket[struct~r/ctc], but the constructor is
named the same as the type, instead of with  @racketidfont{make-} prefix.}
