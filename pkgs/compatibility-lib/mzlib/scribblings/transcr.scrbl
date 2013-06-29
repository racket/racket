#lang scribble/doc
@(require "common.rkt")

@mzlib[#:mode title transcr]

The @racket[transcript-on] and @racket[transcript-off] procedures of
@racketmodname[mzscheme] always raise
@racket[exn:fail:unsupported]. The @racketmodname[mzlib/transcr]
library provides working versions of @racket[transcript-on] and
@racket[transcript-off].

@(define-syntax-rule (go)
   (begin
     (require (for-label mzlib/transcr))

     @deftogether[(
     @defproc[(transcript-on [filename any/c]) any]
     @defproc[(transcript-off) any]
     )]{

   Starts/stops recording a transcript at @racket[filename].}))
@(go)
