#lang scribble/manual
@(require "utils.rkt")

@title[#:tag "doclang"]{Document Language}

@defmodulelang[scribble/doclang]{The @racketmodname[scribble/doclang]
language provides everything from @racket[racket/base], except that it
replaces the @racket[#%module-begin] form.}

The @racketmodname[scribble/doclang] @racket[#%module-begin]
essentially packages the body of the module into a call to
@racket[decode], binds the result to @racket[doc], and exports
@racket[doc].

Any module-level form other than an expression (e.g., a
@racket[require] or @racket[define]) remains at the top level, and
the @racket[doc] binding is put at the end of the module. As usual, a
module-top-level @racket[begin] slices into the module top level.
