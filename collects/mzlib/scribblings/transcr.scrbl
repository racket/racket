#lang scribble/doc
@(require "common.rkt")

@mzlib[#:mode title transcr]

The @scheme[transcript-on] and @scheme[transcript-off] procedures of
@schememodname[mzscheme] always raise
@scheme[exn:fail:unsupported]. The @schememodname[mzlib/transcr]
library provides working versions of @scheme[transcript-on] and
@scheme[transcript-off].

@(define-syntax-rule (go)
  (begin
   (require (for-label mzlib/transcr))

   @deftogether[(
   @defproc[(transcript-on [filename any/c]) any]
   @defproc[(transcript-off) any]
   )]{

   Starts/stops recording a transcript at @scheme[filename].}))
@(go)
