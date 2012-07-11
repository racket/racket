#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/math))

@mzlib[#:mode title math]

@deprecated[@racketmodname[racket/math]]{}

Re-exports @racketmodname[scheme/math], and also exports @racket[e].

@defthing[e real?]{

An approximation to Euler's constant: @number->string[(exp 1)].}
