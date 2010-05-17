#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/math))

@mzlib[#:mode title math]

Re-exports @schememodname[scheme/math], and also exports @scheme[e].

@defthing[e real?]{

An approximation to Euler's constant: @number->string[(exp 1)].}
