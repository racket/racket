#lang scribble/doc
@(require scribble/manual
          unstable/scribble
          "../../scribblings/utils.rkt"
          (for-label scheme/base))

@title[#:style '(toc)]{@bold{Carl Eastlund's Scheme Utilities}}

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@table-of-contents[]

@include-section["set.scrbl"]

@include-section["debug.scrbl"]
