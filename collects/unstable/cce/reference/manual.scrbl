#lang scribble/doc
@(require scribble/manual
          "../../scribblings/utils.rkt"
          "../scribble.ss"
          (for-label scheme/base))

@title[#:style '(toc)]{@bold{Carl Eastlund's Scheme Utilities}}

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@table-of-contents[]

@include-section["set.scrbl"]
@include-section["dict.scrbl"]

@include-section["syntax.scrbl"]
@include-section["define.scrbl"]

@include-section["class.scrbl"]

@include-section["require-provide.scrbl"]
@include-section["planet.scrbl"]

@include-section["exn.scrbl"]

@include-section["debug.scrbl"]

@include-section["scribble.scrbl"]

@include-section["gui.scrbl"]
@include-section["drscheme.scrbl"]
@include-section["slideshow.scrbl"]
