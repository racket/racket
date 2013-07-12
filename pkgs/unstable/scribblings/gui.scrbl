#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-label scribble/base))

#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-syntax racket/base racket/path)
          (for-label scribble/base))

@unstable-title[#:style '(toc) #:tag "unstable-gui"]{GUI}

@local-table-of-contents[#:style 'immediate-only]

@include-section["gui/notify.scrbl"]
@include-section["gui/prefs.scrbl"]
@include-section["gui/pict.scrbl"]
@include-section["gui/slideshow.scrbl"]
@include-section["gui/pslide.scrbl"]
@include-section["gui/redex.scrbl"]
@include-section["gui/snip.scrbl"]
@include-section["gui/scribble.scrbl"]
