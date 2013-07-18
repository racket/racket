#lang scribble/base
@(require scriblib/footnote)

@(define-footnote footnote generate-footnotes)

@title{Document}

Left.@footnote{A}

Right.@footnote{A♯}

@generate-footnotes[]
