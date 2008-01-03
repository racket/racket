#lang scribble/doc
@require[scribble/manual]
@require[scribble/bnf]
@require["utils.ss"]

@title[#:tag "docreader"]{Document Reader}

@defmodulelang[scribble/doc]{The @schememodname[scribble/doc] language is
the same as @schememodname[scribble/doclang], except that
@scheme[read-inside-syntax] is used to read the body of the module. In
other words, the module body starts in Scribble ``text'' mode instead
of S-expression mode.}
