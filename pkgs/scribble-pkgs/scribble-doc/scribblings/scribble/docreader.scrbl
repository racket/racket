#lang scribble/doc
@(require scribble/manual scribble/bnf "utils.rkt")

@title[#:tag "docreader"]{Document Reader}

@defmodulelang[scribble/doc]{The @racketmodname[scribble/doc] language is
the same as @racketmodname[scribble/doclang], except that
@racket[read-syntax-inside] is used to read the body of the module. In
other words, the module body starts in Scribble ``text'' mode instead
of S-expression mode.}
