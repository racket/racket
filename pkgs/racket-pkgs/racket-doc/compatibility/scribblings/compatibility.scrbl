#lang scribble/doc
@(require scribble/base scribble/manual)

@title[#:tag "compatibility"]{Compatibility: Features from Racket Relatives}

The @racketidfont{compatibility} collection includes features borrowed from
other languages closely related to Racket.
We provide these features to ease porting code from these languages to Racket.

We do @emph{not} recommend using any of these bindings in new code.
Racket provides better alternatives, which we point to in this manual.
We @emph{strongly} recommend using these alternatives.

@local-table-of-contents[#:style 'immediate-only]

@include-section["defmacro.scrbl"]
@include-section["package.scrbl"]
@include-section["mlists.scrbl"]

@(bibliography
  (bib-entry #:key "Waddell99"
             #:author "Oscar Waddell and R. Kent Dybvig"
             #:title "Extending the Scope of Syntactic Abstraction"
             #:location "Principles of Programming Languages"
             #:date "1999"))
