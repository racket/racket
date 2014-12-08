#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "modules" #:style 'toc]{Modules}


Modules let you organize Racket code into multiple files and reusable
libraries.

@local-table-of-contents[]

@include-section["module-basics.scrbl"]
@include-section["module-syntax.scrbl"]
@include-section["module-paths.scrbl"]
@include-section["module-require.scrbl"]
@include-section["module-provide.scrbl"]
@include-section["module-set.scrbl"]
