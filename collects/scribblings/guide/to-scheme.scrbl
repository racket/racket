#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "to-scheme" #:style 'toc]{Scheme Essentials}

This chapter provides a quick introduction to Scheme as background for
the rest of the guide. Readers with some Scheme experience can safely
skip to @secref["datatypes"].

@local-table-of-contents[]

@include-section["simple-data.scrbl"]
@include-section["simple-syntax.scrbl"]
@include-section["lists.scrbl"]
@include-section["truth.scrbl"]
