#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/struct
          scribble/eval)

@title{Redex: Practical Semantics Engineering}

@author["Robert Bruce Findler" "Casey Klein"]

PLT Redex consists of a domain-specific language for specifying
reduction semantics, plus a suite of tools for working with the
semantics. 

This manual consists of two parts: a tutorial introduction and a reference for Redex. 
Also see
@link["http://redex.racket-lang.org/"]{@tt{http://redex.racket-lang.org/}}
and the @tt{examples} subdirectory in the @tt{redex} collection.

@table-of-contents[]

@include-section["scribblings/tut.scrbl"]
@include-section["scribblings/ref.scrbl"]

@index-section[]

