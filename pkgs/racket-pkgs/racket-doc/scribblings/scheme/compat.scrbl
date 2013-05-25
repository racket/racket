#lang scribble/manual

@title[#:tag "compat-exe"]{Compatibility Executables}

The following executables are included in the Racket distribution for
compatibility with older versions of Racket:

@itemlist[

 @item{@as-index{@exec{mzscheme}} --- the same a @exec{racket -I scheme/init}}

 @item{@as-index{@exec{mred}} --- the same a @exec{gracket -I scheme/gui/init}}

 @item{@as-index{@exec{drscheme}} --- the same as @exec{drracket}}

 @item{@as-index{@exec{mzc}} --- an old interface to some of the tools
       provided by @exec{raco}, including @exec{raco make} and
       @exec{raco ctool}; use @exec{mzc --help} for more information}

 @item{@as-index{@exec{plt-help}} --- the same as @exec{raco docs}}

]
