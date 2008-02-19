#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "modules" #:style 'toc]{Modules}

Scheme definitions and expressions are normally written inside of a
module. Although a @tech{REPL} evaluates definitions and expressions outside
of a module for exploration and debugging purposes, and although
@scheme[load] can evaluate definitions and expressions from a file as
if they appeared in a @tech{REPL} interaction, code that is meant to last for
more than a few seconds belongs in a module.

@local-table-of-contents[]

@include-section["module-basics.scrbl"]
@include-section["module-syntax.scrbl"]
@include-section["module-paths.scrbl"]
@include-section["module-require.scrbl"]
@include-section["module-provide.scrbl"]
@include-section["module-set.scrbl"]
