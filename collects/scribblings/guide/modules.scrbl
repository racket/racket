#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "guide:modules" #:style 'toc]{Modules}

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
