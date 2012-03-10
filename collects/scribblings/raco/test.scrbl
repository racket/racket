#lang scribble/doc
@(require scribble/manual 
          scribble/bnf
          "common.rkt" 
          (for-label racket/runtime-path
                     launcher/launcher))

@title[#:tag "test"]{@exec{raco test}: Run tests}

The @exec{raco test} command requires and runs the @racket['test]
submodules associated with paths given on the command line. When a
path refers to a directory, the tool recursively discovers all
internal files that end in @filepath{.rkt} and inspects them as well.

The @exec{raco test} command accepts a few flags:

@itemize[
 @item{@DFlag{s} @nonterm{id} or @DFlag{submodule} @nonterm{id}--- Requires the submodule @nonterm{id} rather than @racket['test].}

 @item{@DFlag{r} or @DFlag{run-if-absent}--- Requires the default module if the given submodule is not present in a file.}
]
