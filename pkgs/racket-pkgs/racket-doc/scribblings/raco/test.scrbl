#lang scribble/doc
@(require scribble/manual 
          scribble/bnf
          "common.rkt" 
          (for-label racket/runtime-path
                     launcher/launcher))

@title[#:tag "test"]{@exec{raco test}: Run tests}

The @exec{raco test} command requires and runs the @racket[test]
submodule (if any) associated with each path given on the command line. When a
path refers to a directory, the tool recursively discovers all
files that end in @filepath{.rkt} within the directory and runs their 
@racket[test] submodules.

The @exec{raco test} command accepts a few flags:

@itemize[
 @item{@Flag{s} @nonterm{name} or @DFlag{submodule} @nonterm{name}
       --- Requires the submodule @nonterm{name} rather than @racket[test].}

 @item{@Flag{r} or @DFlag{run-if-absent}
       --- Requires the top-level module of a file if the relevant submodule is not 
       present. This is the default mode.}

 @item{@Flag{x} or @DFlag{no-run-if-absent}
       --- Ignores a file if the relevant submodule is not present.}

 @item{@Flag{c} or @DFlag{collection}
       --- Intreprets the arguments as collections where all files should be tested.}

 @item{@Flag{p} or @DFlag{package}
       --- Intreprets the arguments as packages where all files should be tested. (All package scopes are searched for the first, most specific package.)}
]
