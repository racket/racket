#lang scribble/doc
@(require scribble/manual 
          "common.rkt" 
          (for-label racket/runtime-path
                     launcher/launcher))

@title[#:tag "test"]{@exec{raco test}: Run tests}

The @exec{raco test} command requires and runs the @racket['test]
submodules associated with paths given on the command line. When a
path refers to a directory, the tool recursively discovers all
internal files and inspects them as well.
