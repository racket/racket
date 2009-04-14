#lang scribble/doc

@(require scribble/manual "shared.ss" 
          (for-label scheme teachpack/2htdp/batch-io))
@(require scribble/struct)

@; -----------------------------------------------------------------------------

@teachpack["batch-io"]{Batch Input/Output}

@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/batch-io]

The batch-io teachpack introduces two functions: @scheme[read-file] and
@scheme[write-file]. It facilitates the reading and writing of entire files
in one batch. 

@defproc[(read-file [f (and/c string? file-exists?)]) string?]{
 reads the content of file @scheme[f], located in the samd folder
 (directory) as the program, and produces it as a string. If @scheme[f]
 doesn't exist, the function signals an error.
}

@defproc[(write-file [f string?] [cntnt string?]) boolean?]{
 turns @scheme[cntnt] into the content of file @scheme[f], located in the
 same folder (directory) as the program. If the file exists when the
 function is called, the function produces @scheme[true]; otherwise it
 produces @scheme[false].}
