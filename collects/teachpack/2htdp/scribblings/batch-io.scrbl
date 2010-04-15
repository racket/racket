#lang scribble/doc

@(require (for-label scheme teachpack/2htdp/batch-io))
@(require scribble/manual "shared.ss")
@(require scribble/struct)

@; -----------------------------------------------------------------------------

@(define-syntax-rule (reading name ctc s)
   @defproc[(@name [f (and/c string? file-exists?)]) @ctc ]{
 reads the content of file @scheme[f] and produces it as @s .
 The file @scheme[f] must exist and must be located in the same folder
 (directory) as the program; otherwise the function signals an error.} )

@teachpack["batch-io"]{Batch Input/Output}

@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/batch-io]

The batch-io teachpack introduces several functions and a form for reading
 content from files and one function for writing to a file.

@reading[read-file-as-string string?]{a string, including newlines}

@reading[read-file-as-lines (listof string?)]{a list of strings, one per line}

@reading[read-file-as-1strings (listof 1string?)]{a list of one-char strings, one per character}

@defform/subs[#:id read-file-as-csv 
              #:literals 
	      (turn-row-into)
              (read-file-as-csv f-expr clause)
              ([clause
		 (turn-row-into row-expr)
		 ])]{
 reads the content of file @scheme[f] and produces it as a list of rows. 
 The file @scheme[f] must be a file of comma-separated values (csv).
 It must exist and must be located in the same folder
 (directory) as the program; otherwise the function signals an error.

 The form comes with one optional clause: @scheme[turn-into-row], which is
 described next.}

 @defform[(turn-row-into row-expr)
          #:contracts
          ([row-expr (-> (listof (or/c string? number?)) any)])]{
 requests that each row is processed by the result of @scheme[row-expr]
 before it is added to the result of @scheme[read-file-as-csv].}


@defproc[(write-file [f string?] [cntnt string?]) boolean?]{
 turns @scheme[cntnt] into the content of file @scheme[f], located in the
 same folder (directory) as the program. If the file exists when the
 function is called, the function produces @scheme[true]; otherwise it
 produces @scheme[false].}

