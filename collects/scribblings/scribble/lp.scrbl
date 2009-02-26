#lang scribble/doc
@(require scribble/manual scheme/runtime-path scribble/lp-include)

@title[#:tag "lp"]{Literate Programming}

Programs written using @schememodname[scribble/lp] are simultaneously
two things: a program, and a document describing the program. 

Programs in @schememodname[scribble/lp] are viewed in two different
ways, either by running the program directly, or by including it with
@scheme[include-lp].  When running the program, all of the
@scheme[chunk] expressions are collected and stitched together into a
program and the rest of the module is discarded. When using
@scheme[include-lp], the entire contents of the module are preserved
and are treated like an ordinary Scribble document, where
@scheme[chunk]s are typeset in a manner similar to @scheme[codeblock].

@(define-runtime-path lp-ex "lp-ex.ss")

For example, consider this program:
@(call-with-input-file lp-ex
   (lambda (port)
     (verbatim
      (apply 
       string-append
       (let loop ()
	 (let ([line (read-line port 'any)])
	   (cond
	    [(eof-object? line) '()]
            [(equal? line "") (cons "  \n" (loop))]
            [else 
	       (list* line "\n" (loop))])))))))

When this file is @scheme[require]d in the normal manner, it defines a
function @scheme[f] that squares its argument, and the documentation
is ignored. When it is included with @scheme[lp-include], it looks
like this:

@lp-include["lp-ex.ss"]

@section{@schememodname[scribble/lp] language}

@defmodulelang[scribble/lp]{This is a Scribble's core support for Literate Programming.}

@defform[(chunk <id> expressions ...)]{
  Introduces a chunk, binding @scheme[<id>] for use in other chunks.

  If @scheme[<id>] is @tt{<*>}, then this chunk is used as the main
  chunk in the file. If @tt{<*>} is never used, then the first chunk
  in the file is treated as the main chunk.
}

@section{@schememodname[scribble/lp-include] module}

@defmodule[scribble/lp-include]{}

@defform[(lp-include filename)]{
Includes the source of @scheme[filename] as the typeset version of the literate
program.
}