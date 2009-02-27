#lang scribble/doc
@(require scribble/manual 
          scribble/struct 
          scheme/runtime-path
          (prefix-in lp-ex: "lp-ex-doc.scrbl")
          "utils.ss"
          (for-label scribble/lp-include scribble/lp))

@title[#:tag "lp" #:style `((css "lp.css") (tex "lp.tex")) ]{Literate Programming}

Programs written using @schememodname[scribble/lp] are simultaneously
two things: a program and a document describing the program.

Programs in @schememodname[scribble/lp] are viewed in two different
ways, either by running the program directly or by including it with
@scheme[lp-include].  When running the program, all of the
@scheme[chunk] expressions are collected and stitched together into a
program, and the rest of the module is discarded. When using
@scheme[lp-include], the entire contents of the module are preserved
and are treated like an ordinary Scribble document, where
@scheme[chunk]s are typeset in a manner similar to @scheme[codeblock].

@(define-runtime-path lp-ex "lp-ex.ss")

For example, consider this program:

@(call-with-input-file lp-ex
   (lambda (port)
     (verbatim
      #:indent 2
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

@(make-blockquote
  "LPBoxed"
  (flow-paragraphs (part-flow lp-ex:doc)))

@section{@schememodname[scribble/lp] Language}

@defmodulelang[scribble/lp]{The @schememodname[scribble/lp] language
provides core support for literate programming.}

@defform[(chunk id form ...)]{

  Introduces a chunk, binding @scheme[id] for use in other
  chunks. Normally, @scheme[id] starts with @litchar{<} and ends with
  @litchar{>}.

  If @scheme[id] is @schemeidfont{<*>}, then this chunk is used as the main
  chunk in the file. If @schemeidfont{<*>} is never used, then the first chunk
  in the file is treated as the main chunk.
}

@section{@schememodname[scribble/lp-include] Module}

@defmodule[scribble/lp-include]{}

@defform[(lp-include filename)]{
Includes the source of @scheme[filename] as the typeset version of the literate
program.
}
