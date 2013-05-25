#lang scribble/doc
@(require scribble/manual scribble/core scribble/html-properties
          scribble/latex-properties
          racket/runtime-path
          "utils.rkt"
          (prefix-in lp-ex: "lp-ex-doc.scrbl")
          (for-label scribble/lp-include scribble/lp))

@title[#:tag "lp"
       #:style (make-style #f
                           (list (make-css-addition "lp.css")
                                 (make-tex-addition "lp.tex")))
      ]{Literate Programming}

Programs written using @racketmodname[scribble/lp] are simultaneously
two things: a program and a document describing the program.

Programs in @racketmodname[scribble/lp] are viewed in two different
ways, either by running the program directly or by including it with
@racket[lp-include].  When running the program, all of the
@racket[chunk] expressions are collected and stitched together into a
program, and the rest of the module is discarded. When using
@racket[lp-include], the entire contents of the module are preserved
and are treated like an ordinary Scribble document, where
@racket[chunk]s are typeset in a manner similar to @racket[codeblock].

@(define-runtime-path lp-ex "lp-ex.rkt")

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

When this file is @racket[require]d in the normal manner, it defines a
function @racket[f] that squares its argument, and the documentation
is ignored. When it is included with @racket[lp-include], it looks
like this:

@(make-nested-flow
  (make-style "LPBoxed" null)
  (part-blocks lp-ex:doc))

@section{@racketmodname[scribble/lp] Language}

@defmodulelang[scribble/lp]{The @racketmodname[scribble/lp] language
provides core support for literate programming.}

@defform[(chunk id form ...)]{

  Introduces a chunk, binding @racket[id] for use in other
  chunks. Normally, @racket[id] starts with @litchar{<} and ends with
  @litchar{>}.

  When running a scribble program only the code inside the
  chunks is run; the rest is ignored. 

  If @racket[id] is @racketidfont{<*>}, then this chunk is
  used as the main chunk in the file. If @racketidfont{<*>}
  is never used, then the first chunk in the file is treated
  as the main chunk. If some chunk is not referenced from
  the main chunk (possibly indirectly via other chunks that
  the main chunk references), then it is not included in the
  program and thus is not run.

}

@section{@racketmodname[scribble/lp-include] Module}

@defmodule[scribble/lp-include]{The
@racketmodname[scribble/lp-include] library is normally used within a
Scribble document---that is, a module that starts with something like
@racket[#, @hash-lang[] scribble/base] or @racket[#, @hash-lang[]
scribble/manual], instead of @racket[#, @hash-lang[] racket].}

@defform[(lp-include filename)]{
Includes the source of @racket[filename] as the typeset version of the literate
program.
}
