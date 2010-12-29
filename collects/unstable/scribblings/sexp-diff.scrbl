#lang scribble/doc
@(require scribble/base
          scribble/manual
	  scribble/eval
	  "utils.rkt"
          (for-label unstable/sexp-diff
                     racket/serialize
                     racket/contract
                     racket/base))

@(define diff-eval (make-base-eval))
@(parameterize ([current-eval diff-eval])
   (eval '(require unstable/sexp-diff)))

@title[#:tag "sexp-diff"]{S-Expression Diff}

@defmodule[unstable/sexp-diff]

@unstable-header[]

@defproc[(sexp-diff [old-tree any/c] [new-tree any/c])
         any/c]{
 Takes two S-Expressions and returns their diff. Based on the
 Levenshtein distance for trees.

 @examples[#:eval diff-eval (sexp-diff '(0 (1 2 3)) '(0 (4 2 3)))]
}


@close-eval[diff-eval]
