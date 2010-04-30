#lang scribble/manual
@(require scribble/eval
          "utils.rkt"
          (for-label unstable/mutated-vars
                     racket/contract
                     racket/base))

@title[#:tag "mutated-vars"]{Finding Mutated Variables}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/mutated-vars))

@defmodule[unstable/mutated-vars]

@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]


@defproc[(find-mutated-vars [stx syntax?]) void?]{ Traverses
@racket[stx], which should be @racket[module-level-form] in the sense
of the grammar for 
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{fully-expanded} forms,
and records all of the variables that are mutated.}

@defproc[(is-var-mutated? [id identifier?]) boolean?]{
Produces @racket[#t] if @racket[id] is mutated by an expression
	 previously passed to @racket[find-mutated-vars], otherwise
	 produces @racket[#f].


@examples[#:eval the-eval
(find-mutated-vars #'(begin (set! var 'foo) 'bar))
(is-var-mutated? #'var)
(is-var-mutated? #'other-var)
]
}
