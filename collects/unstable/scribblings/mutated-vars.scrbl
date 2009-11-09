#lang scribble/manual
@(require scribble/eval
          (for-label unstable/mutated-vars
                     scheme/contract
                     scheme/base))

@title[#:tag "mutated-vars"]{Finding Mutated Variables}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/mutated-vars))

@defmodule[unstable/mutated-vars]

@author[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]


@defproc[(find-mutated-vars [stx syntax?]) void?]{ Traverses
@scheme[stx], which should be @scheme[module-level-form] in the sense
of the grammar for 
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{fully-expanded} forms,
and records all of the variables that are mutated.}

@defproc[(is-var-mutated? [id identifier?]) boolean?]{
Produces @scheme[#t] if @scheme[id] is mutated by an expression
	 previously passed to @scheme[find-mutated-vars], otherwise
	 produces @scheme[#f].


@examples[#:eval the-eval
(find-mutated-vars #'(begin (set! var 'foo) 'bar))
(is-var-mutated? #'var)
(is-var-mutated? #'other-var)
]
}
