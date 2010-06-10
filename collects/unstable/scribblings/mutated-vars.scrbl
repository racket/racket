#lang scribble/manual
@(require scribble/eval
          "utils.rkt"
          (for-label unstable/mutated-vars
                     racket/contract
                     racket/dict
                     racket/base))

@title[#:tag "mutated-vars"]{Finding Mutated Variables}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/mutated-vars racket/dict))

@defmodule[unstable/mutated-vars]

@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]


@defproc[(find-mutated-vars [stx syntax?]) dict?]{Traverses
@racket[stx], which should be @racket[module-level-form] in the sense
of the grammar for 
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{fully-expanded} forms,
and records all of the variables that are mutated.  The result is a
dictionary that maps each mutated identifier to @racket[#t].} 

@examples[#:eval the-eval
(define t (find-mutated-vars #'(begin (set! var 'foo) 'bar)))
(dict-ref t #'var #f)
(dict-ref t #'other-var #f)
]
}
