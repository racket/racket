#lang scribble/manual
@(require scribble/eval
          "utils.rkt"
          (for-label unstable/mutated-vars
                     racket/contract
                     racket/dict
                     syntax/id-table
                     racket/base))

@title[#:tag "mutated-vars"]{Finding Mutated Variables}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/mutated-vars syntax/id-table racket/dict))

@defmodule[unstable/mutated-vars]

@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]


@defproc[(find-mutated-vars [stx syntax?] [dict dict? (make-immutable-free-id-table)]) dict?]{Traverses
@racket[stx], which should be @racket[module-level-form] in the sense
of the grammar for 
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{fully-expanded} forms,
and records all of the variables that are mutated.
Each mutated variable is added to @racket[dict], mapped to
@racket[#t].  If @racket[dict] is mutable, as determined by
@racket[dict-mutable?], then the table is updated destructively.
Otherwise, the table is updated functionally.} 

@examples[#:eval the-eval
(define t (find-mutated-vars #'(begin (set! var 'foo) 'bar)))
(dict-ref t #'var #f)
(dict-ref t #'other-var #f)
(define tbl (make-free-id-table))
(find-mutated-vars #'(begin (set! var 'foo) 'bar) tbl)
(dict-ref tbl #'var #f)
]
}
