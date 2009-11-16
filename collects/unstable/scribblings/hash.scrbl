#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
	  "utils.ss"
         (for-label unstable/hash
                    scheme/contract
                    scheme/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/hash))

@title[#:tag "hash"]{Hash Tables}

@defmodule[unstable/hash]

@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]

@defproc[(hash-union [t1 hash?] [t2 hash?] [combine (any/c any/c any/c . -> . any/c)]) hash?]{
Produces the combination of @scheme[t1] and @scheme[t2].  If either
@scheme[t1] or @scheme[t2] has a value for key @scheme[k], then the
result has the same value for @scheme[k].   If both @scheme[t1] and
@scheme[t2] have a value for @scheme[k], the result has the value
@scheme[(combine k (hash-ref t1 k) (hash-ref t2 k))] for @scheme[k].  

@examples[#:eval the-eval
(hash-union #hash((a . 5) (b . 0)) #hash((d . 12) (c . 1)) (lambda (k v1 v2) v1))
(hash-union #hash((a . 5) (b . 0)) #hash((a . 12) (c . 1)) (lambda (k v1 v2) v1))
]
}