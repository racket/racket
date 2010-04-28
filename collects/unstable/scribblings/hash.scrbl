#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
	  "utils.rkt"
         (for-label unstable/hash
                    racket/contract
                    racket/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/hash))

@title[#:tag "hash"]{Hash Tables}

@defmodule[unstable/hash]

@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]

@defproc[(hash-union [t1 hash?] [t2 hash?] [combine (any/c any/c any/c . -> . any/c)]) hash?]{
Produces the combination of @racket[t1] and @racket[t2].  If either
@racket[t1] or @racket[t2] has a value for key @racket[k], then the
result has the same value for @racket[k].   If both @racket[t1] and
@racket[t2] have a value for @racket[k], the result has the value
@racket[(combine k (hash-ref t1 k) (hash-ref t2 k))] for @racket[k].  

@examples[#:eval the-eval
(hash-union #hash((a . 5) (b . 0)) #hash((d . 12) (c . 1)) (lambda (k v1 v2) v1))
(hash-union #hash((a . 5) (b . 0)) #hash((a . 12) (c . 1)) (lambda (k v1 v2) v1))
]
}