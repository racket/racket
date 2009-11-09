#lang scribble/manual
@(require scribble/eval
          (for-label unstable/poly-c
                     scheme/contract
                     scheme/base))

@title[#:tag "poly-c"]{Anaphoric Contracts}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/poly-c scheme/contract))

@defmodule[unstable/poly-c]

@author[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]
	@author+email["Carl Eastlund" "cce@ccs.neu.edu" #:obfuscate? #t]]


@defform[(poly/c ([id+ id-] ...) cnt)]{
Creates an ``anaphoric'' contract, using the @scheme[id+ ...] as the
positive positions, and the @scheme[id- ...] as the negative positions.  

Anaphoric contracts verify that only values provided to a given
positive position flow out of the corresponding negative position.

@examples[#:eval the-eval
(define/contract (f x) (poly/c ([in out]) (in . -> . out)) 
  (if (equal? x 17) 18 x))
(f 1)
(f #f)
(f 17)
]
}
