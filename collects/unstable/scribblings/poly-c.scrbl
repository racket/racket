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

@defproc[(apply/c [cnt any/c] 
		  [#:name name any/c 
		  (build-compound-type-name 'apply/c c)]) contract?]{
Produces a procedure contract that is like @scheme[cnt], but any delayed
evalutation in @scheme[cnt] is re-done on every
application of the contracted function.
}

@defproc[(memory/c [#:name name any/c "memory/c"]
		   [#:from from any/c (format "~a:from" name)]
		   [#:to to any/c (format "~a:to" name)]
		   [#:weak weak? any/c #t]
		   [#:equal equal (or/c 'eq 'eqv 'equal) 'eq]
		   [#:table make-table  (-> hash?)
		   (case equal		
                     [(eq) (if weak? make-weak-hasheq make-hasheq)]
                     [(eqv) (if weak? make-weak-hasheqv make-hasheqv)]
                     [(equal) (if weak? make-weak-hash make-hash)])]
		   )
		    (values flat-contract? flat-contract?)]{

Produces a pair of contracts.  The first contract remembers all values
that flow into it, and rejects nothing.  The second accepts only
values that have previously been passed to the first contract.  

If @scheme[weak?] is not @scheme[#f], the first contract holds onto
the values only weakly.  @scheme[from] and @scheme[to] are the names
of the of the two contracts. }


