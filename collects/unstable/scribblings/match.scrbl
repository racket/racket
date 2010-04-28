#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval	  
	  "utils.rkt"
         (for-label unstable/match
		    racket/match
                    racket/contract
                    racket/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/match racket/match))

@title[#:tag "match"]{Match}

@defmodule[unstable/match]

@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]

@defform*[[(== val comparator) (== val)]]{
A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{match expander} 
which checks if the matched value is the same as @racket[val] when
compared by @racket[comparator].  If @racket[comparator] is
not provided, it defaults to @racket[equal?].  

@examples[#:eval the-eval
(match (list 1 2 3)
  [(== (list 1 2 3)) 'yes]
  [_ 'no])
(match (list 1 2 3)
  [(== (list 1 2 3) eq?) 'yes]
  [_ 'no])
(match (list 1 2 3)
  [(list 1 2 (== 3 =)) 'yes]
  [_ 'no])
]
}