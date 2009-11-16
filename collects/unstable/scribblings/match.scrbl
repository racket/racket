#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval	  
	  "utils.ss"
         (for-label unstable/match
		    scheme/match
                    scheme/contract
                    scheme/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/match scheme/match))

@title[#:tag "match"]{Match}

@defmodule[unstable/match]

@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]

@defform*[[(== val comparator) (== val)]]{
A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{match expander} 
which checks if the matched value is the same as @scheme[val] when
compared by @scheme[comparator].  If @scheme[comparator] is
not provided, it defaults to @scheme[equal?].  

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