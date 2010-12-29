#lang scribble/manual
@(require scribble/eval	  
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

@addition[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defform[(match? val-expr pat ...)]{

Returns @scheme[#t] if the result of @scheme[val-expr] matches any of
@scheme[pat], and returns @scheme[#f] otherwise.

@defexamples[
#:eval (eval/require 'racket/match 'unstable/match)
(match? (list 1 2 3)
  (list a b c)
  (vector x y z))
(match? (vector 1 2 3)
  (list a b c)
  (vector x y z))
(match? (+ 1 2 3)
  (list a b c)
  (vector x y z))
]

}

@defform[(as ([lhs-id rhs-expr] ...) pat ...)]{

As a match expander, binds each @scheme[lhs-id] as a pattern variable with the
result value of @scheme[rhs-expr], and continues matching each subsequent
@scheme[pat].

@defexamples[
#:eval (eval/require 'racket/match 'unstable/match)
(match (list 1 2 3)
  [(as ([a 0]) (list b c d)) (list a b c d)])
]

}

@close-eval[the-eval]
