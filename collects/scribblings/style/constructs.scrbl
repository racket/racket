#lang scribble/base

@(require "shared.rkt")

@title{Choosing the Right Construct}

Racket provides a range of constructs for the same or similar purposes.
Although the Racket designers don't think that there is one right way for
everything, we prefer certain constructs in certain situations for consistency
and readability.

@; -----------------------------------------------------------------------------
@section{Definitions}

Here are a few of Racket's definitional constructs: @scheme[let], @scheme[let*],
@scheme[letrec], and @scheme[define]. Except for the last one, all others force
an increase to the indentation level. We therefore request that you favor
@scheme[define] over all other features when feasible.

@compare[
@racketmod[#:file
@tt{good}
racket

(define (swap x y)
  (define t (unbox x))
  (set-box! x (unbox y))
  (set-box! y t))
]
@; -----------------------------------------------------------------------------
@racketmod[#:file
@tt{bad}
racket

(define (swap x y)
  (let ([t (unbox x)])
    (set-box! x (unbox y))
    (set-box! y t)))
]
]

@; -----------------------------------------------------------------------------
@section{Conditionals}

Like definitional constructs, conditionals come in many flavors,
too. Because @scheme[cond] and its relatives (@scheme[case],
@scheme[match], etc) now allow local uses of @scheme[define], you should
prefer them over @scheme[if].

@compare[
@racketmod[#:file
@tt{good}
racket

(cond
  [(empty? l) true]
  [else
   (define fst (first l))
   (define rst (rest l))
   (and (flat-rate fst)
        (curved fst (chk rst)))])
]
@racketmod[#:file
@tt{bad}
racket

(if (empty? l)
    true
    (let ([fst (first l)]
	  [rst (rest l)])
      (and (flat-rate fst)
	   (curved fst (chk rst)))))
]
]

Of course you should also favor @scheme[cond] (and its relatives) over
@scheme[if] to match the shape of the data definition.
