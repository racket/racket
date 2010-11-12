#lang scribble/manual
@(require scribble/struct
          scribble/decode
          scribble/eval
	  "utils.rkt"
          (for-label racket/base
                     racket/contract
                     unstable/prop-contract))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/contract unstable/prop-contract))

@title{Contracts for struct type properties}

@defmodule[unstable/prop-contract]

@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defproc[(struct-type-property/c [value-contract contract?])
         contract?]{

Produces a contract for struct type properties. When the contract is
applied to a struct type property, it produces a wrapped struct type
property. When the wrapped struct type property is used to create a
new struct type (via @racket[struct], @racket[make-struct-type], etc),
it applies @racket[value-contract] to the value associated with the
property.

The contract has no effect on the struct type property accessor.

@examples[#:eval the-eval
(define-values (prop prop? prop-ref)
  (make-struct-type-property 'prop))

(define/contract wrapped
    (struct-type-property/c (-> any/c (-> number? number?)))
  prop)

(struct s (f)
  #:property wrapped (lambda (s) (s-f s)))

(define (get-f s) ((prop-ref s) s))

(define s1 (s add1))
((get-f s1) 5)
((get-f s1) 'apple)

(define s2 (s (lambda (n) (if (zero? n) 'zero 'nonzero))))
((get-f s2) 5)
((get-f s2) 'apple)
]
}
