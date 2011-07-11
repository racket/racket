#lang scribble/manual
@(require scribble/struct scribble/decode scribble/eval "utils.rkt"
          (for-label racket/base racket/contract unstable/prop-contract))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/contract unstable/prop-contract))

@title{Contracts for struct type properties}

@defmodule[unstable/prop-contract]

@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defproc[(struct-type-property/c [value-contract contract?])
         contract?]{

Produces a contract for struct type properties. When the contract is
applied to a struct type property, it produces a wrapped struct type
property that applies @racket[value-contract] to the value associated
with the property when used to create a new struct type (via
@racket[struct], @racket[make-struct-type], etc).

The struct type property's accessor function is not affected; it must
be protected separately.

@examples[#:eval the-eval
(module propmod racket
  (require racket/contract
           unstable/prop-contract)
  (define-values (prop prop? prop-ref)
    (make-struct-type-property 'prop))
  (define (prop-app x v)
    (((prop-ref x) x) v))
  (provide/contract
   [prop? (-> any/c boolean?)]
   [prop (struct-type-property/c
          (-> prop? (-> number? boolean?)))]
   [prop-app (-> prop? number? boolean?)])
  (provide prop-ref))

(module structmod racket
  (require 'propmod)
  (struct s (f) #:property prop (lambda (s) (s-f s)))
  (provide (struct-out s)))

(require 'propmod 'structmod)
(define s1 (s even?))
(prop-app s1 5)
(prop-app s1 'apple)

(define s2 (s "not a fun"))
(prop-app s2 5)

(define s3 (s list))
(prop-app s3 5)

((prop-ref s3) 'apple)
]
The first contract error above is a simple function contract violation
on @racket[prop-app]. The second and third contract errors above blame
the @racketidfont{structmod} module, because it accepted the struct type
property contract. To avoid blame, @racketidfont{structmod} 
should have placed a contract on @racket[s]. The final contract error,
involving @racket[s3], blames @racketidfont{propmod} because the struct
type property contract obliges @racketidfont{propmod} to make sure the
property's value is not misused, but @racketidfont{propmod} allows
direct access to the property value via @racket[prop-ref]. To 
avoid blame, @racketidfont{propmod} should remove the export of
@racket[prop-ref] or protect it with a contract.
}

@close-eval[the-eval]
