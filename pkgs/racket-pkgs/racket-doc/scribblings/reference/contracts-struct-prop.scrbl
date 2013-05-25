#lang scribble/manual
@(require scribble/struct scribble/decode scribble/eval
          (for-label racket/base racket/contract))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/contract))

@title{Structure Type Property Contracts}

@defproc[(struct-type-property/c [value-contract contract?])
         contract?]{

Produces a contract for a @tech{structure type property}. When the
contract is applied to a struct type property, it produces a wrapped
struct type property that applies @racket[value-contract] to the value
associated with the property when it used to create a new struct type
(via @racket[struct], @racket[make-struct-type], etc).

The struct type property's accessor function is not affected; if it is
exported, it must be protected separately.

As an example, consider the following module. It creates a structure
type property, @racket[prop], whose value should be a function mapping
a structure instance to a numeric predicate. The module also exports
@racket[app-prop], which extracts the predicate from a structure
instance and applies it to a given value.

@interaction[#:eval the-eval
(module propmod racket
  (require racket/contract)
  (define-values (prop prop? prop-ref)
    (make-struct-type-property 'prop))
  (define (app-prop x v)
    (((prop-ref x) x) v))
  (provide/contract
   [prop? (-> any/c boolean?)]
   [prop (struct-type-property/c
          (-> prop? (-> integer? boolean?)))]
   [app-prop (-> prop? integer? boolean?)])
  (provide prop-ref))
]

The @racket[structmod] module creates a structure type named
@racket[s] with a single field; the value of @racket[prop] is a
function that extracts the field value from an instance. Thus the
field ought to be an integer predicate, but notice that
@racket[structmod] places no contract on @racket[s] enforcing that
constraint.

@interaction[#:eval the-eval
(module structmod racket
  (require 'propmod)
  (struct s (f) #:property prop (lambda (s) (s-f s)))
  (provide (struct-out s)))
(require 'propmod 'structmod)
]

First we create an @racket[s] instance with an integer predicate, so
the constraint on @racket[prop] is in fact satisfied. The first call
to @racket[app-prop] is correct; the second simply violates the
contract of @racket[app-prop].

@interaction[#:eval the-eval
(define s1 (s even?))
(app-prop s1 5)
(app-prop s1 'apple)
]

We are able to create @racket[s] instances with values other than
integer predicates, but applying @racket[app-prop] on them blames
@racket[structmod], because the function associated with
@racket[prop]---that is, @racket[(lambda (s) (s-f s))]---does not
always produce a value satisfying @racket[(-> integer? boolean?)]. 

@interaction[#:eval the-eval
(define s2 (s "not a fun"))
(app-prop s2 5)

(define s3 (s list))
(app-prop s3 5)
]

The fix would be to propagate the obligation inherited from
@racket[prop] to @racket[s]:

@racketblock[
(provide (contract-out
           [struct s ([f (-> integer? boolean?)])]))
]

Finally, if we directly apply the property accessor,
@racket[prop-ref], and then misuse the resulting function, the
@racket[propmod] module is blamed:

@interaction[#:eval the-eval
((prop-ref s3) 'apple)
]

The @racket[propmod] module has an obligation to ensure a function
associated with @racket[prop] is applied only to values satisfying
@racket[prop?]. By directly providing @racket[prop-ref], it enables
that constraint to be violated (and thus it is blamed), even though
the bad application actually occurs elsewhere.

Generally there is no need to provide a structure type property
accessor at all; it is typically only used by other functions within
the module. But if it must be provided, it should be protected thus:

@racketblock[
(provide (contract-out
           [prop-ref (-> prop? (-> prop? (-> integer? boolean?)))]))
]
}

@close-eval[the-eval]
