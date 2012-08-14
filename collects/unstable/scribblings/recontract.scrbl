#lang scribble/manual
@(require scribble/eval 
          "utils.rkt"
          (for-label racket/base
                     racket/contract
                     unstable/recontract))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/contract/base unstable/recontract))

@title[#:tag "recontract"]{Re-Contracting Identifiers}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/recontract]

@defform[(recontract-out id ...)]{

Provides each @racket[id] with its existing contract, but changes the
positive blame party of the contract to the enclosing module, instead
of the module that originally attached the contract to @racket[id].
Each @racket[id] must be imported from a module that exports it via
@racket[contract-out] or @racket[recontract-out]; otherwise a syntax
error is raised.

Use @racket[recontract-out] when you want to use the same contracts
both between different parts of a library and between the library and
its clients. The library should use @racket[recontract-out] in the
public interface modules so that clients do not see references to
private implementation modules in contract errors.

@examples[#:eval the-eval
(module private racket
  (define (f x) (if (positive? x) x 'wrong))
  (provide (contract-out [f (-> real? real?)])))

(module public racket
  (require 'private unstable/recontract)
  (provide (recontract-out f)))

(require 'public)
(f 1)
(f -2)
(f 'apple)
]
}

@(close-eval the-eval)
