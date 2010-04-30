#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label unstable/contract
                     racket/contract
                     racket/base))

@title[#:tag "contract"]{Contracts}

@defmodule[unstable/contract]

@unstable-header[]

@defthing[non-empty-string/c contract?]{
Contract for non-empty strings.
}

@defthing[port-number? contract?]{
Equivalent to @racket[(between/c 1 65535)].
}

@defthing[path-element? contract?]{
Equivalent to @racket[(or/c path-string? (symbols 'up 'same))].
}

@addition{Ryan Culpepper}

@defproc[(if/c [predicate (-> any/c any/c)]
               [then-contract contract?]
               [else-contract contract?])
         contract?]{

Produces a contract that, when applied to a value, first tests the
value with @racket[predicate]; if @racket[predicate] returns true, the
@racket[then-contract] is applied; otherwise, the
@racket[else-contract] is applied. The resulting contract is a flat
contract if both @racket[then-contract] and @racket[else-contract] are
flat contracts.

For example, the following contract enforces that if a value is a
procedure, it is a thunk; otherwise it can be any (non-procedure)
value:
  @racketblock[(if/c procedure? (-> any) any/c)]
Note that the following contract is @bold{not} equivalent:
  @racketblock[(or/c (-> any) any/c) (code:comment "wrong!")]
The last contract is the same as @racket[any/c] because
@racket[or/c] tries flat contracts before higher-order contracts.
}

@defproc[(rename-contract [contract contract?]
                          [name any/c])
         contract?]{

Produces a contract that acts like @racket[contract] but with the name
@racket[name].

The resulting contract is a flat contract if @racket[contract] is a
flat contract.
}
