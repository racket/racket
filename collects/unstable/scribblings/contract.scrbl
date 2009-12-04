#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.ss"
          (for-label unstable/contract
                     scheme/contract
                     scheme/base))

@title[#:tag "contract"]{Contracts}

@defmodule[unstable/contract]

@defthing[non-empty-string/c contract?]{
Contract for non-empty strings.
}

@defthing[port-number? contract?]{
Equivalent to @scheme[(between/c 1 65535)].
}

@defthing[path-element? contract?]{
Equivalent to @scheme[(or/c path-string? (symbols 'up 'same))].
}

@addition{Ryan Culpepper}

@defproc[(if/c [predicate (-> any/c any/c)]
               [then-contract contract?]
               [else-contract contract?])
         contract?]{

Produces a contract that, when applied to a value, first tests the
value with @scheme[predicate]; if @scheme[predicate] returns true, the
@scheme[then-contract] is applied; otherwise, the
@scheme[else-contract] is applied. The resulting contract is a flat
contract if both @scheme[then-contract] and @scheme[else-contract] are
flat contracts.

For example, the following contract enforces that if a value is a
procedure, it is a thunk; otherwise it can be any (non-procedure)
value:
  @schemeblock[(if/c procedure? (-> any) any/c)]
Note that the following contract is @bold{not} equivalent:
  @schemeblock[(or/c (-> any) any/c) (code:comment "wrong!")]
The last contract is the same as @scheme[any/c] because
@scheme[or/c] tries flat contracts before higher-order contracts.

}
