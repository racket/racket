#lang scribble/doc
@(require (except-in "utils.rkt" _fun)
          (for-label scheme/match
                     (only-in ffi/unsafe/static _fun))
          (for-syntax racket/base)
          scribble/eval
          scribble/racket)

@(begin
   (define-syntax-rule (define-dynamic_fun id)
      (begin
       (require (for-label ffi/unsafe))
       (define id @racket[_fun])))
    (define-dynamic_fun dynamic_fun))

@title[#:tag "static-fun"]{Static Callout and Callback Cores}

@defmodule[ffi/unsafe/static]{The
@racketmodname[ffi/unsafe/static] library provides the same bindings
as @racketmodname[ffi/unsafe], but with a replacement @racket[_fun]
form.}

@history[#:added "8.11.0.2"]

@defform[#:literals (->> :: :)
         (_fun fun-option ... maybe-args type-spec ... ->> type-spec
               maybe-wrapper)]{

Like @dynamic_fun from @racketmodname[ffi/unsafe], but triggers an
error at compile time in the @CS[] implementation of Racket if the
compiler is unable to infer enough information about the resulting C
type to statically generate code for @tech{callouts} and
@tech{callbacks} using the type.

The @racket[type-spec] forms and some @racket[fun-option] forms within
@racket[_fun] are arbitrary expressions that can compute C types and
options at run time. If the optimizer can statically infer underlying
representations, then it can generate the necessary code for a
@tech{callout} or @tech{callback} statically, instead of deferring
code generation to run time. This optimization applies even when using
@dynamic_fun from @racketmodname[ffi/unsafe], but @racket[_fun] from
@racketmodname[ffi/unsafe/static] insists that the optimization must
apply.

Currently, the benefit of static generation for @tech{callout} and
@tech{callback} code is limited, because run-time code generation is
fast and cached. In the long run, static generation may provide more
benefit.

}
