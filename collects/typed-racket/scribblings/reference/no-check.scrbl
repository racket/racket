#lang scribble/manual

@begin[(require "../utils.rkt")
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@title{Typed Racket Syntax Without Type Checking}

@defmodulelang*[(typed/racket/no-check
                 typed/racket/base/no-check)]

On occasions where the Typed Racket syntax is useful, but actual
typechecking is not desired, the @racketmodname[typed/racket/no-check]
and @racketmodname[typed/racket/base/no-check] languages are useful.
They provide the same bindings and syntax as
@racketmodname[typed/racket] and @racketmodname[typed/racket/base],
but do no type checking.

Examples:

@racketmod[typed/racket/no-check
(: x Number)
(define x "not-a-number")]
