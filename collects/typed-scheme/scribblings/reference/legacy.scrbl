#lang scribble/manual

@begin[(require "../utils.rkt" scribble/eval scriblib/footnote
                racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])
                           (only-in racket/base for)
                           racket/list srfi/14
                           version/check))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))


@title{Legacy Forms}

The following forms are provided by Typed Racket for backwards
compatibility.  

@defidform[define-type-alias]{Equivalent to @racket[define-type].}
@defidform[define-typed-struct]{Equivalent to @racket[define-struct:]}
@defidform[require/opaque-type]{Similar to using the @racket[opaque]
keyword with @racket[require/typed].}
@defidform[require-typed-struct]{Similar to using the @racket[struct]
keyword with @racket[require/typed].}
@defidform[pdefine:]{Defines a polymorphic function.}
@defform[(pred t)]{Equivalent to @racket[(Any -> Boolean : t)].}

@defalias[Un U]
@defalias[mu Rec]
@defalias[Tuple List]
@defalias[Parameter Parameterof]
@defalias[Pair Pairof]
