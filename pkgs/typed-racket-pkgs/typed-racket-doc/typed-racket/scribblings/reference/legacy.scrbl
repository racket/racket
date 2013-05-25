#lang scribble/manual

@begin[(require "../utils.rkt")
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@title{Legacy Forms}

The following forms are provided by Typed Racket for backwards
compatibility.

@defidform[define-type-alias]{Equivalent to @racket[define-type].}
@defidform[define-typed-struct]{Equivalent to @racket[define-struct:]}
@defidform[require/opaque-type]{Similar to using the @racket[opaque]
keyword with @racket[require/typed].}
@defidform[require-typed-struct]{Similar to using the @racket[struct]
keyword with @racket[require/typed].}
@defidform[require-typed-struct/provide]{Similar to
@racket[require-typed-struct], but also provides the imported identifiers.}
@defidform[pdefine:]{Defines a polymorphic function.}
@defform[(pred t)]{Equivalent to @racket[(Any -> Boolean : t)].}

@defalias[Un U]
@defalias[mu Rec]
@defalias[Tuple List]
@defalias[Parameter Parameterof]
@defalias[Pair Pairof]
@defalias[values Values]
