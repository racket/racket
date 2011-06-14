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


@title{Experimental Features}

These features are currently experimental and subject to change.

@defform[(Class args ...)]{A type constructor for typing classes created using @racketmodname[racket/class].}
@defform[(Instance c)]{A type constructor for typing objects created using @racketmodname[racket/class].}

@defform[(:type t)]{Prints the type @racket[_t].}

@defform[(declare-refinement id)]{Declares @racket[id] to be usable in
refinement types.}

@defform[(Refinement id)]{Includes values that have been tested with the
predicate @racket[id], which must have been specified with
@racket[declare-refinement].}

@defform[(define-typed-struct/exec forms ...)]{Defines an executable structure.}
