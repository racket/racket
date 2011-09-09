#lang scribble/manual

@begin[(require "../utils.rkt")
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@title{Experimental Features}

These features are currently experimental and subject to change.

@defform[(Class args ...)]{A type constructor for typing classes created using @racketmodname[racket/class].}
@defform[(Instance c)]{A type constructor for typing objects created using @racketmodname[racket/class].}

@defform[(:type t)]{Prints the type @racket[_t].}

@defform[(:print-type e)]{Prints the type of @racket[_e]. This prints the whole
type, which can sometimes be quite large.}

@defform[(:query-result-type f t)]{Given a function @racket[f] and a desired
return type @racket[t], shows the arguments types @racket[f] should be given to
return a value of type @racket[t].}

@defform[(declare-refinement id)]{Declares @racket[id] to be usable in
refinement types.}

@defform[(Refinement id)]{Includes values that have been tested with the
predicate @racket[id], which must have been specified with
@racket[declare-refinement].}

@defform[(define-typed-struct/exec forms ...)]{Defines an executable structure.}
