#lang scribble/manual

@begin[(require "../utils.rkt")
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@title{Exploring Types}

In addition to printing a summary of the types of REPL results, Typed Racket
provides interactive utilities to explore and query types.
The following bindings are only available at the Typed Racket REPL.

@defform[(:type t)]{Prints the type @racket[_t].}

@defform[(:print-type e)]{Prints the type of @racket[_e]. This prints the whole
type, which can sometimes be quite large.}

@defform[(:query-type/args f t ...)]{Given a function @racket[f] and argument
types @racket[t], shows the result type of @racket[f].}

@defform[(:query-type/result f t)]{Given a function @racket[f] and a desired
return type @racket[t], shows the arguments types @racket[f] should be given to
return a value of type @racket[t].}
