#lang scribble/manual

@begin[(require "../utils.rkt")
       (require scribble/eval)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@title{Exploring Types}

In addition to printing a summary of the types of REPL results, Typed Racket
provides interactive utilities to explore and query types.
The following bindings are only available at the Typed Racket REPL.

@defform[(:type maybe-verbose t)
         #:grammar ([maybe-verbose (code:line)
                                   (code:line #:verbose)])]{
  Prints the type @racket[_t]. If @racket[_t] is a type alias
  (e.g., @racket[Number]), then it will be expanded to its representation
  when printing. Any further type aliases in the type named by @racket[_t]
  will remain unexpanded.

  If @racket[#:verbose] is provided, all type aliases are expanded
  in the printed type.

  @examples[#:eval the-top-eval
    ;; I'm not sure why, but the :type examples below don't work
    ;; without the #%top-interaction in the first example
    (eval:alts (:type Number) (#%top-interaction . (:type Number)))
    (:type Real)
    (:type #:verbose Number)
  ]
}

@defform[(:print-type e)]{Prints the type of @racket[_e]. This prints the whole
type, which can sometimes be quite large.}

@defform[(:query-type/args f t ...)]{Given a function @racket[f] and argument
types @racket[t], shows the result type of @racket[f].}

@defform[(:query-type/result f t)]{Given a function @racket[f] and a desired
return type @racket[t], shows the arguments types @racket[f] should be given to
return a value of type @racket[t].}

@close-eval[the-top-eval]

