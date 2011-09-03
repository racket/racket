#lang scribble/manual

@begin[(require "../utils.rkt" scriblib/footnote)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@title{Optimization in Typed Racket}

@note{
See
@secref[#:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")]{optimization}
in the guide for tips to get the most out of the optimizer.
}

Typed Racket provides a type-driven optimizer that rewrites well-typed
programs to potentially make them faster. It should in no way make
your programs slower or unsafe.

Typed Racket's optimizer is turned on by default. If you want to
deactivate it (for debugging, for instance), you must add the
@racket[#:no-optimize] keyword when specifying the language of your
program:

@racketmod[typed/racket #:no-optimize]
