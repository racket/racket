#lang scribble/manual

@begin[(require (for-label (only-meta-in 0 typed/racket)) scribble/eval
		"utils.rkt" (only-in "quick.scrbl" typed-mod))]

@(define the-eval (make-base-eval))
@(the-eval '(require typed/racket))

@title[#:tag "optimization"]{Optimization in Typed Racket}

Typed Racket provides a type-driven optimizer that rewrites well-typed
programs to potentially make them faster. It should in no way make
your programs slower or unsafe.

@section{Using the optimizer}

Typed Racket's optimizer is not currently turned on by default. If you
want to activate it, you must add the @racket[#:optimize] keyword when
specifying the language of your program:

@racketmod[typed/racket #:optimize]
