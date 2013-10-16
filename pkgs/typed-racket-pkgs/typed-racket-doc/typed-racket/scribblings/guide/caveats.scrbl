#lang scribble/manual

@(require "../utils.rkt"
          scribble/eval
          (for-label (only-meta-in 0 typed/racket)))

@(define the-eval (make-base-eval))
@(the-eval '(require typed/racket))

@title[#:tag "caveats"]{Caveats and Limitations}

This section describes limitations and subtle aspects of the
type system that programmers often stumble on while porting programs
to Typed Racket.

@section{The @racket[Integer] type and @racket[integer?]}

In Typed Racket, the @racket[Integer] type corresponds to values
that return @racket[#t] for the @racket[exact-integer?] predicate,
@bold{@emph{not}} the @racket[integer?] predicate. In particular,
values that return @racket[#t] for @racket[integer?] may be
@rtech{inexact number}s (e.g, @racket[1.0]).

When porting a program to Typed Racket, you may need to replace
uses of functions like @racket[round] and @racket[floor] with
corresponding exact functions like @racket[exact-round] and
@racket[exact-floor].

In other cases, it may be necessary to use @racket[assert]ions
or @racket[cast]s.

@section{Type inference for polymorphic functions}

Typed Racket's local type inference algorithm is currently not
able to infer types for polymorphic functions that are used
on higher-order arguments that are themselves polymorphic.

For example, the following program results in a type error
that demonstrates this limitation:

@interaction[#:eval the-eval
  (map cons '(a b c d) '(1 2 3 4))
]

The issue is that the type of @racket[cons] is also polymorphic:

@interaction[#:eval the-eval cons]

To make this expression type-check, the @racket[inst] form can
be used to instantiate the polymorphic argument (e.g., @racket[cons])
at a specific type:

@interaction[#:eval the-eval
  (map (inst cons Symbol Integer) '(a b c d) '(1 2 3 4))
]

