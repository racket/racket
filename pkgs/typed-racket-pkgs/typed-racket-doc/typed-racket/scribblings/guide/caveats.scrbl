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

@section{Typed-untyped interaction and contract generation}

When a typed module @racket[require]s bindings from an untyped
module (or vice-versa), there are some types that cannot be
converted to a corresponding contract.

This could happen because a type is not yet supported in the
contract system, because Typed Racket's contract generator has
not been updated, or because the contract is too difficult
to generate. In some of these cases, the limitation will be
fixed in a future release.

The following illustrates an example type that cannot be
converted to a contract:

@interaction[#:eval the-eval
  (require/typed racket/base
    [object-name (case-> (-> Struct-Type-Property Symbol)
                         (-> Regexp (U String Bytes)))])
]

This function type by cases is a valid type, but a corresponding
contract is difficult to generate because the check on the result
depends on the check on the domain. In the future, this may be
supported with dependent contracts.

A more approximate type will work for this case, but with a loss
of type precision at use sites:

@interaction[#:eval the-eval
  (require/typed racket/base
    [object-name (-> (U Struct-Type-Property Regexp)
                     (U String Bytes Symbol))])
  (object-name #rx"a regexp")
]

@section{Unsupported features}

Units are not currently supported at all in Typed Racket, but they
will potentially be supported in a future version.

Most structure type properties do not work in Typed Racket, including
support for generic interfaces.

@section{Type generalization}

Not so much a caveat as a feature that may have unexpected consequences.
To make programming with invariant type constructors (such as @racket[Boxof])
easier, Typed Racket generalizes types that are used as arguments to invariant
type constructors. For example:

@interaction[#:eval the-eval
  0
  (define b (box 0))
  b
]

@racket[0] has type @racket[Zero], which means that @racket[b] ``should'' have
type @racket[(Boxof Zero)]. On the other hand, that type is not especially
useful, as it only allows @racket[0] to be stored in the box. Most likely, the
intent was to have a box of a more general type (such as @racket[Integer]) and
initialize it with @racket[0]. Type generalization does exactly that.

In some cases, however, type generalization can lead to unexpected results:

@interaction[#:eval the-eval
  (box (ann 1 Fixnum))
]

The intent of this code may be to create of box of @racket[Fixnum], but Typed
Racket will generalize it anyway. To create a box of @racket[Fixnum], the box
itself should have a type annotation:

@interaction[#:eval the-eval
  (ann (box 1) (Boxof Fixnum))
  ((inst box Fixnum) 1)
]

@section{Macros and compile-time computation}

Typed Racket will type-check all expressions at the run-time phase of
the given module and will prevent errors that would occur at run-time.
However, expressions at compile-time---including computations that
occur inside macros---are not checked.

Concretely, this means that expressions inside, for example, a
@racket[begin-for-syntax] block are not checked:

@interaction[#:eval the-eval
  (begin-for-syntax (+ 1 "foo"))
]

Similarly, expressions inside of macros defined in Typed Racket are
not type-checked. On the other hand, the macro's expansion is always
type-checked:

@defs+int[#:eval the-eval
  ((define-syntax (example-1 stx)
     (+ 1 "foo")
     #'1)
   (define-syntax (example-2 stx)
     #'(+ 1 "foo")))
  (example-1)
  (example-2)
]

Note that functions defined in Typed Racket that are used at
compile-time in other typed modules or untyped modules will be
type-checked and then protected with contracts as described in
@secref["typed-untyped-interaction"].

Additionally, macros that are defined in Typed Racket modules cannot
be used in ordinary Racket modules because such uses can
circumvent the protections of the type system.

@section{Expensive contract boundaries}

Contract boundaries installed for typed-untyped interaction may cause
significant slowdowns. See @secref{contract-costs} for details.
