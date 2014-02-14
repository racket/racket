#lang scribble/manual

@(require "../utils.rkt"
          scribble/eval
          (for-label (only-meta-in 0 typed/racket)))

@(define the-eval (make-base-eval))
@(the-eval '(require typed/racket))

@title[#:tag "typed-untyped-interaction"]{Typed-Untyped Interaction}

In the previous sections, all of the examples have consisted of programs
that are entirely typed. One of the key features of Typed Racket is that
it allows the combination of both typed and untyped code in a single
program.

@section{Using Untyped Code from Typed Code}

Suppose that we write the untyped module from @secref["quick"] again:

@racketmod[#:file "distance.rkt"
racket

(provide (struct-out pt)
         distance)

(struct pt (x y))

(code:contract distance : pt pt -> real)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))
]

If we want to use the @racket[_distance] function defined in the above module
from a typed module, we need to use the @racket[require/typed] form to import
it. Since the untyped module did not specify any types, we need to annotate
the imports with types (just like how the example in @secref["quick"] had
additional type annotations with @racket[:]):

@margin-note{Note that a typed module @emph{does not} need to use @racket[require/typed]
to import from another typed module. The @racket[require] form will work in such
cases.}

@racketmod[#:file "client.rkt"
typed/racket

(require/typed "distance.rkt"
               [#:struct pt ([x : Real] [y : Real])]
               [distance (-> pt pt Real)])

(distance (pt 3 5) (p 7 0))
]

The @racket[require/typed] form has several kinds of clauses. The
@racket[#:struct] clause specifies the import of a structure type
and allows us to use the structure type as if it were defined with
@racket[struct:].

The second clause in the example above specifies that a given
binding @racket[_distance] has the given type @racket[(-> pt pt Real)].

Note that the @racket[require/typed] form can import bindings
from any module, including those that are part of the Racket standard
library. For example,

@racketmod[
typed/racket

(require/typed racket/base [add1 (-> Integer Integer)])
]

is a valid use of the @racket[require/typed] form and imports @racket[add1]
from the @racketmodname[racket/base] library.

@section{Using Typed Code in Untyped Code}

In the previous subsection, we saw that the use of untyped code from
typed code requires the use of @racket[require/typed]. However, the
use of code in the other direction (i.e., the use of typed code from
untyped code) requires no additional work.

If an untyped module @racket[require]s a typed module, it will be able
to use the bindings defined in the typed module as expected. The major
exception to this rule is that macros defined in typed modules may not
be used in untyped modules.

@section{Protecting Typed-Untyped Interaction}

One might wonder if the interactions described in the first two
subsections are actually safe; after all, untyped code might be able to
ignore the errors that Typed Racket's type system will catch at
compile-time.

To ensure that typed-untyped interactions are safe, Typed Racket establishes
contracts wherever typed and untyped code interact. For example, suppose
that we write an untyped module that implements an @racket[_increment]
function:

@margin-note{For general information on Racket's contract system
, see @secref[#:doc '(lib "scribblings/guide/guide.scrbl")]{contracts}.}

@interaction[#:eval the-eval
(module increment racket
  (provide increment)

  (code:comment "increment : exact-integer? -> exact-integer?")
  (define (increment x) "this is broken"))
]

and a typed module that uses it:

@interaction[#:eval the-eval
(module client typed/racket

  (require/typed 'increment [increment (-> Integer Integer)])

  (increment 5))
]

This combined program is not correct. All uses of @racket[_increment]
in Typed Racket are correct under the assumption that the
@racket[_increment] function upholds the @racket[(-> Integer Integer)]
type. Unfortunately, our @racket[_increment] implementation does not
actually uphold this assumption, because the function actually produces
strings.

On the other hand, when the program is run:

@interaction[#:eval the-eval (require 'client)]

we find that the contract system checks the assumption made by the typed
module and correctly finds that the assumption failed because of the
implementation in the untyped module (hence it is @emph{blamed} in the
error message).

In the same fashion, Typed Racket checks all functions and other values
that pass from a typed module to untyped module or vice versa with
contracts. This means that, for example, Typed Racket can safely optimize
programs (see @secref["optimization"]) with the assurance that the program
will not segfault due to an unchecked assumption.

@bold{Important caveat}: contracts such as the @racket[Integer] check from
above are performant. However, contracts in general can
have a non-trivial performance impact, especially with the use of first-class
functions or other higher-order data such as vectors.

Note that no contract overhead is ever incurred for uses of typed
values from another typed module.

@close-eval[the-eval]

