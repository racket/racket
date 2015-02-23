#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label racket/unit racket/class))

@(define toy-eval (make-base-eval))

@(interaction-eval #:eval toy-eval (require racket/unit))

@(define-syntax-rule (racketmod/eval [pre ...] form more ...)
   (begin
     (racketmod pre ... form more ...)
     (interaction-eval #:eval toy-eval form)))

@title[#:tag "units" #:style 'toc]{Units@aux-elem{ (Components)}}

@deftech{Units} organize a program into separately compilable and
reusable @deftech{components}. A unit resembles a procedure in that
both are first-class values that are used for abstraction. While
procedures abstract over values in expressions, units abstract over
names in collections of definitions. Just as a procedure is called to
evaluate its expressions given actual arguments for its formal
parameters, a unit is @deftech{invoked} to evaluate its definitions
given actual references for its imported variables. Unlike a
procedure, however, a unit's imported variables can be partially
linked with the exported variables of another unit @italic{prior to
invocation}. Linking merges multiple units together into a single
compound unit. The compound unit itself imports variables that will be
propagated to unresolved imported variables in the linked units, and
re-exports some variables from the linked units for further linking.

@local-table-of-contents[]

@; ----------------------------------------

@section{Signatures and Units}

The interface of a unit is described in terms of
@deftech{signatures}. Each signature is defined (normally within a
@racket[module]) using @racket[define-signature].  For example, the
following signature, placed in a @filepath{toy-factory-sig.rkt} file,
describes the exports of a component that implements a toy factory:

@margin-note{By convention, signature names end with @litchar{^}.}

@racketmod/eval[[#:file
"toy-factory-sig.rkt"
racket]

(define-signature toy-factory^
  (build-toys  (code:comment #, @tt{(integer? -> (listof toy?))})
   repaint     (code:comment #, @tt{(toy? symbol? -> toy?)})
   toy?        (code:comment #, @tt{(any/c -> boolean?)})
   toy-color)) (code:comment #, @tt{(toy? -> symbol?)})

(provide toy-factory^)
]

An implementation of the @racket[toy-factory^] signature is written
using @racket[define-unit] with an @racket[export] clause that names
@racket[toy-factory^]:

@margin-note{By convention, unit names end with @litchar["@"].}

@racketmod/eval[[#:file
"simple-factory-unit.rkt"
racket

(require "toy-factory-sig.rkt")]

(define-unit simple-factory@
  (import)
  (export toy-factory^)

  (printf "Factory started.\n")

  (define-struct toy (color) #:transparent)

  (define (build-toys n)
    (for/list ([i (in-range n)])
      (make-toy 'blue)))

  (define (repaint t col)
    (make-toy col)))

(provide simple-factory@)
]

The @racket[toy-factory^] signature also could be referenced by a unit
that needs a toy factory to implement something else. In that case,
@racket[toy-factory^] would be named in an @racket[import] clause.
For example, a toy store would get toys from a toy factory. (Suppose,
for the sake of an example with interesting features, that the store
is willing to sell only toys in a particular color.)

@racketmod/eval[[#:file
"toy-store-sig.rkt"
racket]

(define-signature toy-store^
  (store-color     (code:comment #, @tt{(-> symbol?)})
   stock!          (code:comment #, @tt{(integer? -> void?)})
   get-inventory)) (code:comment #, @tt{(-> (listof toy?))})

(provide toy-store^)
]

@racketmod/eval[[#:file
"toy-store-unit.rkt"
racket

(require "toy-store-sig.rkt"
         "toy-factory-sig.rkt")]

(define-unit toy-store@
  (import toy-factory^)
  (export toy-store^)

  (define inventory null)

  (define (store-color) 'green)

  (define (maybe-repaint t)
    (if (eq? (toy-color t) (store-color))
        t
        (repaint t (store-color))))

  (define (stock! n)
    (set! inventory 
          (append inventory
                  (map maybe-repaint
                       (build-toys n)))))

  (define (get-inventory) inventory))

(provide toy-store@)
]

Note that @filepath{toy-store-unit.rkt} imports
@filepath{toy-factory-sig.rkt}, but not
@filepath{simple-factory-unit.rkt}.  Consequently, the
@racket[toy-store@] unit relies only on the specification of a toy
factory, not on a specific implementation.

@; ----------------------------------------

@section{Invoking Units}

The @racket[simple-factory@] unit has no imports, so it can be
@tech{invoked} directly using @racket[invoke-unit]:

@interaction[
#:eval toy-eval
(eval:alts (require "simple-factory-unit.rkt") (void))
(invoke-unit simple-factory@)
]

The @racket[invoke-unit] form does not make the body definitions
available, however, so we cannot build any toys with this factory. The
@racket[define-values/invoke-unit] form binds the identifiers of a
signature to the values supplied by a unit (to be @tech{invoked}) that
implements the signature:

@interaction[
#:eval toy-eval
(define-values/invoke-unit/infer simple-factory@)
(build-toys 3)
]

Since @racket[simple-factory@] exports the @racket[toy-factory^]
signature, each identifier in @racket[toy-factory^] is defined by the
@racket[define-values/invoke-unit/infer] form. The
@racketidfont{/infer} part of the form name indicates that the
identifiers bound by the declaration are inferred from
@racket[simple-factory@].

Now that the identifiers in @racket[toy-factory^] are defined, we can
also invoke @racket[toy-store@], which imports @racket[toy-factory^]
to produce @racket[toy-store^]:

@interaction[
#:eval toy-eval
(eval:alts (require "toy-store-unit.rkt") (void))
(define-values/invoke-unit/infer toy-store@)
(get-inventory)
(stock! 2)
(get-inventory)
]

Again, the @racketidfont{/infer} part
@racket[define-values/invoke-unit/infer] determines that
@racket[toy-store@] imports @racket[toy-factory^], and so it supplies
the top-level bindings that match the names in @racket[toy-factory^]
as imports to @racket[toy-store@].

@; ----------------------------------------

@section{Linking Units}

We can make our toy economy more efficient by having toy factories
that cooperate with stores, creating toys that do not have to be
repainted. Instead, the toys are always created using the store's
color, which the factory gets by importing @racket[toy-store^]:

@racketmod/eval[[#:file
"store-specific-factory-unit.rkt"
racket

(require "toy-store-sig.rkt"
         "toy-factory-sig.rkt")]

(define-unit store-specific-factory@
  (import toy-store^)
  (export toy-factory^)

  (define-struct toy () #:transparent)

  (define (toy-color t) (store-color))

  (define (build-toys n)
    (for/list ([i (in-range n)])
      (make-toy)))

  (define (repaint t col)
    (error "cannot repaint")))

(provide store-specific-factory@)
]

To invoke @racket[store-specific-factory@], we need
@racket[toy-store^] bindings to supply to the unit. But to get
@racket[toy-store^] bindings by invoking @racket[toy-store@], we will
need a toy factory! The unit implementations are mutually dependent,
and we cannot invoke either before the other.

The solution is to @deftech{link} the units together, and then we can
invoke the combined units. The @racket[define-compound-unit/infer] form
links any number of units to form a combined unit. It can propagate
imports and exports from the linked units, and it can satisfy each
unit's imports using the exports of other linked units.

@interaction[
#:eval toy-eval
(eval:alts (require "toy-factory-sig.rkt") (void))
(eval:alts (require "toy-store-sig.rkt") (void))
(eval:alts (require "store-specific-factory-unit.rkt") (void))
(define-compound-unit/infer toy-store+factory@
  (import)
  (export toy-factory^ toy-store^)
  (link store-specific-factory@
        toy-store@))
]

The overall result above is a unit @racket[toy-store+factory@] that
exports both @racket[toy-factory^] and @racket[toy-store^]. The
connection between @racket[store-specific-factory@] and
@racket[toy-store@] is inferred from the signatures that each imports
and exports.

This unit has no imports, so we can always invoke it:

@interaction[
#:eval toy-eval
(define-values/invoke-unit/infer toy-store+factory@)
(stock! 2)
(get-inventory)
(map toy-color (get-inventory))
]

@; ----------------------------------------

@section[#:tag "firstclassunits"]{First-Class Units}

The @racket[define-unit] form combines @racket[define] with a
@racket[unit] form, similar to the way that @racket[(define (f x)
....)]  combines @racket[define] followed by an identifier with an
implicit @racket[lambda].

Expanding the shorthand, the definition of @racket[toy-store@] could
almost be written as

@racketblock[
(define toy-store@
  (unit
   (import toy-factory^)
   (export toy-store^)

   (define inventory null)

   (define (store-color) 'green)
   ....))
]

A difference between this expansion and @racket[define-unit] is that
the imports and exports of @racket[toy-store@] cannot be
inferred. That is, besides combining @racket[define] and
@racket[unit], @racket[define-unit] attaches static information to the
defined identifier so that its signature information is available
statically to @racket[define-values/invoke-unit/infer] and other
forms.

Despite the drawback of losing static signature information,
@racket[unit] can be useful in combination with other forms that work
with first-class values. For example, we could wrap a @racket[unit]
that creates a toy store in a @racket[lambda] to supply the store's
color:

@racketmod/eval[[#:file
"toy-store-maker.rkt"
racket

(require "toy-store-sig.rkt"
         "toy-factory-sig.rkt")]

(define toy-store@-maker
  (lambda (the-color)
    (unit
     (import toy-factory^)
     (export toy-store^)

     (define inventory null)

     (define (store-color) the-color)

     (code:comment @#,t{the rest is the same as before})

     (define (maybe-repaint t)
       (if (eq? (toy-color t) (store-color))
           t
           (repaint t (store-color))))

     (define (stock! n)
       (set! inventory
             (append inventory
                     (map maybe-repaint
                          (build-toys n)))))

     (define (get-inventory) inventory))))

(provide toy-store@-maker)
]

To invoke a unit created by @racket[toy-store@-maker], we must use
@racket[define-values/invoke-unit], instead of the
@racketidfont{/infer} variant:

@interaction[
#:eval toy-eval
(eval:alts (require "simple-factory-unit.rkt") (void))
(define-values/invoke-unit/infer simple-factory@)
(eval:alts (require "toy-store-maker.rkt") (void))
(define-values/invoke-unit (toy-store@-maker 'purple)
  (import toy-factory^)
  (export toy-store^))
(stock! 2)
(get-inventory)
]

In the @racket[define-values/invoke-unit] form, the @racket[(import
toy-factory^)] line takes bindings from the current context that match
the names in @racket[toy-factory^] (the ones that we created by
invoking @racket[simple-factory@]), and it supplies them as imports to
@racket[toy-store@]. The @racket[(export toy-store^)] clause indicates
that the unit produced by @racket[toy-store@-maker] will export
@racket[toy-store^], and the names from that signature are defined
after invoking the unit.

To link a unit from @racket[toy-store@-maker], we can use the
@racket[compound-unit] form:

@interaction[
#:eval toy-eval
(eval:alts (require "store-specific-factory-unit.rkt") (void))
(define toy-store+factory@
  (compound-unit
   (import)
   (export TF TS)
   (link [((TF : toy-factory^)) store-specific-factory@ TS]
         [((TS : toy-store^)) toy-store@ TF])))
]

This @racket[compound-unit] form packs a lot of information into one
place. The left-hand-side @racket[TF] and @racket[TS] in the
@racket[link] clause are binding identifiers. The identifier
@racket[TF] is essentially bound to the elements of
@racket[toy-factory^] as implemented by
@racket[store-specific-factory@].  The identifier @racket[TS] is
similarly bound to the elements of @racket[toy-store^] as implemented
by @racket[toy-store@]. Meanwhile, the elements bound to @racket[TS]
are supplied as imports for @racket[store-specific-factory@], since
@racket[TS] follows @racket[store-specific-factory@]. The elements
bound to @racket[TF] are similarly supplied to
@racket[toy-store@]. Finally, @racket[(export TF TS)] indicates that
the elements bound to @racket[TF] and @racket[TS] are exported from
the compound unit.

The above @racket[compound-unit] form uses
@racket[store-specific-factory@] as a first-class unit, even though
its information could be inferred. Every unit can be used as a
first-class unit, in addition to its use in inference contexts. Also,
various forms let a programmer bridge the gap between inferred and
first-class worlds. For example, @racket[define-unit-binding] binds a
new identifier to the unit produced by an arbitrary expression; it
statically associates signature information to the identifier, and it
dynamically checks the signatures against the first-class unit
produced by the expression.

@; ----------------------------------------

@section{Whole-@racket[module] Signatures and Units}

In programs that use units, modules like @filepath{toy-factory-sig.rkt}
and @filepath{simple-factory-unit.rkt} are common. The
@racket[racket/signature] and @racket[racket/unit] module names can be
used as languages to avoid much of the boilerplate module, signature,
and unit declaration text.

For example, @filepath{toy-factory-sig.rkt} can be written as

@racketmod[
racket/signature

build-toys  (code:comment #, @tt{(integer? -> (listof toy?))})
repaint     (code:comment #, @tt{(toy? symbol? -> toy?)})
toy?        (code:comment #, @tt{(any/c -> boolean?)})
toy-color   (code:comment #, @tt{(toy? -> symbol?)})
]

The signature @racket[toy-factory^] is automatically provided from the
module, inferred from the filename @filepath{toy-factory-sig.rkt} by
replacing the @filepath{-sig.rkt} suffix with @racketidfont{^}.

Similarly, @filepath{simple-factory-unit.rkt} module can be written

@racketmod[
racket/unit

(require "toy-factory-sig.rkt")

(import)
(export toy-factory^)

(printf "Factory started.\n")

(define-struct toy (color) #:transparent)

(define (build-toys n)
  (for/list ([i (in-range n)])
    (make-toy 'blue)))

(define (repaint t col)
  (make-toy col))
]

The unit @racket[simple-factory@] is automatically provided from the
module, inferred from the filename @filepath{simple-factory-unit.rkt} by
replacing the @filepath{-unit.rkt} suffix with @racketidfont["@"].

@; ----------------------------------------

@(interaction-eval #:eval toy-eval (require racket/contract))

@section{Contracts for Units}

There are a couple of ways of protecting units with contracts.  One way
is useful when writing new signatures, and the other handles the case
when a unit must conform to an already existing signature.

@subsection{Adding Contracts to Signatures}

When contracts are added to a signature, then all units which implement
that signature are protected by those contracts.  The following version
of the @racket[toy-factory^] signature adds the contracts previously
written in comments:

@racketmod/eval[[#:file
"contracted-toy-factory-sig.rkt"
racket]

(define-signature contracted-toy-factory^
  ((contracted
    [build-toys (-> integer? (listof toy?))]
    [repaint    (-> toy? symbol? toy?)]
    [toy?       (-> any/c boolean?)]
    [toy-color  (-> toy? symbol?)])))

(provide contracted-toy-factory^)]

Now we take the previous implementation of @racket[simple-factory@] and
implement this version of @racket[toy-factory^] instead:

@racketmod/eval[[#:file
"contracted-simple-factory-unit.rkt"
racket

(require "contracted-toy-factory-sig.rkt")]

(define-unit contracted-simple-factory@
  (import)
  (export contracted-toy-factory^)

  (printf "Factory started.\n")

  (define-struct toy (color) #:transparent)

  (define (build-toys n)
    (for/list ([i (in-range n)])
      (make-toy 'blue)))

  (define (repaint t col)
    (make-toy col)))

(provide contracted-simple-factory@)
]

As before, we can invoke our new unit and bind the exports so
that we can use them.  This time, however, misusing the exports
causes the appropriate contract errors.

@interaction[
#:eval toy-eval
(eval:alts (require "contracted-simple-factory-unit.rkt") (void))
(define-values/invoke-unit/infer contracted-simple-factory@)
(build-toys 3)
(build-toys #f)
(repaint 3 'blue)
]

@subsection{Adding Contracts to Units}

However, sometimes we may have a unit that must conform to an
already existing signature that is not contracted.  In this case,
we can create a unit contract with @racket[unit/c] or use
the @racket[define-unit/contract] form, which defines a unit which
has been wrapped with a unit contract.

For example, here's a version of @racket[toy-factory@] which still
implements the regular @racket[toy-factory^], but whose exports
have been protected with an appropriate unit contract.

@racketmod/eval[[#:file
"wrapped-simple-factory-unit.rkt"
racket

(require "toy-factory-sig.rkt")]

(define-unit/contract wrapped-simple-factory@
  (import)
  (export (toy-factory^
           [build-toys (-> integer? (listof toy?))]
           [repaint    (-> toy? symbol? toy?)]
           [toy?       (-> any/c boolean?)]
           [toy-color  (-> toy? symbol?)]))

  (printf "Factory started.\n")

  (define-struct toy (color) #:transparent)

  (define (build-toys n)
    (for/list ([i (in-range n)])
      (make-toy 'blue)))

  (define (repaint t col)
    (make-toy col)))

(provide wrapped-simple-factory@)
]

@interaction[
#:eval toy-eval
(eval:alts (require "wrapped-simple-factory-unit.rkt") (void))
(define-values/invoke-unit/infer wrapped-simple-factory@)
(build-toys 3)
(build-toys #f)
(repaint 3 'blue)
]


@; ----------------------------------------

@section{@racket[unit] versus @racket[module]}

As a form for modularity, @racket[unit] complements @racket[module]:

@itemize[

 @item{The @racket[module] form is primarily for managing a universal
       namespace. For example, it allows a code fragment to refer
       specifically to the @racket[car] operation from
       @racketmodname[racket/base]---the one that extracts the first
       element of an instance of the built-in pair datatype---as
       opposed to any number of other functions with the name
       @racket[car]. In other words, the @racket[module] construct lets
       you refer to @emph{the} binding that you want.}

 @item{The @racket[unit] form is for parameterizing a code fragment
       with respect to most any kind of run-time value. For example,
       it allows a code fragment to work with a @racket[car]
       function that accepts a single argument, where the specific
       function is determined later by linking the fragment to
       another. In other words, the @racket[unit] construct lets you
       refer to @emph{a} binding that meets some specification.}

]

The @racket[lambda] and @racket[class] forms, among others, also allow
paremetrization of code with respect to values that are chosen
later. In principle, any of those could be implemented in terms of any
of the others. In practice, each form offers certain
conveniences---such as allowing overriding of methods or especially
simple application to values---that make them suitable for different
purposes.

The @racket[module] form is more fundamental than the others, in a
sense. After all, a program fragment cannot reliably refer to a
@racket[lambda], @racket[class], or @racket[unit] form without the
namespace management provided by @racket[module]. At the same time,
because namespace management is closely related to separate expansion
and compilation, @racket[module] boundaries end up as
separate-compilation boundaries in a way that prohibits mutual
dependencies among fragments. For similar reasons, @racket[module]
does not separate interface from implementation.

Use @racket[unit] when @racket[module] by itself almost works, but
when separately compiled pieces must refer to each other, or when you
want a stronger separation between @defterm{interface} (i.e., the
parts that need to be known at expansion and compilation time) and
@defterm{implementation} (i.e., the run-time parts). More generally,
use @racket[unit] when you need to parameterize code over functions,
datatypes, and classes, and when the parameterized code itself
provides definitions to be linked with other parameterized code.

@; ----------------------------------------------------------------------

@close-eval[toy-eval]
