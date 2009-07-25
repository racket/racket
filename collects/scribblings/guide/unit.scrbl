#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"

          (for-label scheme/unit
                     scheme/class))

@(define toy-eval (make-base-eval))

@(interaction-eval #:eval toy-eval (require scheme/unit))

@(define-syntax-rule (schememod/eval [pre ...] form more ...)
   (begin
     (schememod pre ... form more ...)
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
@scheme[module]) using @scheme[define-signature].  For example, the
following signature, placed in a @filepath{toy-factory-sig.ss} file,
describes the exports of a component that implements a toy factory:

@margin-note{By convention, signature names with @litchar{^}.}

@schememod/eval[[#:file
"toy-factory-sig.ss"
scheme]

(define-signature toy-factory^
  (build-toys  (code:comment #, @tt{(integer? -> (listof toy?))})
   repaint     (code:comment #, @tt{(toy? symbol? -> toy?)})
   toy?        (code:comment #, @tt{(any/c -> boolean?)})
   toy-color)) (code:comment #, @tt{(toy? -> symbol?)})

(provide toy-factory^)
]

An implementation of the @scheme[toy-factory^] signature is written
using @scheme[define-unit] with an @scheme[export] clause that names
@scheme[toy-factory^]:

@margin-note{By convention, unit names with @litchar["@"].}

@schememod/eval[[#:file
"simple-factory-unit.ss"
scheme

(require "toy-factory-sig.ss")]

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

The @scheme[toy-factory^] signature also could be referenced by a unit
that needs a toy factory to implement something else. In that case,
@scheme[toy-factory^] would be named in an @scheme[import] clause.
For example, a toy store would get toys from a toy factory. (Suppose,
for the sake of an example with interesting features, that the store
is willing to sell only toys in a particular color.)

@schememod/eval[[#:file
"toy-store-sig.ss"
scheme]

(define-signature toy-store^
  (store-color     (code:comment #, @tt{(-> symbol?)})
   stock!          (code:comment #, @tt{(integer? -> void?)})
   get-inventory)) (code:comment #, @tt{(-> (listof toy?))})

(provide toy-store^)
]

@schememod/eval[[#:file
"toy-store-unit.ss"
scheme

(require "toy-store-sig.ss"
         "toy-factory-sig.ss")]

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

Note that @filepath{toy-store-unit.ss} imports
@filepath{toy-factory-sig.ss}, but not
@filepath{simple-factory-unit.ss}.  Consequently, the
@scheme[toy-store@] unit relies only on the specification of a toy
factory, not on a specific implementation.

@; ----------------------------------------

@section{Invoking Units}

The @scheme[simple-factory@] unit has no imports, so it can be
@tech{invoked} directly using @scheme[invoke-unit]:

@interaction[
#:eval toy-eval
(eval:alts (require "simple-factory-unit.ss") (void))
(invoke-unit simple-factory@)
]

The @scheme[invoke-unit] form does not make the body definitions
available, however, so we cannot build any toys with this factory. The
@scheme[define-values/invoke-unit] form binds the identifiers of a
signature to the values supplied by a unit (to be @tech{invoked}) that
implements the signature:

@interaction[
#:eval toy-eval
(define-values/invoke-unit/infer simple-factory@)
(build-toys 3)
]

Since @scheme[simple-factory@] exports the @scheme[toy-factory^]
signature, each identifier in @scheme[toy-factory^] is defined by the
@scheme[define-values/invoke-unit/infer] form. The
@schemeidfont{/infer} part of the form name indicates that the
identifiers bound by the declaration are inferred from
@scheme[simple-factory@].

Now that the identifiers in @scheme[toy-factory^] are defined, we can
also invoke @scheme[toy-store@], which imports @scheme[toy-factory^]
to produce @scheme[toy-store^]:

@interaction[
#:eval toy-eval
(eval:alts (require "toy-store-unit.ss") (void))
(define-values/invoke-unit/infer toy-store@)
(get-inventory)
(stock! 2)
(get-inventory)
]

Again, the @schemeidfont{/infer} part
@scheme[define-values/invoke-unit/infer] determines that
@scheme[toy-store@] imports @scheme[toy-factory^], and so it supplies
the top-level bindings that match the names in @scheme[toy-factory^]
as imports to @scheme[toy-store@].

@; ----------------------------------------

@section{Linking Units}

We can make our toy economy more efficient by having toy factories
that cooperate with stores, creating toys that do not have to be
repainted. Instead, the toys are always created using the store's
color, which the factory gets by importing @scheme[toy-store^]:

@schememod/eval[[#:file
"store-specific-factory-unit.ss"
scheme

(require "toy-factory-sig.ss")]

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

To invoke @scheme[store-specific-factory@], we need
@scheme[toy-store^] bindings to supply to the unit. But to get
@scheme[toy-store^] bindings by invoking @scheme[toy-store@], we will
need a toy factory! The unit implementations are mutually dependent,
and we cannot invoke either before the other.

The solution is to @deftech{link} the units together, and then we can
invoke the combined units. The @scheme[define-compound-unit/infer] form
links any number of units to form a combined unit. It can propagate
imports and exports from the linked units, and it can satisfy each
unit's imports using the exports of other linked units.

@interaction[
#:eval toy-eval
(eval:alts (require "store-specific-factory-unit.ss") (void))
(define-compound-unit/infer toy-store+factory@
  (import)
  (export toy-factory^ toy-store^)
  (link store-specific-factory@
        toy-store@))
]

The overall result above is a unit @scheme[toy-store+factory@] that
exports both @scheme[toy-factory^] and @scheme[toy-store^]. The
connection between @scheme[store-specific-factory@] and
@scheme[toy-store@] is inferred from the signatures that each imports
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

The @scheme[define-unit] form combines @scheme[define] with a
@scheme[unit] form, similar to the way that @scheme[(define (f x)
....)]  combines @scheme[define] followed by an identifier with an
implicit @scheme[lambda].

Expanding the shorthand, the definition of @scheme[toy-store@] could
almost be written as

@schemeblock[
(define toy-store@
  (unit
   (import toy-factory^)
   (export toy-store^)

   (define inventory null)

   (define (store-color) 'green)
   ....))
]

A difference between this expansion and @scheme[define-unit] is that
the imports and exports of @scheme[toy-store@] cannot be
inferred. That is, besides combining @scheme[define] and
@scheme[unit], @scheme[define-unit] attaches static information to the
defined identifier so that its signature information is available
statically to @scheme[define-values/invoke-unit/infer] and other
forms.

Despite the drawback of losing static signature information,
@scheme[unit] can be useful in combination with other forms that work
with first-class values. For example, we could wrap a @scheme[unit]
that creates a toy store in a @scheme[lambda] to supply the store's
color:

@schememod/eval[[#:file
"toy-store-maker.ss"
scheme

(require "toy-store-sig.ss"
         "toy-factory-sig.ss")]

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

To invoke a unit created by @scheme[toy-store@-maker], we must use
@scheme[define-values/invoke-unit], instead of the
@schemeidfont{/infer} variant:

@interaction[
#:eval toy-eval
(eval:alts (require "simple-factory-unit.ss") (void))
(define-values/invoke-unit/infer simple-factory@)
(eval:alts (require "toy-store-maker.ss") (void))
(define-values/invoke-unit (toy-store@-maker 'purple)
  (import toy-factory^)
  (export toy-store^))
(stock! 2)
(get-inventory)
]

In the @scheme[define-values/invoke-unit] form, the @scheme[(import
toy-factory^)] line takes bindings from the current context that match
the names in @scheme[toy-factory^] (the ones that we created by
invoking @scheme[simple-factory@]), and it supplies them as imports to
@scheme[toy-store@]. The @scheme[(export toy-store^)] clause indicates
that the unit produced by @scheme[toy-store@-maker] will export
@scheme[toy-store^], and the names from that signature are defined
after invoking the unit.

To link a unit from @scheme[toy-store@-maker], we can use the
@scheme[compound-unit] form:

@interaction[
#:eval toy-eval
(eval:alts (require "store-specific-factory-unit.ss") (void))
(define toy-store+factory@
  (compound-unit
   (import)
   (export TF TS)
   (link [((TF : toy-factory^)) store-specific-factory@ TS]
         [((TS : toy-store^)) toy-store@ TF])))
]

This @scheme[compound-unit] form packs a lot of information into one
place. The left-hand-side @scheme[TF] and @scheme[TS] in the
@scheme[link] clause are binding identifiers. The identifier
@scheme[TF] is essentially bound to the elements of
@scheme[toy-factory^] as implemented by
@scheme[store-specific-factory@].  The identifier @scheme[TS] is
similarly bound to the elements of @scheme[toy-store^] as implemented
by @scheme[toy-store@]. Meanwhile, the elements bound to @scheme[TS]
are supplied as imports for @scheme[store-specific-factory@], since
@scheme[TS] follows @scheme[store-specific-factory@]. The elements
bound to @scheme[TF] are similarly supplied to
@scheme[toy-store@]. Finally, @scheme[(export TF TS)] indicates that
the elements bound to @scheme[TF] and @scheme[TS] are exported from
the compound unit.

The above @scheme[compound-unit] form uses
@scheme[store-specific-factory@] as a first-class unit, even though
its information could be inferred. Every unit can be used as a
first-class unit, in addition to its use in inference contexts. Also,
various forms let a programmer bridge the gap between inferred and
first-class worlds. For example, @scheme[define-unit-binding] binds a
new identifier to the unit produced by an arbitrary expression; it
statically associates signature information to the identifier, and it
dynamically checks the signatures against the first-class unit
produced by the expression.

@; ----------------------------------------

@section{Whole-@scheme[module] Signatures and Units}

In programs that use units, modules like @filepath{toy-factory-sig.ss}
and @filepath{simple-factory-unit.ss} are common. The
@scheme[scheme/signature] and @scheme[scheme/unit] module names can be
used as languages to avoid much of the boilerplate module, signature,
and unit declaration text.

For example, @filepath{toy-factory-sig.ss} can be written as

@schememod[
scheme/signature

build-toys  (code:comment #, @tt{(integer? -> (listof toy?))})
repaint     (code:comment #, @tt{(toy? symbol? -> toy?)})
toy?        (code:comment #, @tt{(any/c -> boolean?)})
toy-color   (code:comment #, @tt{(toy? -> symbol?)})
]

The signature @scheme[toy-factory^] is automatically provided from the
module, inferred from the filename @filepath{toy-factory-sig.ss} by
replacing the @filepath{-sig.ss} suffix with @schemeidfont{^}.

Similarly, @filepath{simple-factory-unit.ss} module can be written

@schememod[
scheme/unit

(require "toy-factory-sig.ss")

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

The unit @scheme[simple-factory@] is automatically provided from the
module, inferred from the filename @filepath{simple-factory-unit.ss} by
replacing the @filepath{-unit.ss} suffix with @schemeidfont["@"].

@; ----------------------------------------

@(interaction-eval #:eval toy-eval (require scheme/contract))

@section{Contracts for Units}

There are a couple of ways of protecting units with contracts.  One way
is useful when writing new signatures, and the other handles the case
when a unit must conform to an already existing signature.

@subsection{Adding Contracts to Signatures}

When contracts are added to a signature, then all units which implement
that signature are protected by those contracts.  The following version
of the @scheme[toy-factory^] signature adds the contracts previously
written in comments:

@schememod/eval[[#:file
"contracted-toy-factory-sig.ss"
scheme]

(define-signature contracted-toy-factory^
  ((contracted
    [build-toys (-> integer? (listof toy?))]
    [repaint    (-> toy? symbol? toy?)]
    [toy?       (-> any/c boolean?)]
    [toy-color  (-> toy? symbol?)])))

(provide contracted-toy-factory^)]

Now we take the previous implementation of @scheme[simple-factory@] and
implement this version of @scheme[toy-factory^] instead:

@schememod/eval[[#:file
"contracted-simple-factory-unit.ss"
scheme

(require "contracted-toy-factory-sig.ss")]

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
(eval:alts (require "contracted-simple-factory-unit.ss") (void))
(define-values/invoke-unit/infer contracted-simple-factory@)
(build-toys 3)
(build-toys #f)
(repaint 3 'blue)
]

@subsection{Adding Contracts to Units}

However, sometimes we may have a unit that must conform to an
already existing signature that is not contracted.  In this case,
we can create a unit contract with @scheme[unit/c] or use
the @scheme[define-unit/contract] form, which defines a unit which
has been wrapped with a unit contract.

For example, here's a version of @scheme[toy-factory@] which still
implements the regular @scheme[toy-factory^], but whose exports
have been protected with an appropriate unit contract.

@schememod/eval[[#:file
"wrapped-simple-factory-unit.ss"
scheme

(require "toy-factory-sig.ss")]

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

(provide contracted-simple-factory@)
]

@interaction[
#:eval toy-eval
(eval:alts (require "wrapped-simple-factory-unit.ss") (void))
(define-values/invoke-unit/infer wrapped-simple-factory@)
(build-toys 3)
(build-toys #f)
(repaint 3 'blue)
]


@; ----------------------------------------

@section{@scheme[unit] versus @scheme[module]}

As a form for modularity, @scheme[unit] complements @scheme[module]:

@itemize[

 @item{The @scheme[module] form is primarily for managing a universal
       namespace. For example, it allows a code fragment to refer
       specifically to the @scheme[car] operation from
       @schememodname[scheme/base]---the one that extracts the first
       element of an instance of the built-in pair datatype---as
       opposed to any number of other functions with the name
       @scheme[car]. In other word, the @scheme[module] construct lets
       you refer to @emph{the} binding that you want.}

 @item{The @scheme[unit] form is for parameterizing a code fragment
       with respect to most any kind of run-time value. For example,
       it allows a code fragement for work with a @scheme[car]
       function that accepts a single argument, where the specific
       function is determined later by linking the fragment to
       another. In other words, the @scheme[unit] construct lets you
       refer to @emph{a} binding that meets some specification.}

]

The @scheme[lambda] and @scheme[class] forms, among others, also allow
paremetrization of code with respect to values that are chosen
later. In principle, any of those could be implemented in terms of any
of the others. In practice, each form offers certain
conveniences---such as allowing overriding of methods or especially
simple application to values---that make them suitable for different
purposes.

The @scheme[module] form is more fundamental than the others, in a
sense. After all, a program fragment cannot reliably refer to
@scheme[lambda], @scheme[class], or @scheme[unit] form without the
namespace management provided by @scheme[module]. At the same time,
because namespace management is closely related to separate expansion
and compilation, @scheme[module] boundaries end up as
separate-compilation boundaries in a way that prohibits mutual
dependencies among fragments. For similar reasons, @scheme[module]
does not separate interface from implementation.

Use @scheme[unit] when @scheme[module] by itself almost works, but
when separately compiled pieces must refer to each other, or when you
want a stronger separation between @defterm{interface} (i.e., the
parts that need to be known at expansion and compilation time) and
@defterm{implementation} (i.e., the run-time parts). More generally,
use @scheme[unit] when you need to parameterize code over functions,
datatypes, and classes, and when the parameterized code itself
provides definitions to be linked with other parameterized code.

@; ----------------------------------------------------------------------

@close-eval[toy-eval]
