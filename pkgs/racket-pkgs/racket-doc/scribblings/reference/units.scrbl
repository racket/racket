#lang scribble/doc
@(require "mz.rkt" (for-label racket/unit-exptime))

@(define-syntax defkeywords
   (syntax-rules (*)
     [(_ [* (form ...) as see])
      (defform* [form ...]
        "Allowed only in a " (racket as) "; see " (racket see) ".")]
     [(_ [* (form ...) see-eg])
      (defform* [form ...]
        "Allowed only in certain forms; see, for example, " (racket see-eg) ".")]
     [(_ [form as see])
      (defkeywords [* (form) as see])]
     [(_ [form see-eg])
      (defkeywords [* (form) see-eg])]
     [(_ f ...)
      (begin (defkeywords f) ...)]))

@title[#:tag "mzlib:unit" #:style 'toc]{Units}

@guideintro["units"]{units}

@deftech{Units} organize a program into separately compilable and
reusable components. The imports and exports of a unit are grouped
into a @deftech{signature}, which can include ``static'' information
(such as macros) in addition to placeholders for run-time values.
Units with suitably matching signatures can be @deftech{linked}
together to form a larger unit, and a unit with no imports can be
@deftech{invoked} to execute its body.

@note-lib[racket/unit #:use-sources (racket/unit)]{ The
@racketmodname[racket/unit] module name can be used as a language name
with @racketfont{#lang}; see @secref["single-unit"].}

@local-table-of-contents[]

@; ------------------------------------------------------------------------

@section[#:tag "creatingunits"]{Creating Units}

@defform/subs[
#:literals (import export prefix rename only except tag init-depend tag)
(unit
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...)
  init-depends-decl
  unit-body-expr-or-defn
  ...)

([tagged-sig-spec
  sig-spec
  (tag id sig-spec)]

 [sig-spec
  sig-id
  (prefix id sig-spec)
  (rename sig-spec (id id) ...)
  (only sig-spec id ...)
  (except sig-spec id ...)]

 [init-depends-decl
  code:blank
  (init-depend tagged-sig-id ...)]

 [tagged-sig-id
  sig-id
  (tag id sig-id)])]{

Produces a unit that encapsulates its
@racket[unit-body-expr-or-defn]s. Expressions in the @racket[unit]
body can refer to identifiers bound by the @racket[sig-spec]s of the
@racket[import] clause, and the body must include one definition for
each identifier of a @racket[sig-spec] in the @racket[export] clause.
An identifier that is exported cannot be @racket[set!]ed in either the
defining unit or in importing units, although the implicit assignment
to initialize the variable may be visible as a mutation.

Each import or export @racket[sig-spec] ultimately refers to a
@racket[sig-id], which is an identifier that is bound to a signature
by @racket[define-signature].

In a specific import or export position, the set of identifiers bound
or required by a particular @racket[sig-id] can be adjusted in a few
ways:

@itemize[

 @item{@racket[(prefix id sig-spec)] as an import binds the same as
 @racket[sig-spec], except that each binding is prefixed with @racket[id].
 As an export, this form causes definitions using the @racket[id]
 prefix to satisfy the exports required by @racket[sig-spec].}
 
 @item{@racket[(rename sig-spec (id id) ...)] as an import binds the
 same as @racket[sig-spec], except that the first @racket[id] is used
 for the binding instead of the second @racket[id] (where
 @racket[sig-spec] by itself must imply a binding that is
 @racket[bound-identifier=?] to second @racket[id]).  As an export,
 this form causes a definition for the first @racket[id] to satisfy
 the export named by the second @racket[id] in @racket[sig-spec].}

 @item{@racket[(only sig-spec id ...)] as an import binds the same as
 @racket[sig-spec], but restricted to just the listed @racket[id]s
 (where @racket[sig-spec] by itself must imply a binding that is
 @racket[bound-identifier=?] to each @racket[id]).  This form is not
 allowed for an export.}

 @item{@racket[(except sig-spec id ...)] as an import binds the same
 as @racket[sig-spec], but excluding all listed @racket[id]s (where
 @racket[sig-spec] by itself must imply a binding that is
 @racket[bound-identifier=?] to each @racket[id]).  This form is not
 allowed for an export.}

]

As suggested by the grammar, these adjustments to a signature can be
nested arbitrarily.

A unit's declared imports are matched with actual supplied imports by
signature. That is, the order in which imports are supplied to a unit
when linking is irrelevant; all that matters is the signature
implemented by each supplied import. One actual import must be
provided for each declared import. Similarly, when a unit implements
multiple signatures, the order of the export signatures does not
matter.

To support multiple imports or exports for the same signature, an
import or export can be tagged using the form @racket[(tag
  id sig-spec)]. When an import declaration of a unit is
tagged, then one actual import must be given the same tag (with the
same signature) when the unit is linked. Similarly, when an export
declaration is tagged for a unit, then references to that particular
export must explicitly use the tag.

A unit is prohibited syntactically from importing two signatures that
are not distinct, unless they have different tags; two signatures are
@defterm{distinct} only if they share no ancestor through
@racket[extends]. The same syntactic constraint applies to exported
signatures. In addition, a unit is prohibited syntactically from
importing the same identifier twice (after renaming and other
transformations on a @racket[sig-spec]), exporting the same identifier
twice (again, after renaming), or exporting an identifier that is
imported.

When units are linked, the bodies of the linked units are
executed in an order that is specified at the linking site. An
optional @racket[(init-depend tagged-sig-id ...)]
declaration constrains the allowed orders of linking by specifying
that the current unit must be initialized after the unit that supplies
the corresponding import. Each @racket[tagged-sig-id] in an
@racket[init-depend] declaration must have a corresponding import in the
@racket[import] clause.}

@defform/subs[
#:literals (define-syntaxes define-values define-values-for-export
            open extends contracted struct)
(define-signature id extension-decl
  (sig-elem ...))

([extension-decl
  code:blank
  (code:line extends sig-id)]

 [sig-elem
  id
  (define-syntaxes (id ...) expr)
  (define-values (id ...) expr) 
  (define-values-for-export (id ...) expr) 
  (contracted [id contract] ...)
  (open sig-spec) 
  (struct id (field ...) struct-option ...) 
  (sig-form-id . datum)]

 [field id
        [id #:mutable]]
 [struct-option #:mutable
                (code:line #:constructor-name constructor-id)
                (code:line #:extra-constructor-name constructor-id)
                #:omit-constructor
                #:omit-define-syntaxes
                #:omit-define-values])]{

Binds an identifier to a signature that specifies a group
of bindings for import or export:

@itemize[

 @item{Each @racket[id] in a signature declaration means that a unit
 implementing the signature must supply a variable definition for the
 @racket[id]. That is, @racket[id] is available for use in units
 importing the signature, and @racket[id] must be defined by units
 exporting the signature.}

 @item{Each @racket[define-syntaxes] form in a signature declaration
 introduces a macro that is available for use in any unit that
 imports the signature.  Free variables in the definition's
 @racket[expr] refer to other identifiers in the signature first, or
 the context of the @racket[define-signature] form if the signature
 does not include the identifier.}

 @item{Each @racket[define-values] form in a signature declaration
 introduces code that effectively prefixes every unit that imports the
 signature.  Free variables in the definition's @racket[expr] are
 treated the same as for @racket[define-syntaxes].}

 @item{Each @racket[define-values-for-export] form in a signature
 declaration introduces code that effectively suffixes every unit that
 exports the signature.  Free variables in the definition's
 @racket[expr] are treated the same as for @racket[define-syntaxes].}

 @item{Each @racket[contracted] form in a signature declaration means
 that a unit exporting the signature must supply a variable definition
 for each @racket[id] in that form.  If the signature is imported, then
 uses of @racket[id] inside the unit are protected by the appropriate
 contracts using the unit as the negative blame.  If the signature is
 exported, then the exported values are protected by the appropriate
 contracts which use the unit as the positive blame, but internal uses
 of the exported identifiers are not protected.  Variables in the
 @racket[contract] expressions are treated the same as for
 @racket[define-syntaxes].}

 @item{Each @racket[(open sig-spec)] adds to the signature everything
 specified by @racket[sig-spec].}

 @item{Each @racket[(struct id (field ...) struct-option ...)]  adds
 all of the identifiers that would be bound by @racket[(struct id
 (field ...) field-option ...)], where the extra option
 @racket[#:omit-constructor] omits the constructor identifier.}

 @item{Each @racket[(sig-form-id . datum)] extends the signature in a
 way that is defined by @racket[sig-form-id], which must be bound by
 @racket[define-signature-form].  One such binding is for
 @racket[struct/ctc].}

]

When a @racket[define-signature] form includes an @racket[extends]
clause, then the define signature automatically includes everything in
the extended signature. Furthermore, any implementation of the new
signature can be used as an implementation of the extended signature.}

@defkeywords[[(open sig-spec) _sig-elem define-signature]
             [(define-values-for-export (id ...) expr) _sig-elem define-signature]
             [(contracted [id contract] ...) _sig-elem define-signature]
             [(only sig-spec id ...) _sig-spec unit]
             [(except sig-spec id ...) _sig-spec unit]
             [(rename sig-spec (id id) ...) _sig-spec unit]
             [(prefix id sig-spec) _sig-spec unit]
             [(import tagged-sig-spec ...) unit]
             [(export tagged-sig-spec ...) unit]
             [(link linkage-decl ...) compound-unit]
             [* [(tag id sig-spec)
                 (tag id sig-id)] unit]
             [(init-depend tagged-sig-id ...) init-depend-decl unit]]

@defidform[extends]{

Allowed only within @racket[define-signature].}

@; ------------------------------------------------------------------------

@section[#:tag "invokingunits"]{Invoking Units}

@defform*[#:literals (import)
          [(invoke-unit unit-expr)
           (invoke-unit unit-expr (import tagged-sig-spec ...))]]{

Invokes the unit produced by @racket[unit-expr]. For each of the
unit's imports, the @racket[invoke-unit] expression must contain a
@racket[tagged-sig-spec] in the @racket[import] clause; see
@racket[unit] for the grammar of @racket[tagged-sig-spec]. If the unit
has no imports, the @racket[import] clause can be omitted.

When no @racket[tagged-sig-spec]s are provided, @racket[unit-expr]
must produce a unit that expects no imports. To invoke the unit, all
bindings are first initialized to the @|undefined-const| value. Next,
the unit's body definitions and expressions are evaluated in order; in
the case of a definition, evaluation sets the value of the
corresponding variable(s). Finally, the result of the last expression
in the unit is the result of the @racket[invoke-unit] expression.

Each supplied @racket[tagged-sig-spec] takes bindings from the
surrounding context and turns them into imports for the invoked unit.
The unit need not declare an import for every provided
@racket[tagged-sig-spec], but one @racket[tagged-sig-spec] must be
provided for each declared import of the unit. For each variable
identifier in each provided @racket[tagged-sig-spec], the value of the
identifier's binding in the surrounding context is used for the
corresponding import in the invoked unit.}

@defform[
#:literals (import export)
(define-values/invoke-unit unit-expr 
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...))]{

Like @racket[invoke-unit], but the values of the unit's exports are
copied to new bindings.

The unit produced by @racket[unit-expr] is linked and invoked as for
@racket[invoke-unit]. In addition, the @racket[export] clause is
treated as a kind of import into the local definition context. That
is, for every binding that would be available in a unit that used the
@racket[export] clause's @racket[tagged-sig-spec] as an import, a
definition is generated for the context of the
@racket[define-values/invoke-unit] form.}

@; ------------------------------------------------------------------------

@section[#:tag "compoundunits"]{Linking Units and Creating Compound Units}

@defform/subs[
#:literals (: import export link tag)
(compound-unit
  (import link-binding ...)
  (export tagged-link-id ...)
  (link linkage-decl ...))

([link-binding
  (link-id : tagged-sig-id)]

 [tagged-link-id
  (tag id link-id)
  link-id]

 [linkage-decl
  ((link-binding ...) unit-expr tagged-link-id ...)])]{

Links several units into one new compound unit without immediately
invoking any of the linked units.  The @racket[unit-expr]s in the
@racket[link] clause determine the units to be linked in creating the
compound unit. The @racket[unit-expr]s are evaluated when the
@racket[compound-unit] form is evaluated.

The @racket[import] clause determines the imports of the compound
unit. Outside the compound unit, these imports behave as for a plain
unit; inside the compound unit, they are propagated to some of the
linked units. The @racket[export] clause determines the exports of the
compound unit.  Again, outside the compound unit, these exports are
treated the same as for a plain unit; inside the compound unit, they
are drawn from the exports of the linked units. Finally, the left-hand
and right-hand parts of each declaration in the @racket[link] clause
specify how the compound unit's imports and exports are propagated to
the linked units.

Individual elements of an imported or exported signature are not
available within the compound unit. Instead, imports and exports are
connected at the level of whole signatures. Each specific import or
export (i.e., an instance of some signature, possibly tagged) is given
a @racket[link-id] name. Specifically, a @racket[link-id] is bound by
the @racket[import] clause or the left-hand part of a declaration in
the @racket[link] clause. A bound @racket[link-id] is referenced in
the right-hand part of a declaration in the @racket[link] clause or by
the @racket[export] clause.

The left-hand side of a @racket[link] declaration gives names to each
expected export of the unit produced by the corresponding
@racket[unit-expr]. The actual unit may export additional signatures,
and it may export an extension of a specific signature instead of just
the specified one. If the unit does not export one of the specified
signatures (with the specified tag, if any), the
@exnraise[exn:fail:contract] when the @racket[compound-unit] form is
evaluated.

The right-hand side of a @racket[link] declaration specifies the
imports to be supplied to the unit produced by the corresponding
@racket[unit-expr]. The actual unit may import fewer signatures, and
it may import a signature that is extended by the specified one.  If
the unit imports a signature (with a particular tag) that is not
included in the supplied imports, the @exnraise[exn:fail:contract]
when the @racket[compound-unit] form is evaluated. Each
@racket[link-id] supplied as an import must be bound either in the
@racket[import] clause or in some declaration within the @racket[link]
clause.

The order of declarations in the @racket[link] clause determines the
order of invocation of the linked units. When the compound unit is
invoked, the unit produced by the first @racket[unit-expr] is invoked
first, then the second, and so on. If the order specified in the
@racket[link] clause is inconsistent with @racket[init-depend]
declarations of the actual units, then the
@exnraise[exn:fail:contract] when the @racket[compound-unit] form is
evaluated.}

@; ------------------------------------------------------------------------

@section[#:tag "linkinference"]{Inferred Linking}

@defform[
#:literals (import export)
(define-unit unit-id
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...)
  init-depends-decl
  unit-body-expr-or-defn
  ...)
]{

Binds @racket[unit-id] to both a unit and static information about the
unit.

Evaluating a reference to a @racket[unit-id] bound by
@racket[define-unit] produces a unit, just like evaluating an
@racket[id] bound by @racket[(define id (unit ...))]. In addition,
however, @racket[unit-id] can be used in @racket[compound-unit/infer].
See @racket[unit] for information on @racket[tagged-sig-spec], 
@racket[init-depends-decl], and @racket[unit-body-expr-or-defn].}

@defform/subs[
#:literals (import export link tag :)
(compound-unit/infer
  (import tagged-infer-link-import ...)
  (export tagged-infer-link-export ...)
  (link infer-linkage-decl ...))

([tagged-infer-link-import
  tagged-sig-id
  (link-id : tagged-sig-id)]

 [tagged-infer-link-export
  (tag id infer-link-export)
  infer-link-export]

 [infer-link-export
  link-id
  sig-id]

 [infer-linkage-decl
  ((link-binding ...) unit-id 
                      tagged-link-id ...)
  unit-id])]{

Like @racket[compound-unit]. Syntactically, the difference between
@racket[compound-unit] and @racket[compound-unit/infer] is that the
@racket[unit-expr] for a linked unit is replaced with a
@racket[unit-id], where a @racket[unit-id] is bound by
@racket[define-unit] (or one of the other unit-binding forms that we
introduce later in this section). Furthermore, an import can name just
a @racket[sig-id] without locally binding a @racket[link-id], and an
export can be based on a @racket[sig-id] instead of a
@racket[link-id], and a declaration in the @racket[link] clause can be
simply a @racket[unit-id] with no specified exports or imports.

The @racket[compound-unit/infer] form expands to
@racket[compound-unit] by adding @racket[sig-ids] as needed to
the @racket[import] clause, by replacing @racket[sig-id]s in the
@racket[export] clause by @racket[link-id]s, and by completing
the declarations of the @racket[link] clause. This completion is based
on static information associated with each
@racket[unit-id]. Links and exports can be inferred when all
signatures exported by the linked units are distinct from each other
and from all imported signatures, and when all imported signatures are
distinct. Two signatures are @defterm{distinct} only if they
share no ancestor through @racket[extends].

The long form of a @racket[link] declaration can be used to resolve
ambiguity by giving names to some of a unit's exports and supplying
specific bindings for some of a unit's imports. The long form need not
name all of a unit's exports or supply all of a unit's imports if the
remaining parts can be inferred.

Like @racket[compound-unit], the @racket[compound-unit/infer] form
produces a (compound) unit without statically binding information
about the result unit's imports and exports. That is,
@racket[compound-unit/infer] consumes static information, but it does
not generate it. Two additional forms,
@racket[define-compound-unit] and
@racket[define-compound-unit/infer], generate static information
(where the former does not consume static information).}

@defform[
#:literals (import export link)
(define-compound-unit id
  (import link-binding ...)
  (export tagged-link-id ...)
  (link linkage-decl ...))
]{

Like @racket[compound-unit], but binds static information about the
compound unit like @racket[define-unit].}


@defform[
#:literals (import export link)
(define-compound-unit/infer id
  (import link-binding ...)
  (export tagged-infer-link-export ...)
  (link infer-linkage-decl ...))
]{

Like @racket[compound-unit/infer], but binds static information about
the compound unit like @racket[define-unit].}

@defform[
#:literals (import export)
(define-unit-binding unit-id
  unit-expr
  (import tagged-sig-spec ...+)
  (export tagged-sig-spec ...+)
  init-depends-decl)
]{

Like @racket[define-unit], but the unit implementation is determined
from an existing unit produced by @racket[unit-expr]. The imports and
exports of the unit produced by @racket[unit-expr] must be consistent
with the declared imports and exports, otherwise the
@exnraise[exn:fail:contract] when the @racket[define-unit-binding]
form is evaluated.}

@defform/subs[
#:literals (link)
(invoke-unit/infer unit-spec)
[(unit-spec unit-id (link link-unit-id ...))]]{

Like @racket[invoke-unit], but uses static information associated with
@racket[unit-id] to infer which imports must be assembled from the
current context.  If given a link form containing multiple
@racket[link-unit-id]s, then the units are first linked via
@racket[define-compound-unit/infer].}

@defform/subs[
#:literals (export link)
(define-values/invoke-unit/infer maybe-exports unit-spec)
[(maybe-exports code:blank (export tagged-sig-spec ...))
 (unit-spec unit-id (link link-unit-id ...))]]{

Like @racket[define-values/invoke-unit], but uses static information
associated with @racket[unit-id] to infer which imports must be
assembled from the current context and which exports should be bound
by the definition.  If given a link form containing multiple
@racket[link-unit-id]s, then the units are first linked via
@racket[define-compound-unit/infer].}

@; ------------------------------------------------------------------------

@section{Generating A Unit from Context}

@defform[
(unit-from-context tagged-sig-spec)
]{

Creates a unit that implements an interface using bindings in the
enclosing environment.  The generated unit is essentially the same as

@racketblock[
(unit
  (import)
  (export tagged-sig-spec)
  (define id expr) ...)
]

for each @racket[id] that must be defined to satisfy the exports, and
each corresponding @racket[expr] produces the value of @racket[id] in
the environment of the @racket[unit-from-context] expression. (The unit
cannot be written as above, however, since each @racket[id] definition
within the unit shadows the binding outside the @racket[unit] form.)

See @racket[unit] for the grammar of @racket[tagged-sig-spec].}

@defform[
(define-unit-from-context id tagged-sig-spec)
]{

Like @racket[unit-from-context], in that a unit is constructed from
the enclosing environment, and like @racket[define-unit], in that 
@racket[id] is bound to static information to be used later with inference.}

@; ------------------------------------------------------------------------

@section{Structural Matching}

@defform[
#:literals (import export)
(unit/new-import-export
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...)
  init-depends-decl 
  ((tagged-sig-spec ...) unit-expr tagged-sig-spec))
]{

Similar to @racket[unit], except the body of the unit is determined by
an existing unit produced by @racket[unit-expr]. The result is a unit
whose implementation is @racket[unit-expr], but whose imports,
exports, and initialization dependencies are as in the
@racket[unit/new-import-export] form (instead of as in the unit
produced by @racket[unit-expr]).

The final clause of the @racket[unit/new-import-export] form
determines the connection between the old and new imports and exports.
The connection is similar to the way that @racket[compound-unit]
propagates imports and exports; the difference is that the connection
between @racket[import] and the right-hand side of the link clause is
based on the names of elements in signatures, rather than the names of
the signatures. That is, a @racket[tagged-sig-spec] on the right-hand
side of the link clause need not appear as a @racket[tagged-sig-spec]
in the @racket[import] clause, but each of the bindings implied by the
linking @racket[tagged-sig-spec] must be implied by some
@racket[tagged-sig-spec] in the @racket[import] clause. Similarly,
each of the bindings implied by an @racket[export]
@racket[tagged-sig-spec] must be implied by some left-hand-side
@racket[tagged-sig-spec] in the linking clause.}

@defform[
#:literals (import export)
(define-unit/new-import-export unit-id
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...)
  init-depends-decl 
  ((tagged-sig-spec ...) unit-expr tagged-sig-spec))
]{

Like @racket[unit/new-import-export], but binds static information to
@racket[unit-id] like @racket[define-unit].}

@defform[
#:literals (import export)
(unit/s
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...)
  init-depends-decl
  unit-id)]{

Like @racket[unit/new-import-export], but the linking clause is
inferred, so @racket[unit-id] must have the appropriate static
information.}
@defform[
#:literals (import export)
(define-unit/s name-id
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...)
  init-depends-decl
  unit-id)]{

Like @racket[unit/s], but binds static information to @racket[name-id]
like @racket[define-unit].}

@; ------------------------------------------------------------------------

@section[#:tag "define-sig-form"]{Extending the Syntax of Signatures}

@defform*[
[(define-signature-form sig-form-id expr)
 (define-signature-form (sig-form-id id) body ...+)]
]{

Binds @racket[sig-form-id] for use within a @racket[define-signature]
form.

In the first form, the result of @racket[expr] must be a transformer
procedure.  In the second form, @racket[sig-form-id] is bound to a
transformer procedure whose argument is @racket[id] and whose body is
the @racket[body]s. The result of the transformer must be a list of
syntax objects, which are substituted for a use of
@racket[sig-form-id] in a @racket[define-signature] expansion. (The
result is a list so that the transformer can produce multiple
declarations; @racket[define-signature] has no splicing @racket[begin]
form.)}

@defform/subs[
(struct/ctc id ([field contract-expr] ...) struct-option ...) 

([field id
        [id #:mutable]]
 [struct-option #:mutable
                #:omit-constructor
                #:omit-define-syntaxes
                #:omit-define-values])]{

For use with @racket[define-signature]. The @racket[struct/ctc] form works
similarly to @racket[struct], but the constructor, predicate, field
accessors, and field mutators are contracted appropriately.}

@; ------------------------------------------------------------------------

@section{Unit Utilities}

@defproc[(unit? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a unit, @racket[#f] otherwise.}


@defform[(provide-signature-elements sig-spec ...)]{

Expands to a @racket[provide] of all identifiers implied by the
@racket[sig-spec]s. See @racket[unit] for the grammar of
@racket[sig-spec].}

@; ------------------------------------------------------------------------

@section[#:tag "unitcontracts"]{Unit Contracts}

@defform/subs[#:literals (import export)
              (unit/c (import sig-block ...) (export sig-block ...))
              ([sig-block (tagged-sig-id [id contract] ...)
                          tagged-sig-id])]{

A @deftech{unit contract} wraps a unit and checks both its imported and
exported identifiers to ensure that they match the appropriate contracts.
This allows the programmer to add contract checks to a single unit value
without adding contracts to the imported and exported signatures.

The unit value must import a subset of the import signatures and export a
superset of the export signatures listed in the unit contract.  Any
identifier which is not listed for a given signature is left alone.
Variables used in a given @racket[contract] expression first refer to other
variables in the same signature, and then to the context of the 
@racket[unit/c] expression.}

@defform/subs[#:literals (import export)
              (define-unit/contract unit-id
                (import sig-spec-block ...)
                (export sig-spec-block ...)
                init-depends-decl
                unit-body-expr-or-defn
                ...)
              ([sig-spec-block (tagged-sig-spec [id contract] ...)
                               tagged-sig-spec])]{
The @racket[define-unit/contract] form defines a unit compatible with
link inference whose imports and exports are contracted with a unit
contract.  The unit name is used for the positive blame of the contract.}


@; ------------------------------------------------------------------------

@section[#:tag "single-unit"]{Single-Unit Modules}

When @racketmodname[racket/unit] is used as a language name with
@racketfont{#lang}, the module body is treated as a unit body.  The
body must match the following @racket[_module-body] grammar:

@racketgrammar*[
#:literals (import export require begin)
[module-body (code:line
              require-decl ...
              (import tagged-sig-expr ...)
              (export tagged-sig-expr ...)
              init-depends-decl
              unit-body-expr-or-defn
              ...)]
[require-decl (require require-spec ...)
              (begin require-decl ...)
              derived-require-form]]

After any number of @racket[_require-decl]s, the content of the module
is the same as a @racket[unit] body.

The resulting unit is exported as @racket[_base]@racketidfont["@"],
where @racket[_base] is derived from the enclosing module's name
(i.e., its symbolic name, or its path without the directory and file
suffix). If the module name ends in @racketidfont{-unit}, then
@racket[_base] corresponds to the module name before
@racketidfont{-unit}. Otherwise, the module name serves as
@racket[_base].

@; ------------------------------------------------------------------------

@section{Single-Signature Modules}

@defmodulelang[racket/signature]{The @racketmodname[racket/signature]
language treats a module body as a unit signature.}

The body must match the following @racket[_module-body] grammar:

@racketgrammar*[
#:literals (require)
[module-body (code:line (require require-spec ...) ... sig-spec ...)]
]

See @secref["creatingunits"] for the grammar of @racket[_sig-spec].
Unlike the body of a @racketmodname[racket/unit] module, a
@racket[require] in a @racketmodname[racket/signature] module must be
a literal use of @racket[require].


The resulting signature is exported as
@racket[_base]@racketidfont["^"], where @racket[_base] is derived from
the enclosing module's name (i.e., its symbolic name, or its path
without the directory and file suffix). If the module name ends in
@racketidfont{-sig}, then @racket[_base] corresponds to the module
name before @racketidfont{-sig}. Otherwise, the module name serves as
@racket[_base].

@; ----------------------------------------------------------------------

@section{Transformer Helpers}

@defmodule[racket/unit-exptime #:use-sources (racket/unit-exptime)]

The @racketmodname[racket/unit-exptime] library provides procedures
that are intended for use by macro transformers. In particular, the
library is typically imported using @racket[for-syntax] into a module
that defines macro with @racket[define-syntax].

@defproc[(unit-static-signatures [unit-identifier identifier?]
                                 [err-syntax syntax?])
         (values (list/c (cons/c (or/c symbol? #f)
                                 identifier?))
                 (list/c (cons/c (or/c symbol? #f)
                                 identifier?)))]{

If @racket[unit-identifier] is bound to static unit information via
@racket[define-unit] (or other such forms), the result is two
values. The first value is for the unit's imports, and the second is
for the unit's exports. Each result value is a list, where each list
element pairs a symbol or @racket[#f] with an identifier. The symbol
or @racket[#f] indicates the import's or export's tag (where
@racket[#f] indicates no tag), and the identifier indicates the
binding of the corresponding signature.

If @racket[unit-identifier] is not bound to static unit information,
then the @exnraise[exn:fail:syntax]. In that case, the given
@racket[err-syntax] argument is used as the source of the error, where
@racket[unit-identifier] is used as the detail source location.}


@defproc[(signature-members [sig-identifier identifier?]
                            [err-syntax syntax?])
         (values (or/c identifier? #f)
                 (listof identifier?)
                 (listof identifier?)
                 (listof identifier?))]{

If @racket[sig-identifier] is bound to static unit information via
@racket[define-signature] (or other such forms), the result is four
values:

@itemize[

  @item{an identifier or @racket[#f] indicating the signature (of any)
        that is extended by the @racket[sig-identifier] binding;}

  @item{a list of identifiers representing the variables
        supplied/required by the signature;}

  @item{a list of identifiers for variable definitions in the
        signature (i.e., variable bindings that are provided on
        import, but not defined by units that implement the
        signature); and}

  @item{a list of identifiers with syntax definitions in the signature.}

]

If @racket[sig-identifier] is not bound to a signature, then the
@exnraise[exn:fail:syntax]. In that case, the given
@racket[err-syntax] argument is used as the source of the error, where
@racket[sig-identifier] is used as the detail source location.}
