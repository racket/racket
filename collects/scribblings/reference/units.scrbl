#lang scribble/doc
@(require "mz.ss"
          (for-label racket/unit-exptime))

@(define-syntax defkeywords
   (syntax-rules (*)
     [(_ [* (form ...) as see])
      (defform* [form ...]
        "Allowed only in a " (scheme as) "; see " (scheme see) ".")]
     [(_ [* (form ...) see-eg])
      (defform* [form ...]
        "Allowed only in certain forms; see, for example, " (scheme see-eg) ".")]
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

@note-lib[racket/unit #:use-sources (mzlib/unit)]{ The
@schememodname[racket/unit] module name can be used as a language name
with @schemefont{#lang}; see @secref["single-unit"].}

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
@scheme[unit-body-expr-or-defn]s. Expressions in the @scheme[unit]
body can refer to identifiers bound by the @scheme[sig-spec]s of the
@scheme[import] clause, and the body must include one definition for
each identifier of a @scheme[sig-spec] in the @scheme[export] clause.
An identifier that is exported cannot be @scheme[set!]ed in either the
defining unit or in importing units, although the implicit assignment
to initialize the variable may be visible as a mutation.

Each import or export @scheme[sig-spec] ultimately refers to a
@scheme[sig-id], which is an identifier that is bound to a signature
by @scheme[define-signature].

In a specific import or export position, the set of identifiers bound
or required by a particular @scheme[sig-id] can be adjusted in a few
ways:

@itemize[

 @item{@scheme[(prefix id sig-spec)] as an import binds the same as
 @scheme[sig-spec], except that each binding is prefixed with @scheme[id].
 As an export, this form causes definitions using the @scheme[id]
 prefix to satisfy the exports required by @scheme[sig-spec].}
 
 @item{@scheme[(rename sig-spec (id id) ...)] as an import binds the
 same as @scheme[sig-spec], except that the first @scheme[id] is used
 for the binding instead of the second @scheme[id] (where
 @scheme[sig-spec] by itself must imply a bindingthat is
 @scheme[bound-identifier=?] to second @scheme[id]).  As an export,
 this form causes a definition for the first @scheme[id] to satisfy
 the export named by the second @scheme[id] in @scheme[sig-spec].}

 @item{@scheme[(only sig-spec id ...)] as an import binds the same as
 @scheme[sig-spec], but restricted to just the listed @scheme[id]s
 (where @scheme[sig-spec] by itself must imply a binding that is
 @scheme[bound-identifier=?] to each @scheme[id]).  This form is not
 allowed for an export.}

 @item{@scheme[(except sig-spec id ...)] as an import binds the same
 as @scheme[sig-spec], but excluding all listed @scheme[id]s (where
 @scheme[sig-spec] by itself must imply a binding that is
 @scheme[bound-identifier=?] to each @scheme[id]).  This form is not
 allowed for an export.}

]

As suggested by the grammar, these adjustments to a signature can be
nested arbitrarily.

A unit's declared imports are matched with actual supplied imports by
signature. That is, the order in which imports are suppplied to a unit
when linking is irrelevant; all that matters is the signature
implemented by each supplied import. One actual import must be
provided for each declared import. Similarly, when a unit implements
multiple signatures, the order of the export signatures does not
matter.

To support multiple imports or exports for the same signature, an
import or export can be tagged using the form @scheme[(tag
  id sig-spec)]. When an import declaration of a unit is
tagged, then one actual import must be given the same tag (with the
same signature) when the unit is linked. Similarly, when an export
declaration is tagged for a unit, then references to that particular
export must explicitly use the tag.

A unit is prohibited syntactically from importing two signatures that
are not distinct, unless they have different tags; two signatures are
@defterm{distinct} only if when they share no ancestor through
@scheme[extends]. The same syntactic constraint applies to exported
signatures. In addition, a unit is prohibited syntactically from
importing the same identifier twice (after renaming and other
transformations on a @scheme[sig-spec]), exporting the same identifier
twice (again, after renaming), or exporting an identifier that is
imported.

When units are linked, the bodies of the linked units are
executed in an order that is specified at the linking site. An
optional @scheme[(init-depend tagged-sig-id ...)]
declaration constrains the allowed orders of linking by specifying
that the current unit must be initialized after the unit that supplies
the corresponding import. Each @scheme[tagged-sig-id] in an
@scheme[init-depend] declaration must have a corresponding import in the
@scheme[import] clause.}

@defform/subs[
#:literals (define-syntaxes define-values define-values-for-export open extends contracted)
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

 @item{Each @scheme[id] in a signature declaration means that a unit
 implementing the signature must supply a variable definition for the
 @scheme[id]. That is, @scheme[id] is available for use in units
 importing the signature, and @scheme[id] must be defined by units
 exporting the signature.}
      
 @item{Each @scheme[define-syntaxes] form in a signature declaration
 introduces a macro to that is available for use in any unit that
 imports the signature.  Free variables in the definition's
 @scheme[expr] refer to other identifiers in the signature first, or
 the context of the @scheme[define-signature] form if the signature
 does not include the identifier.}

 @item{Each @scheme[define-values] form in a signature declaration
 introduces code that effectively prefixes every unit that imports the
 signature.  Free variables in the definition's @scheme[expr] are
 treated the same as for @scheme[define-syntaxes].}

 @item{Each @scheme[define-values-for-export] form in a signature
 declaration introduces code that effectively suffixes every unit that
 exports the signature.  Free variables in the definition's
 @scheme[expr] are treated the same as for @scheme[define-syntaxes].}

 @item{Each @scheme[contracted] form in a signature declaration means
 that a unit exporting the signature must supply a variable definition
 for each @scheme[id] in that form.  If the signature is imported, then
 uses of @scheme[id] inside the unit are protected by the appropriate
 contracts using the unit as the negative blame.  If the signature is
 exported, then the exported values are protected by the appropriate
 contracts which use the unit as the positive blame, but internal uses
 of the exported identifiers are not protected.  Variables in the
 @scheme[contract] expressions are treated the same as for
 @scheme[define-syntaxes].}

 @item{Each @scheme[(open sig-spec)] adds to the signature everything
 specified by @scheme[sig-spec].}

 @item{Each @scheme[(struct id (field ...) struct-option ...)]  adds
 all of the identifiers that would be bound by @scheme[(struct id
 (field ...) field-option ...)], where the extra option
 @scheme[#:omit-constructor] omits the constructor identifier.}

 @item{Each @scheme[(sig-form-id . datum)] extends the signature in a
 way that is defined by @scheme[sig-form-id], which must be bound by
 @scheme[define-signature-form].  One such binding is for
 @scheme[struct/ctc].}

]

When a @scheme[define-signature] form includes a @scheme[extends]
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

Allowed only within @scheme[define-signature].}

@; ------------------------------------------------------------------------

@section[#:tag "invokingunits"]{Invoking Units}

@defform*[#:literals (import)
          [(invoke-unit unit-expr)
           (invoke-unit unit-expr (import tagged-sig-spec ...))]]{

Invokes the unit produced by @scheme[unit-expr]. For each of the
unit's imports, the @scheme[invoke-unit] expression must contain a
@scheme[tagged-sig-spec] in the @scheme[import] clause; see
@scheme[unit] for the grammar of @scheme[tagged-sig-spec]. If the unit
has no imports, the @scheme[import] clause can be omitted.

When no @scheme[tagged-sig-spec]s are provided, @scheme[unit-expr]
must produce a unit that expect no imports. To invoke the unit, all
bindings are first initialized to the @|undefined-const| value. Next,
the unit's body definitions and expressions are evaluated in order; in
the case of a definition, evaluation sets the value of the
corresponding variable(s). Finally, the result of the last expression
in the unit is the result of the @scheme[invoke-unit] expression.

Each supplied @scheme[tagged-sig-spec] takes bindings from the
surrounding context and turns them into imports for the invoked unit.
The unit need not declare an import for every provided
@scheme[tagged-sig-spec], but one @scheme[tagged-sig-spec] must be
provided for each declared import of the unit. For each variable
identifier in each provided @scheme[tagged-sig-spec], the value of the
identifier's binding in the surrounding context is used for the
corresponding import in the invoked unit.}

@defform[
#:literals (import export)
(define-values/invoke-unit unit-expr 
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...))]{

Like @scheme[invoke-unit], but the values of the unit's exports are
copied to new bindings.

The unit produced by @scheme[unit-expr] is linked and invoked as for
@scheme[invoke-unit]. In addition, the @scheme[export] clause is
treated as a kind of import into the local definition context. That
is, for every binding that would be available in a unit that used the
@scheme[export] clauses's @scheme[tagged-sig-spec] as an import, a
definition is generated for the context of the
@scheme[define-values/invoke-unit] form.}

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
invoking any of the linked units.  The @scheme[unit-expr]s in the
@scheme[link] clause determine the units to be linked in creating the
compound unit. The @scheme[unit-expr]s are evaluated when the
@scheme[compound-unit] form is evaluated.

The @scheme[import] clause determines the imports of the compound
unit. Outside the compound unit, these imports behave as for a plain
unit; inside the compound unit, they are propagated to some of the
linked units. The @scheme[export] clause determines the exports of the
compound unit.  Again, outside the compound unit, these exports are
trested the same as for a plain unit; inside the compound unit, they
are drawn from the exports of the linked units. Finally, the left-hand
and right-hand parts of each declaration in the @scheme[link] clause
specify how the compound unit's imports and exports are propagated to
the linked units.

Individual elements of an imported or exported signature are not
available within the compound unit. Instead, imports and exports are
connected at the level of whole signatures. Each specific import or
export (i.e., an instance of some signature, possibly tagged) is given
a @scheme[link-id] name. Specifically, a @scheme[link-id] is bound by
the @scheme[import] clause or the left-hand part of an declaration in
the @scheme[link] clause. A bound @scheme[link-id] is referenced in
the right-hand part of a declaration in the @scheme[link] clause or by
the @scheme[export] clause.

The left-hand side of a @scheme[link] declaration gives names to each
expected export of the unit produced by the corresponding
@scheme[unit-expr]. The actual unit may export additional signatures,
and it may export an extension of a specific signature instead of just
the specified one. If the unit does not export one of the specified
signatures (with the specified tag, if any), the
@exnraise[exn:fail:contract] when the @scheme[compound-unit] form is
evaluated.

The right-hand side of a @scheme[link] declaration specifies the
imports to be supplied to the unit produced by the corresponding
@scheme[unit-expr]. The actual unit may import fewer signatures, and
it may import a signature that is extended by the specified one.  If
the unit imports a signature (with a particular tag) that is not
included in the supplied imports, the @exnraise[exn:fail:contract]
when the @scheme[compound-unit] form is evaluated. Each
@scheme[link-id] supplied as an import must be bound either in the
@scheme[import] clause or in some declaration within the @scheme[link]
clause.

The order of declarations in the @scheme[link] clause determines the
order of invocation of the linked units. When the compound unit is
invoked, the unit produced by the first @scheme[unit-expr] is invoked
first, then the second, and so on. If the order specified in the
@scheme[link] clause is inconsistent with @scheme[init-depend]
declarations of the actual units, then the
@exnraise[exn:fail:contract] when the @scheme[compound-unit] form is
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

Binds @scheme[unit-id] to both a unit and static information about the
unit.

Evaluating a reference to an @scheme[unit-id] bound by
@scheme[define-unit] produces a unit, just like evaluating an
@scheme[id] bound by @scheme[(define id (unit ...))]. In addition,
however, @scheme[unit-id] can be used in @scheme[compound-unit/infer].
See @scheme[unit] for information on @scheme[tagged-sig-spec], 
@scheme[init-depends-decl], and @scheme[unit-body-expr-or-defn].}

@defform/subs[
#:literals (import export :)
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

Like @scheme[compound-unit]. Syntactically, the difference between
@scheme[compound-unit] and @scheme[compound-unit/infer] is that the
@scheme[unit-expr] for a linked unit is replaced with a
@scheme[unit-id], where a @scheme[unit-id] is bound by
@scheme[define-unit] (or one of the other unit-binding forms that we
introduce later in this section). Furthermore, an import can name just
a @scheme[sig-id] without locally binding a @scheme[link-id], and an
export can be based on a @scheme[sig-id] instead of a
@scheme[link-id], and a declaration in the @scheme[link] clause can be
simply a @scheme[unit-id] with no specified exports or imports.

The @scheme[compound-unit/infer] form expands to
@scheme[compound-unit] by adding @scheme[sig-ids] as needed to
the @scheme[import] clause, by replacing @scheme[sig-id]s in the
@scheme[export] clause by @scheme[link-id]s, and by completing
the declarations of the @scheme[link] clause. This completion is based
on static information associated with each
@scheme[unit-id]. Links and exports can be inferred when all
signatures exported by the linked units are distinct from each other
and from all imported signatures, and when all imported signatures are
distinct. Two signatures are @defterm{distinct} only if when they
share no ancestor through @scheme[extends].

The long form of a @scheme[link] declaration can be used to resolve
ambiguity by giving names to some of a unit's exports and supplying
specific bindings for some of a unit's imports. The long form need not
name all of a unit's exports or supply all of a unit's imports if the
remaining parts can be inferred.

Like @scheme[compound-unit], the @scheme[compound-unit/infer] form
produces a (compound) unit without statically binding information
about the result unit's imports and exports. That is,
@scheme[compound-unit/infer] consumes static information, but it does
not generate it. Two additional forms,
@scheme[define-compound-unit] and
@scheme[define-compound-unit/infer], generate static information
(where the former does not consume static information).}

@defform[
#:literals (import export link)
(define-compound-unit id
  (import link-binding ...)
  (export tagged-link-id ...)
  (link linkage-decl ...))
]{

Like @scheme[compound-unit], but binds static information about the
compound unit like @scheme[define-unit].}


@defform[
#:literals (import export link)
(define-compound-unit/infer id
  (import link-binding ...)
  (export tagged-infer-link-export ...)
  (link infer-linkage-decl ...))
]{

Like @scheme[compound-unit/infer], but binds static information about
the compound unit like @scheme[define-unit].}

@defform[
#:literals (import export)
(define-unit-binding unit-id
  unit-expr
  (import tagged-sig-spec ...+)
  (export tagged-sig-spec ...+)
  init-depends-decl)
]{

Like @scheme[define-unit], but the unit implementation is determined
from an existing unit produced by @scheme[unit-expr]. The imports and
exports of the unit produced by @scheme[unit-expr] must be consistent
with the declared imports and exports, otherwise the
@exnraise[exn:fail:contract] when the @scheme[define-unit-binding]
form is evaluated.}

@defform/subs[
#:literals (link)
(invoke-unit/infer unit-spec)
[(unit-spec unit-id (link link-unit-id ...))]]{

Like @scheme[invoke-unit], but uses static information associated with
@scheme[unit-id] to infer which imports must be assembled from the
current context.  If given a link form containing multiple
@scheme[link-unit-id]s, then the units are first linked via
@scheme[define-compound-unit/infer].}

@defform/subs[
#:literals (link)
(define-values/invoke-unit/infer maybe-exports unit-spec)
[(maybe-exports code:blank (export tagged-sig-spec ...))
 (unit-spec unit-id (link link-unit-id ...))]]{

Like @scheme[define-values/invoke-unit], but uses static information
associated with @scheme[unit-id] to infer which imports must be
assembled from the current context and what exports should be bound
by the definition.  If given a link form containing multiple
@scheme[link-unit-id]s, then the units are first linked via
@scheme[define-compound-unit/infer].}

@; ------------------------------------------------------------------------

@section{Generating A Unit from Context}

@defform[
(unit-from-context tagged-sig-spec)
]{

Creates a unit that implements an interface using bindings in the
enclosing environment.  The generated unit is essentially the same as

@schemeblock[
(unit
  (import)
  (export tagged-sig-spec)
  (define id expr) ...)
]

for each @scheme[id] that must be defined to satisfy the exports, and
each corresponding @scheme[expr] produces the value of @scheme[id] in
the environment of the @scheme[unit-from-context] expression. (The unit
cannot be written as above, however, since each @scheme[id] definition
within the unit shadows the binding outside the @scheme[unit] form.)

See @scheme[unit] for the grammar of @scheme[tagged-sig-spec].}

@defform[
(define-unit-from-context id tagged-sig-spec)
]{

Like @scheme[unit-from-context], in that a unit is constructed from
the enclosing environment, and like @scheme[define-unit], in that 
@scheme[id] is bound to static information to be used later with inference.}

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

Similar to @scheme[unit], except the body of the unit is determined by
an existing unit produced by @scheme[unit-expr]. The result is a unit
that whose implementation is @scheme[unit-expr], but whose imports,
exports, and initialization dependencies are as in the
@scheme[unit/new-import-export] form (instead of as in the unit
produced by @scheme[unit-expr]).

The final clause of the @scheme[unit/new-import-export] form
determines the connection between the old and new imports and exports.
The connection is similar to the way that @scheme[compound-unit]
propagates imports and exports; the difference is that the connection
between @scheme[import] and the right-hand side of the link clause is
based on the names of elements in signatures, rather than the names of
the signatures. That is, a @scheme[tagged-sig-spec] on the right-hand
side of the link clause need not apppear as a @scheme[tagged-sig-spec]
in the @scheme[import] clause, but each of the bindings implied by the
linking @scheme[tagged-sig-spec] must be implied by some
@scheme[tagged-sig-spec] in the @scheme[import] clause. Similarly,
each of the bindings implied by an @scheme[export]
@scheme[tagged-sig-spec] must be implied by some left-hand-side
@scheme[tagged-sig-spec] in the linking clause.}

@defform[
#:literals (import export)
(define-unit/new-import-export unit-id
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...)
  init-depends-decl 
  ((tagged-sig-spec ...) unit-expr tagged-sig-spec))
]{

Like @scheme[unit/new-import-export], but binds static information to
@scheme[unit-id] like @scheme[define-unit].}

@defform[
#:literals (import export)
(unit/s
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...)
  init-depends-decl
  unit-id)]{

Like @scheme[unit/new-import-export], but the linking clause is
inferred, so @scheme[unit-id] must have the appropriate static
information.}
@defform[
#:literals (import export)
(define-unit/s name-id
  (import tagged-sig-spec ...)
  (export tagged-sig-spec ...)
  init-depends-decl
  unit-id)]{

Like @scheme[unit/s], but binds static information to @scheme[name-id]
like @scheme[define-unit].}

@; ------------------------------------------------------------------------

@section[#:tag "define-sig-form"]{Extending the Syntax of Signatures}

@defform*[
[(define-signature-form sig-form-id expr)
 (define-signature-form (sig-form-id id) body ...+)]
]{

Binds @scheme[sig-form-id] for use within a @scheme[define-signature]
form.

In the first form, the result of @scheme[expr] must be a transformer
procedure.  In the second form, @scheme[sig-form-id] is bound to a
transformer procedure whose argument is @scheme[id] and whose body is
the @scheme[body]s. The result of the transformer must be a list of
syntax objects, which are substituted for a use of
@scheme[sig-form-id] in a @scheme[define-signature] expansion. (The
result is a list so that the transformer can produce multiple
declarations; @scheme[define-signature] has no splicing @scheme[begin]
form.)}

@defform/subs[
(struct/ctc id ([field contract-expr] ...) struct-option ...) 

([field id
        [id #:mutable]]
 [struct-option #:mutable
                #:omit-constructor
                #:omit-define-syntaxes
                #:omit-define-values])]{

For use with @scheme[define-signature]. The @scheme[struct/ctc] form works
similarly to @scheme[struct], but the constructor, predicate, field
accessors, and field mutators are contracted appropriately.}

@; ------------------------------------------------------------------------

@section{Unit Utilities}

@defproc[(unit? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a unit, @scheme[#f] otherwise.}


@defform[(provide-signature-elements sig-spec ...)]{

Expands to a @scheme[provide] of all identifiers implied by the
@scheme[sig-spec]s. See @scheme[unit] for the grammar of
@scheme[sig-spec].}

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
Variables used in a given @scheme[contract] expression first refer to other
variables in the same signature, and then to the context of the 
@scheme[unit/c] expression.}
                                          
@defform/subs[#:literals (import export)
              (define-unit/contract unit-id
                (import sig-spec-block ...)
                (export sig-spec-block ...)
                init-depends-decl
                unit-body-expr-or-defn
                ...)
              ([sig-spec-block (tagged-sig-spec [id contract] ...)
                               tagged-sig-spec])]{
The @scheme[define-unit/contract] form defines an unit compatible with
link inference whose imports and exports are contracted with a unit
contract.  The unit name is used for the positive blame of the contract.}


@; ------------------------------------------------------------------------

@section[#:tag "single-unit"]{Single-Unit Modules}

When @schememodname[racket/unit] is used as a language name with
@schemefont{#lang}, the module body is treated as a unit body.  The
body must match the following @scheme[_module-body] grammar:

@schemegrammar*[
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

After any number of @scheme[_require-decl]s, the content of the module
is the same as a @scheme[unit] body.

The resulting unit is exported as @scheme[_base]@schemeidfont["@"],
where @scheme[_base] is derived from the enclosing module's name
(i.e., its symbolic name, or its path without the directory and file
suffix). If the module name ends in @schemeidfont{-unit}, then
@scheme[_base] corresponds to the module name before
@schemeidfont{-unit}. Otherwise, the module name serves as
@scheme[_base].

@; ------------------------------------------------------------------------

@section{Single-Signature Modules}

@defmodulelang[racket/signature]{The @schememodname[racket/signature]
language treats a module body as a unit signature.}

The body must match the following @scheme[_module-body] grammar:

@schemegrammar*[
#:literals (require)
[module-body (code:line (require require-spec ...) ... sig-spec ...)]
]

See @secref["creatingunits"] for the grammar of @scheme[_sig-spec].
Unlike the body of a @schememodname[racket/unit] module, a
@scheme[require] in a @schememodname[racket/signature] module must be
a literal use of @scheme[require].


The resulting signature is exported as
@scheme[_base]@schemeidfont["^"], where @scheme[_base] is derived from
the enclosing module's name (i.e., its symbolic name, or its path
without the directory and file suffix). If the module name ends in
@schemeidfont{-sig}, then @scheme[_base] corresponds to the module
name before @schemeidfont{-sig}. Otherwise, the module name serves as
@scheme[_base].

@; ----------------------------------------------------------------------

@section{Transformer Helpers}

@defmodule[racket/unit-exptime #:use-sources (mzlib/unit-exptime)]

The @schememodname[racket/unit-exptime] library provides procedures
that are intended for use by macro transformers. In particular, the
library is typically imported using @scheme[for-syntax] into a module
that defines macro with @scheme[define-syntax].

@defproc[(unit-static-signatures [unit-identifier identifier?]
                                 [err-syntax syntax?])
         (values (list/c (cons/c (or/c symbol? #f)
                                 identifier?))
                 (list/c (cons/c (or/c symbol? #f)
                                 identifier?)))]{

If @scheme[unit-identifier] is bound to static unit information via
@scheme[define-unit] (or other such forms), the result is two
values. The first value is for the unit's imports, and the second is
for the unit's exports. Each result value is a list, where each list
element pairs a symbol or @scheme[#f] with an identifier. The symbol
or @scheme[#f] indicates the import's or export's tag (where
@scheme[#f] indicates no tag), and the identifier indicates the
binding of the corresponding signature.

If @scheme[unit-identifier] is not bound to static unit information,
then the @exnraise[exn:fail:syntax]. In that case, the given
@scheme[err-syntax] argument is used as the source of the error, where
@scheme[unit-identifer] is used as the detail source location.}


@defproc[(signature-members [sig-identifier identifier?]
                            [err-syntax syntax?])
         (values (or/c identifier? #f)
                 (listof identifier?)
                 (listof identifier?)
                 (listof identifier?))]{

If @scheme[sig-identifier] is bound to static unit information via
@scheme[define-signature] (or other such forms), the result is four
values:

@itemize[

  @item{an identifier or @scheme[#f] indicating the signature (of any)
        that is extended by the @scheme[sig-identifier] binding;}

  @item{a list of identifiers representing the variables
        supplied/required by the signature;}

  @item{a list of identifiers for variable definitions in the
        signature (i.e., variable bindings that are provided on
        import, but not defined by units that implement the
        signature); and}

  @item{a list of identifiers with syntax definitions in the signature.}

]

If @scheme[sig-identifier] is not bound to a signature, then the
@exnraise[exn:fail:syntax]. In that case, the given
@scheme[err-syntax] argument is used as the source of the error, where
@scheme[sig-identifier] is used as the detail source location.}

