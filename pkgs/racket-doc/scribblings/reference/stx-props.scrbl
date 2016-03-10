#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "stxprops"]{Syntax Object Properties}

Every syntax object has an associated @deftech{syntax property} list,
which can be queried or extended with
@racket[syntax-property]. A property is set as @tech{preserved} or not;
a preserved property is maintained for a syntax object in a compiled form that is
marshaled to a byte string or @filepath{.zo} file, and other properties
are discarded when marshaling.

In @racket[read-syntax], the reader attaches a preserved @racket['paren-shape]
property to any pair or vector syntax object generated from parsing a
pair @litchar{[} and @litchar{]} or @litchar["{"] and
@litchar["}"]; the property value is @racket[#\[] in the former case,
and @racket[#\{] in the latter case. The @racket[syntax] form copies
any @racket['paren-shape] property from the source of a template to
corresponding generated syntax.

Both the syntax input to a transformer and the syntax result of a
transformer may have associated properties. The two sets of properties
are merged by the syntax expander: each property in the original and
not present in the result is copied to the result, and the values of
properties present in both are combined with @racket[cons] (result
value first, original value second) and the @racket[cons]ed value is
@tech{preserved} if either of the values were preserved.

Before performing the merge, however, the syntax expander
automatically adds a property to the original syntax object using the
key @indexed-racket['origin]. If the source syntax has no
@racket['origin] property, it is set to the empty list.  Then, still
before the merge, the identifier that triggered the macro expansion
(as syntax) is @racket[cons]ed onto the @racket['origin]
property so far.  The @racket['origin] property thus records (in
reverse order) the sequence of macro expansions that produced an
expanded expression. Usually, the @racket['origin] value is a
list of identifiers. However, a transformer might return
syntax that has already been expanded, in which case an
@racket['origin] list can contain other lists after a merge. The
@racket[syntax-track-origin] procedure implements this tracking.
The @racket['origin] property is added as non-@tech{preserved}.

Besides @racket['origin] tracking for general macro expansion,
Racket adds properties to expanded syntax (often using
@racket[syntax-track-origin]) to record additional expansion details:

@itemize[

 @item{When a @racket[begin] form is spliced into a sequence with
 internal definitions (see @secref["intdef-body"]),
 @racket[syntax-track-origin] is applied to every spliced element from
 the @racket[begin] body. The second argument to
 @racket[syntax-track-origin] is the @racket[begin] form, and the
 third argument is the @racket[begin] keyword (extracted from the
 spliced form).}

 @item{When an internal @racket[define-values] or
 @racket[define-syntaxes] form is converted into a
 @racket[letrec-syntaxes+values] form (see @secref["intdef-body"]),
 @racket[syntax-track-origin] is applied to each generated binding
 clause.  The second argument to @racket[syntax-track-origin] is the
 converted form, and the third argument is the @racket[define-values]
 or @racket[define-syntaxes] keyword form the converted form.}

 @item{When a @racket[letrec-syntaxes+values] expression is fully
 expanded, syntax bindings disappear, and the result is either a
 @racket[letrec-values] form (if the unexpanded form contained
 non-syntax bindings), or only the body of the
 @racket[letrec-syntaxes+values] form (wrapped with @racket[begin] if
 the body contained multiple expressions). To record the disappeared
 syntax bindings, a property is added to the expansion result: an
 immutable list of identifiers from the disappeared bindings, as a
 @indexed-racket['disappeared-binding] property.}

 @item{When a subtyping @racket[struct] form is expanded, the
 identifier used to reference the base type does not appear in the
 expansion. Therefore, the @racket[struct] transformer adds the
 identifier to the expansion result as a
 @indexed-racket['disappeared-use] property.}

 @item{When a reference to an unexported or protected identifier from
 a module is discovered, the @indexed-racket['protected] property is
 added to the identifier with a @racket[#t] value.}

 @item{When @racket[read-syntax]
 generates a syntax object, it attaches a property to the object
 (using a private key) to mark the object as originating from a
 read. The @racket[syntax-original?]  predicate looks for the property
 to recognize such syntax objects. (See @secref["stxops"] for more
 information.)}

]

See also @seclink["Syntax_Properties_that_Check_Syntax_Looks_For"
                  #:doc '(lib "scribblings/tools/tools.scrbl")
                  #:indirect? #t]{Check Syntax}
for one client of the @racket['disappeared-use] and @racket['disappeared-binding]
properties.

See @secref["modinfo"] for information about properties generated
by the expansion of a module declaration. See @racket[lambda] and
@secref["infernames"] for information about properties recognized
when compiling a procedure. See @racket[current-compile] for
information on properties and byte codes.

@;------------------------------------------------------------------------

@defproc*[([(syntax-property [stx syntax?]
                             [key (if preserved? (and/c symbol? symbol-interned?) any/c)]
                             [v any/c]
                             [preserved? any/c (eq? key 'paren-shape)])
             syntax?]
           [(syntax-property [stx syntax?] [key any/c]) any])]{

The three- or four-argument form extends @racket[stx] by associating
an arbitrary property value @racket[v] with the key @racket[key]; the
result is a new syntax object with the association (while @racket[stx]
itself is unchanged). The property is added as @tech{preserved} if
@racket[preserved?] is true, in which case @racket[key] must be an
@tech{interned} symbol, and @racket[v] should be a value can itself
be saved in marshaled bytecode.

The two-argument form returns an arbitrary property value associated
to @racket[stx] with the key @racket[key], or @racket[#f] if no value
is associated to @racket[stx] for @racket[key].

@history[#:changed "6.4.0.14" @elem{Added the @racket[preserved?] argument.}]}


@defproc[(syntax-property-preserved? [stx syntax?] [key (and/c symbol? symbol-interned?)])
         boolean?]{

Returns @racket[#t] if @racket[stx] has a @tech{preserved} property
value for @racket[key], @racket[#f] otherwise.

@history[#:added "6.4.0.14"]}


@defproc[(syntax-property-symbol-keys [stx syntax?]) list?]{

Returns a list of all symbols that as keys have associated properties
in @racket[stx]. @tech{Uninterned} symbols (see @secref["symbols"])
are not included in the result list.}


@defproc[(syntax-track-origin [new-stx syntax?] [orig-stx syntax?] [id-stx identifier?])
         any]{

Adds properties to @racket[new-stx] in the same way that macro
expansion adds properties to a transformer result. In particular, it
merges the properties of @racket[orig-stx] into @racket[new-stx],
first adding @racket[id-stx] as an @racket['origin] property, and it
returns the property-extended syntax object. Use the
@racket[syntax-track-origin] procedure in a macro transformer that
discards syntax (corresponding to @racket[orig-stx] with a keyword
@racket[id-stx]) leaving some other syntax in its place (corresponding
to @racket[new-stx]).

For example, the expression

@racketblock[
(or x y)
]

expands to

@racketblock[
(let ([or-part x]) (if or-part or-part (or y)))
]

which, in turn, expands to

@racketblock[
(let-values ([(or-part) x]) (if or-part or-part y))
]

The syntax object for the final expression will have an
@racket['origin] property whose value is @racket[(list (quote-syntax
let) (quote-syntax or))].}


