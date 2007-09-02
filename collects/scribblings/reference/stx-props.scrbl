#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "stxprops"]{Syntax Object Properties}

Every syntax object has an associated @deftech{syntax property} list,
which can be queried or extended with
@scheme[syntax-property]. Properties are not preserved for a
@scheme[syntax-quoted] syntax object in a compiled form that is
marshaled to a byte string.

In @scheme[read-syntax], the reader attaches a @scheme['paren-shape]
property to any pair or vector syntax object generated from parsing a
pair @litchar["["] and @litchar["]"] or @litchar["{"] and
@litchar["}"]; the property value is @scheme[#\[] in the former case,
and @scheme[#\{] in the latter case. The @scheme[syntax] form copies
any @scheme['paren-shape] property from the source of a template to
corresponding generated syntax.

Both the syntax input to a transformer and the syntax result of a
transformer may have associated properties. The two sets of properties
are merged by the syntax expander: each property in the original and
not present in the result is copied to the result, and the values of
properties present in both are combined with @scheme[cons] (result
value first, original value second).

Before performing the merge, however, the syntax expander
automatically adds a property to the original syntax object using the
key @indexed-scheme['origin]. If the source syntax has no
@scheme['origin] property, it is set to the empty list.  Then, still
before the merge, the identifier that triggered the macro expansion
(as syntax) is @scheme[cons]ed onto the @scheme['origin]
property so far.  The @scheme['origin] property thus records (in
reverse order) the sequence of macro expansions that produced an
expanded expression. Usually, the @scheme['origin] value is an
immutable list of identifiers. However, a transformer might return
syntax that has already been expanded, in which case an
@scheme['origin] list can contain other lists after a merge. The
@scheme[syntax-track-origin] procedure implements this tracking.

Besides @scheme['origin] tracking for general macro expansion,
MzScheme adds properties to expanded syntax (often using
@scheme[syntax-track-origin]) to record additional expansion details:

@itemize{

 @item{When a @scheme[begin] form is spliced into a sequence with
 internal definitions (see @secref["intdef-body"]),
 @scheme[syntax-track-origin] is applied to every spliced element from
 the @scheme[begin] body. The second argument to
 @scheme[syntax-track-origin] is the @scheme[begin] form, and the
 third argument is the @scheme[begin] keyword (extracted from the
 spliced form).}

 @item{When an internal @scheme[define-values] or
 @scheme[define-syntaxes] form is converted into a
 @scheme[letrec-values+syntaxes] form (see @secref["intdef-body"]),
 @scheme[syntax-track-origin] is applied to each generated binding
 clause.  The second argument to @scheme[syntax-track-origin] is the
 converted form, and the third argument is the @scheme[define-values]
 or @scheme[define-syntaxes] keyword form the converted form.}

 @item{When a @scheme[letrec-values+syntaxes] expression is fully
 expanded, syntax bindings disappear, and the result is either a
 @scheme[letrec-values] form (if the unexpanded form contained
 non-syntax bindings), or only the body of the
 @scheme[letrec-values+syntaxes] form (wrapped with @scheme[begin] if
 the body contained multiple expressions). To record the disappeared
 syntax bindings, a property is added to the expansion result: an
 immutable list of identifiers from the disappeared bindings, as a
 @indexed-scheme['disappeared-binding] property.}

 @item{When a subtyping @scheme[define-struct] form is expanded, the
 identifier used to reference the base type does not appear in the
 expansion. Therefore, the @scheme[define-struct] transformer adds the
 identifier to the expansion result as a
 @indexed-scheme['disappeared-use] property.}

 @item{When a reference to an unexported or protected identifier from
 a module is discovered (and the reference is certified; see
 @secref["stxcerts"]), the @indexed-scheme['protected] property is
 added to the identifier with a @scheme[#t] value.}

 @item{When or @scheme[read-syntax] or @scheme[read-honu-syntax]
 generates a syntax object, it attaches a property to the object
 (using a private key) to mark the object as originating from a
 read. The @scheme[syntax-original?]  predicate looks for the property
 to recognize such syntax objects. (See @secref["stxops"] for more
 information.)}

}

See @secref["modinfo"] for information about properties generated
by the expansion of a module declaration. See @scheme[lambda] and
@secref["infernames"] for information about properties recognized
when compiling a procedure. See @scheme[current-compile] for
information on properties and byte codes.

@;------------------------------------------------------------------------

@defproc[(syntax-property [stx syntax?][key any/c][v any/c])
         syntax?]{

Extends @scheme[stx] by associating an arbitrary property value
@scheme[v] with the key @scheme[key]; the result is a new syntax
object with the association (while @scheme[stx] itself is unchanged).}


@defproc[(syntax-property [stx syntax?][key any/c]) any]{

Returns an arbitrary property value associated to @scheme[stx] with
the key @scheme[key], or @scheme[#f] if no value is associated to
@scheme[stx] for @scheme[key].}


@defproc[(syntax-property-symbol-keys [stx syntax?]) list?]{

Returns a list of all symbols that as keys have associated properties
in @scheme[stx]. @tech{Uninterned} symbols (see @secref["symbols"])
are not included in the result list.}


@defproc[(syntax-track-origin [new-stx syntax?][orig-stx syntax?][id-stx syntax?]) 
         any]{

Adds properties to @scheme[new-stx] in the same way that macro
expansion adds properties to a transformer result. In particular, it
merges the properties of @scheme[orig-stx] into @scheme[new-stx],
first adding @scheme[id-stx] as an @scheme['origin] property, and it
returns the property-extended syntax object. Use the
@scheme[syntax-track-origin] procedure in a macro transformer that
discards syntax (corresponding to @scheme[orig-stx] with a keyword
@scheme[id-stx]) leaving some other syntax in its place (corresponding
to @scheme[new-stx]).

For example, the expression

@schemeblock[
(or x y)
]

expands to

@schemeblock[
(let ((or-part x)) (if or-part or-part (or y)))
]

which, in turn, expands to

@schemeblock[
(let-values ([(or-part) x]) (if or-part or-part y))
]

The syntax object for the final expression will have an
@scheme['origin] property whose value is @scheme[(list (quote-syntax
let) (quote-syntax or))].}


