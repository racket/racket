#lang scribble/doc
@(require "mz.ss")

@title[#:tag "stxops"]{Syntax Object Content}

@defproc[(syntax? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{syntax object}, @scheme[#f]
otherwise. See also @secref["stxobj-model"].}

@defproc[(syntax-source [stx syntax?]) any]{

Returns the source for the @tech{syntax object} @scheme[stx], or @scheme[#f]
if none is known. The source is represented by an arbitrary value
(e.g., one passed to @scheme[read-syntax]), but it is typically a file
path string. Source-location information is dropped for a syntax
object that is marshaled as part of compiled code; see also
@scheme[current-compile].}


@defproc[(syntax-line [stx syntax?]) 
         (or/c exact-positive-integer? #f)]{

Returns the line number (positive exact integer) for the start of the
@tech{syntax object} in its source, or @scheme[#f] if the line number or
source is unknown. The result is @scheme[#f] if and only if
@scheme[(syntax-column stx)] produces @scheme[#f]. See also
@secref["linecol"], and see @scheme[syntax-source] for information
about marshaling compiled @tech{syntax object}s.}


@defproc[(syntax-column [stx syntax?])
         (or/c exact-nonnegative-integer? #f)]{

Returns the column number (non-negative exact integer) for the start
of the @tech{syntax object} in its source, or @scheme[#f] if the source
column is unknown. The result is @scheme[#f] if and only if
@scheme[(syntax-line stx)] produces @scheme[#f]. See also
@secref["linecol"], and see @scheme[syntax-source] for information
about marshaling compiled @tech{syntax object}s.}


@defproc[(syntax-position [stx syntax?])
         (or/c exact-positive-integer? #f)]{

Returns the character position (positive exact integer) for the start
of the @tech{syntax object} in its source, or @scheme[#f] if the source
position is unknown. See also @secref["linecol"], and see
@scheme[syntax-source] for information about marshaling compiled
@tech{syntax object}s.}


@defproc[(syntax-span [stx syntax?])
         (or/c exact-nonnegative-integer? #f)]{

Returns the span (non-negative exact integer) in characters of the
@tech{syntax object} in its source, or @scheme[#f] if the span is
unknown. See also @scheme[syntax-source] for information about
marshaling compiled @tech{syntax object}s.}


@defproc[(syntax-original? [stx syntax?]) boolean?]{

Returns @scheme[#t] if @scheme[stx] has the property that
@scheme[read-syntax] and @scheme[read-honu-syntax] attach to the
@tech{syntax object}s that they generate (see @secref["stxprops"]), and if
@scheme[stx]'s @tech{lexical information} does not indicate that the
object was introduced by a syntax transformer (see
@secref["stxobj-model"]). The result is @scheme[#f] otherwise. This
predicate can be used to distinguish @tech{syntax object}s in an expanded
expression that were directly present in the original expression, as
opposed to @tech{syntax object}s inserted by macros.}


@defproc[(syntax-source-module [stx syntax?] [source? any/c #f])
         (or/c module-path-index? symbol? path? #f)]{

Returns an indication of the module whose source contains
@scheme[stx], or @scheme[#f] if @scheme[stx] has no source module.  If
@scheme[source?] is @scheme[#f], then result is a module path index or
symbol (see @secref["modpathidx"]); if @scheme[source?] is true, the
result is a path or symbol corresponding to the loaded module's
source in the sense of @scheme[current-module-declare-source].}


@defproc[(syntax-e [stx syntax?]) any]{

Unwraps the immediate datum structure from a @tech{syntax object},
leaving nested syntax structure (if any) in place.  The result of
@scheme[(syntax-e stx)] is one of the following:

    @itemize[

       @item{a symbol}

       @item{a @tech{syntax pair} (described below)}

       @item{the empty list}

       @item{an immutable vector containing @tech{syntax object}s}

       @item{an immutable box containing @tech{syntax object}s}

       @item{an immutable @tech{hash table} containing @tech{syntax
       object} values (but not necessarily @tech{syntax object} keys)}

       @item{an immutable @tech{prefab} structure containing @tech{syntax object}s}

       @item{some other kind of datum---usually a number, boolean, or string}

    ]

A @deftech{syntax pair} is a pair containing a @tech{syntax object} as its
first element, and either the empty list, a syntax pair, or a syntax
object as its second element.

A @tech{syntax object} that is the result of @scheme[read-syntax] reflects
the use of delimited @litchar{.} in the input by creating a syntax
object for every pair of parentheses in the source, and by creating a
pair-valued @tech{syntax object} @italic{only} for parentheses in the
source. See @secref["parse-pair"] for more information.}


@defproc[(syntax->list [stx syntax?]) (or/c list? #f)]{

Returns a list of @tech{syntax object}s or @scheme[#f]. The result is a list
of @tech{syntax object}s when @scheme[(syntax->datum stx)] would produce a
list. In other words, @tech{syntax pairs} in @scheme[(syntax-e stx)]
are flattened.}


@defproc[(syntax->datum [stx syntax?]) any]{

Returns a datum by stripping the lexical information, source-location
information, properties, and certificates from @scheme[stx]. Inside of
pairs, (immutable) vectors, (immutable) boxes, immutable @tech{hash
table} values (not keys), and immutable @tech{prefab} structures,
@tech{syntax object}s are recursively stripped.

The stripping operation does not mutate @scheme[stx]; it creates new
pairs, vectors, boxes, hash tables, and @tech{prefab} structures as
needed to strip lexical and source-location information recursively.}

@defproc[(datum->syntax [ctxt (or/c syntax? #f)]
                        [v any/c]
                        [srcloc (or/c syntax? #f
                                      (list/c any/c
                                              (or/c exact-positive-integer? #f)
                                              (or/c exact-nonnegative-integer? #f)
                                              (or/c exact-positive-integer? #f)
                                              (or/c exact-nonnegative-integer? #f))
                                      (vector/c any/c
                                               (or/c exact-positive-integer? #f)
                                               (or/c exact-nonnegative-integer? #f)
                                               (or/c exact-positive-integer? #f)
                                               (or/c exact-nonnegative-integer? #f)))
                                #f]
                        [prop (or/c syntax? #f) #f]
                        [cert (or/c syntax? #f) #f])
          syntax?]{

Converts the @tech{datum} @scheme[v] to a @tech{syntax object}.
The contents of pairs, vectors, and boxes, the fields of @tech{prefab}
structures, and the values of immutable hash tables are recursively converted.
The keys of @tech{prefab} structures and the keys of immutable hash tables are
not converted. Mutable vectors and boxes are replaced by immutable vectors and
boxes. For any kind of value other than a
pair, vector, box, immutable @tech{hash table}, immutable
@tech{prefab} structure, or @tech{syntax object}, conversion means
wrapping the value with lexical information, source-location
information, properties, and certificates.

Converted objects in @scheme[v] are given the lexical context
information of @scheme[ctxt] and the source-location information of
@scheme[srcloc]. If @scheme[v] is not already a @tech{syntax object},
then the resulting immediate @tech{syntax object} is given the
properties (see @secref["stxprops"]) of @scheme[prop] and the
@tech{inactive certificates} (see @secref["stxcerts"]) of
@scheme[cert]; if @scheme[v] is a pair, vector, box, immutable
@tech{hash table}, or immutable @tech{prefab} structure, recursively
converted values are not given properties or certificates.

Any of @scheme[ctxt], @scheme[srcloc], @scheme[prop], or @scheme[cert]
can be @scheme[#f], in which case the resulting syntax has no lexical
context, source information, new properties, and/or certificates.

If @scheme[srcloc] is not @scheme[#f] or a @tech{syntax object}, it
must be a list or vector of five elements:

@schemeblock[
  (list source-name line column position span)
  @#,elem{or} (vector source-name line column position span)
]

where @scheme[source-name-v] is an arbitrary value for the source
name; @scheme[line] is an integer for the source line, or @scheme[#f];
@scheme[column] is an integer for the source column, or @scheme[#f];
@scheme[position] is an integer for the source position, or
@scheme[#f]; and @scheme[span] is an integer for the source span, or
@scheme[#f]. The @scheme[line] and @scheme[column] values must both be
numbers or both be @scheme[#f], otherwise the
@exnraise[exn:fail:contract].

Graph structure is not preserved by the conversion of @scheme[v] to a
@tech{syntax object}. Instead, @scheme[v] is essentially unfolded into
a tree. If @scheme[v] has a cycle through pairs, vectors, boxes,
immutable @tech{hash tables}, and immutable @tech{prefab} structures,
then the @exnraise[exn:fail:contract].}

@defproc[(identifier? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{syntax object} and
@scheme[(syntax-e stx)] produces a symbol.}


@defproc[(generate-temporaries [stx-pair (or syntax? list?)]) 
         (listof identifier?)]{

Returns a list of identifiers that are distinct from all other
identifiers. The list contains as many identifiers as
@scheme[stx-pair] contains elements. The @scheme[stx-pair] argument
must be a syntax pair that can be flattened into a list. The elements
of @scheme[stx-pair] can be anything, but string, symbol, keyword
(possibly wrapped as syntax), and identifier elements will be embedded
in the corresponding generated name, which is useful for debugging
purposes. The generated identifiers are built with interned symbols
(not @scheme[gensym]s), so the limitations described with
@scheme[current-compile] do not apply.}


@defproc[(identifier-prune-lexical-context [id-stx identifier?]
                                           [syms (listof symbol?) (list (syntax-e id-stx))])
         identifier?]{

Returns an identifier with the same binding as @scheme[id-stx], but
without lexical information from @scheme[id-stx] that does not apply
to the symbols in @scheme[syms], where even further extension of the
lexical information drops information for other symbols. In
particular, transferring the lexical context via
@scheme[datum->syntax] from the result of this function to a symbol
other than one in @scheme[syms] produces a identifier with no binding.

See also @scheme[quote-syntax/prune].}
