#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:stxops"]{Syntax Object Content}

@defproc[(syntax? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{syntax object}, @scheme[#f]
otherwise. See also @secref["mz:stxobj-model"].}

@defproc[(syntax-source [stx syntax?]) any]{

Returns the source for the syntax object @scheme[stx], or @scheme[#f]
if none is known. The source is represented by an arbitrary value
(e.g., one passed to @scheme[read-syntax]), but it is typically a file
path string. Source-location information is dropped for a syntax
object that is marshaled as part of compiled code; see also
@scheme[current-compile].}


@defproc[(syntax-line [stx syntax?]) 
         (or/c positive-exact-integer?
               false/c)]{

Returns the line number (positive exact integer) for the start of the
syntax object in its source, or @scheme[#f] if the line number or
source is unknown. The result is @scheme[#f] if and only if
@scheme[(syntax-column stx)] produces @scheme[#f]. See also
@secref["mz:linecol"], and see @scheme[syntax-source] for information
about marshaling compiled syntax objects.}


@defproc[(syntax-column [stx syntax?])
         (or/c nonnegative-exact-integer?
               false/c)]{

Returns the column number (non-negative exact integer) for the start
of the syntax object in its source, or @scheme[#f] if the source
column is unknown. The result is @scheme[#f] if and only if
@scheme[(syntax-line stx)] produces @scheme[#f]. See also
@secref["mz:linecol"], and see @scheme[syntax-source] for information
about marshaling compiled syntax objects.}


@defproc[(syntax-position [stx syntax?])
         (or/c positive-exact-integer?
               false/c)]{

Returns the character position (positive exact integer) for the start
of the syntax object in its source, or @scheme[#f] if the source
position is unknown. See also @secref["mz:linecol"], and see
@scheme[syntax-source] for information about marshaling compiled
syntax objects.}


@defproc[(syntax-span [stx syntax?])
         (or/c nonnegative-exact-integer?
               false/c)]{

Returns the span (non-negative exact integer) in characters of the
syntax object in its source, or @scheme[#f] if the span is
unknown. See also @scheme[syntax-source] for information about
marshaling compiled syntax objects.}


@defproc[(syntax-original? [stx syntax?]) boolean?]{

Returns @scheme[#t] if @scheme[stx] has the property that
@scheme[read-syntax] and @scheme[read-honu-syntax] attach to the
syntax objects that they generate (see @secref["mz:stxprops"]), and if
@scheme[stx]'s @tech{lexical information} does not indicate that the
object was introduced by a syntax transformer (see
@secref["mz:stxobj-model"]). The result is @scheme[#f] otherwise. This
predicate can be used to distinguish syntax objects in an expanded
expression that were directly present in the original expression, as
opposed to syntax objects inserted by macros.}


@defproc[(syntax-source-module [stx syntax?])
         (or/c module-path-index? symbol?)]{

Returns a module path index or symbol (see @secref["mz:modpathidx"])
for the module whose source contains @scheme[stx], or @scheme[#f] if
@scheme[stx] has no source module.}


@defproc[(syntax-e [stx syntax?]) any]{

Unwraps the immediate datum structure from a syntax object,
leaving nested syntax structure (if any) in place.  The result of
@scheme[(syntax-e @scheme[stx])] is one of the following:

    @itemize{
    
       @item{a symbol}

       @item{a @tech{syntax pair} (described below)}

       @item{the empty list}

       @item{a vector containing syntax objects}

       @item{some other kind of datum---usually a number, boolean, or string}

    }

A @deftech{syntax pair} is a pair containing a syntax object as its
first element, and either the empty list, a syntax pair, or a syntax
object as its second element.

A syntax object that is the result of @scheme[read-syntax] reflects
the use of delimited @litchar{.} in the input by creating a syntax
object for every pair of parentheses in the source, and by creating a
pair-valued syntax object @italic{only} for parentheses in the
source. See @secref["mz:parse-pair"] for more information.}


@defproc[(syntax->list [stx syntax?])
         (or/c list? false/c)]{

Returns an immutable list of syntax objects or @scheme[#f]. The result
is a list of syntax objects when @scheme[(syntax->datum stx)]
would produce a list. In other words, @tech{syntax pairs} in
@scheme[(syntax-e @scheme[stx])] are flattened.}


@defproc[(syntax->datum [stx syntax?]) any]{

Returns a datum by stripping the lexical and source-location
information from @scheme[stx]. Graph structure is preserved by the
conversion.}


@defproc[(datum->syntax [ctxt (or/c syntax? false/c)]
                        [v any/c]
                        [srcloc (or/c syntax? false/c
                                      (list/c any/c
                                              (or/c positive-exact-integer? false/c)
                                              (or/c nonnegative-exact-integer? false/c)
                                              (or/c nonnegative-exact-integer? false/c)
                                              (or/c positive-exact-integer? false/c)))]
                        [prop (or/c syntax? false/c) #f]
                        [cert (or/c syntax? false/c) #f])
          syntax?]{

Converts the @tech{datum} @scheme[v] to a @tech{syntax object}, using
syntax objects already in @scheme[v] in the result. Converted objects
in @scheme[v] are given the lexical context information of
@scheme[ctxt] and the source-location information of
@scheme[srcloc]. If @scheme[v] is not already a syntax object, then
the resulting immediate syntax object it is given the properties (see
@secref["mz:stxprops"]) of @scheme[prop] and the @tech{inactive certificates}
(see @secref["mz:stxcerts"]) of @scheme[cert].  Any of
@scheme[ctxt], @scheme[srcloc], @scheme[prop], or @scheme[cert] can be
@scheme[#f], in which case the resulting syntax has no lexical
context, source information, new properties, and/or certificates.

If @scheme[srcloc] is not @scheme[#f]
or a syntax object, it must be a list of five elements:

@schemeblock[
  (list source-name line column position span)
]

where @scheme[source-name-v] is an arbitrary value for the source
name; @scheme[line] is an integer for the source line, or @scheme[#f];
@scheme[column] is an integer for the source column, or @scheme[#f];
@scheme[position] is an integer for the source position, or
@scheme[#f]; and @scheme[span] is an integer for the source span, or
@scheme[#f]. The @scheme[line] and @scheme[column] values must both be
numbers or both be @scheme[#f], otherwise the
@exnraise[exn:fail:contract].

Graph structure is preserved by the conversion of @scheme[v] to a
syntax object, but graph structure that is distributed among distinct
syntax objects in @scheme[v] may be hidden from future applications of
@scheme[syntax->datum] and @scheme[syntax-graph?] to the new
syntax object.}

@defproc[(syntax-graph? [stx syntax?]) boolean?]{

Returns @scheme[#t] if @scheme[stx] might be preservably shared within
a syntax object created by @scheme[read-syntax],
@scheme[read-honu-syntax], or @scheme[datum->syntax]. In general,
sharing detection is approximate---@scheme[datum->syntax] can
construct syntax objects with sharing that is hidden from
@scheme[syntax-graph?]---but @scheme[syntax-graph?] reliably returns
@scheme[#t] for at least one syntax object in a cyclic
structure. Meanwhile, deconstructing a syntax object with procedures
such as @scheme[syntax-e] and comparing the results with @scheme[eq?]
can also fail to detect sharing (even cycles), due to the way lexical
information is lazily propagated; only @scheme[syntax->datum] reliably
exposes sharing in a way that can be detected with @scheme[eq?].}

@defproc[(identifier? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a syntax object and
@scheme[(syntax-e stx)] produces a symbol.}


@defproc[(generate-temporaries [stx-pair (or syntax? list?)]) 
         (listof identifier?)]{

Returns a list of identifiers that are distinct from all other
identifiers. The list contains as many identifiers as
@scheme[stx-pair] contains elements. The @scheme[stx-pair] argument
must be a syntax pair that can be flattened into a list. The elements
of @scheme[stx-pair] can be anything, but string, symbol, and
identifier elements will be embedded in the corresponding generated
name (useful for debugging purposes). The generated identifiers are
built with interned symbols (not @scheme[gensym]s), so the limitations
described with @scheme[current-compile] do not apply.}
