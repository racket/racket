#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require["utils.ss"]
@require-for-syntax[mzscheme]

@define-syntax[def-section-like
               (syntax-rules ()
                 [(_ id result/c x ...) (defproc (id [#:tag tag (or/c false/c string?) #f]
                                                     [pre-content any/c] (... ...+))
                                                 result/c
                                                 x ...)])]

@define-syntax[def-elem-proc
               (syntax-rules ()
                 [(_ id x ...) (defproc (id [pre-content any/c] (... ...))
                                        element?
                                        x ...)])]
@define-syntax[def-style-proc
               (syntax-rules ()
                 [(_ id) @def-elem-proc[id]{Like @scheme[elem], but with style @scheme['id]}])]

@title[#:tag "basic"]{Basic Document Forms}

The @file{basic.ss} libraryprovides functions and forms that can be
used from code written either in Scheme or with @elem["@"]
expressions. For example, the @scheme[title] and @scheme[italic]
functions might be called from Scheme as

@schemeblock[
(title #:tag "how-to" "How to Design " (italic "Great") " Programs")
]

or with an @elem["@"] expression as

@verbatim[
#<<EOS
  @title[#:tag "how-to"]{How to Design @italic{Great} Programs}
EOS
]

Although the procedures are mostly design to be used from @elem["@"]
mode, they are easier to document in Scheme mode (partly because we
have Scribble's @file{scheme.ss} and @file{manual.ss}).

@; ------------------------------------------------------------------------

@section{Document Structure}

@defproc[(title [#:tag tag (or/c false/c string?) #f]
                [#:style style any/c #f]
                [pre-content any/c] ...+)
         title-decl?]{

Generates a @scheme[title-decl] to be picked up by @scheme[decode] or
@scheme[decode-part].  The @scheme[pre-content]s list is parsed with
@scheme[decode-content] for the title content. If @scheme[tag] is
@scheme[#f], a tag string is generated automatically from the
content. The tag string is combined with the symbol @scheme['part] to
form the full tag.

A style of @scheme['toc] causes sub-sections to be generated as
separate pages in multi-page HTML output. A style of @scheme['index]
indicates an index section whose body is rendered in two columns for
Latex output.

The section title is automatically indexed. For the index key, a
leading ``A'', ``An'', or ``The'' (followed by whitespace) is
removed.}


@def-section-like[section part-start?]{ Like @scheme[title], but
 generates a @scheme[part-start] of depth @scheme[0] to be by
 @scheme[decode] or @scheme[decode-part].}

@def-section-like[subsection part-start?]{ Like @scheme[section], but
 generates a @scheme[part-start] of depth @scheme[1].}

@def-section-like[subsubsection part-start?]{ Like @scheme[section], but
 generates a @scheme[part-start] of depth @scheme[2].}

@def-section-like[subsubsub*section paragraph?]{ Similar to
 @scheme[section], but merely generates a paragraph that looks like an
 unnumbered section heading (for when the nesting gets too deep to
 include in a table of contents).}

@defproc[(itemize [itm (or/c whitespace? an-item?)] ...) itemization?]{

 Constructs an itemization given a sequence of items constructed by
 @scheme[item]. Whitespace strings among the @scheme[itm]s are
 ignored.

 }

@defproc[(item [pre-flow any/c] ...) item?]{
 Creates an item for use with @scheme[itemize]. The
 @scheme[pre-flow] list is parsed with @scheme[decode-flow].
}

@defproc[(item? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an item produced by
@scheme[item], @scheme[#f] otherwise.}

@defform[(include-section module-path)]{ Requires @scheme[module-path]
 and returns its @scheme[doc] export (without making any imports
 visible to the enclosing context). Since this form expands to
 @scheme[require], it must be used in a module or top-level context.}

@; ------------------------------------------------------------------------

@section{Text Styles}

@def-elem-proc[elem]{ Parses the @scheme[pre-content] list using
@scheme[decode-content], and wraps the result as an element with
style @scheme[#f].}

@def-elem-proc[aux-elem]{Like @scheme[elem], but creates an
@scheme[aux-element].}

@def-style-proc[italic]
@def-style-proc[bold]
@def-style-proc[tt]
@def-style-proc[subscript]
@def-style-proc[superscript]

@defproc[(hspace [n nonnegative-exact-integer?]) element?]{
Produces an element containing @scheme[n] spaces and style @scheme['hspace].
}

@defproc[(span-class [style-name string?] [pre-content any/c] ...)
         element?]{

Parses the @scheme[pre-content] list using @scheme[decode-content],
and produces an element with style @scheme[style-name].

}

@; ------------------------------------------------------------------------

@section{Indexing}

@defproc[(index [words (or/c string? (listof string?))]
                [pre-content any/c] ...)
         index-element?]{

Creates an index element given a plain-text string---or list of
strings for a hierarchy, such as @scheme['("strings" "plain")] for a
``plain'' entry until a more general ``strings'' entry. The strings
also serve as the text to render in the index. The
@scheme[pre-content] list, as parsed by @scheme[decode-content] is the
text to appear in place of the element, to which the index entry
refers.

}

@defproc[(index* [words (listof string?)]
                 [word-contents (listof list?)]
                 [pre-content any/c] ...)
         index-element?]{
Like @scheme[index], except that @scheme[words] must be a list, and
the list of contents render in the index (in parallel to
@scheme[words]) is supplied as @scheme[word-contents].
}

@defproc[(as-index [pre-content any/c] ...)
         index-element?]{

Like @scheme[index], but the word to index is determined by applying
@scheme[content->string] on the parsed @scheme[pre-content] list.}


@defproc[(section-index [word string?] ...)]{

Creates a @scheme[part-index-decl] to be associated with the enclosing
section by @scheme[decode]. The @scheme[word]s serve as both the keys
and as the rendered forms of the keys.}


@defproc[(index-section [tag string?])]{

Produces a section that shows the index the enclosing document. The
@scheme[tag] is required to be unique for each index among a set of
documents that share ``collected'' information.}


@; ------------------------------------------------------------------------

@section{Tables of Contents}

@defproc[(table-of-contents) delayed-flow-element?]{

Returns a delayed flow element that expands to a table of contents for
the enclosing section. For LaTeX output, however, the table of
contents currently spans the entire enclosing document.}


@defproc[(local-table-of-contents) delayed-flow-element?]{

Returns a delayed flow element that may expand to a table of contents
for the enclosing section, depending on the output type. For
multi-page HTML output, the flow element is a table of contents; for
Latex output, the flow element is empty.}
