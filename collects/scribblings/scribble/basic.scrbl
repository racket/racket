#reader"../docreader.ss"
@require["../manual.ss"]
@require["utils.ss"]
@require-for-syntax[mzscheme]

@define-syntax[def-title-like
               (syntax-rules ()
                 [(_ id result/c x ...) (defproc (id [#:tag tag (or/c false/c string?) #f]
                                                     [pre-content any/c] ...0)
                                                 result/c
                                                 x ...)])]

@define-syntax[def-elem-proc
               (syntax-rules ()
                 [(_ id x ...) (defproc (id [pre-content any/c] ...0)
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

@section{Document Structure}

@def-title-like[title title-decl?]{ Generates a @scheme[title-decl] to
 be picked up by @scheme[decode] or @scheme[decode-part].  The
 @scheme[pre-content]s list is parsed with @scheme[decode-content] for
 the title content. If @scheme[tag] is @scheme[#f], a tag string is
 generated automatically from the content. The tag string is combined
 with the symbol @scheme['section] to form the full tag.}

@def-title-like[section section-start?]{ Like @scheme[title], but
 generates a @scheme[section-start] of depth @scheme[0] to be by
 @scheme[decode] or @scheme[decode-part].}

@def-title-like[subsection section-start?]{ Like @scheme[section], but
 generates a @scheme[section-start] of depth @scheme[1].}

@def-title-like[subsubsection section-start?]{ Like @scheme[section], but
 generates a @scheme[section-start] of depth @scheme[2].}

@def-title-like[subsubsub*section paragraph?]{ Similar to
 @scheme[section], but merely generates a paragraph that looks like an
 unnumbered section heading (for when the nesting gets too deep to
 include in a table of contents).}

@defproc[(itemize [itm (or/c whitespace? an-item?)] ...0) itemization?]{

 Constructs an itemization given a sequence of items constructed by
 @scheme[item]. Whitespace strings among the @scheme[itm]s are
 ignored.

 }

@defproc[(item pre-flow ...0) item?]{
 Creates an item for use with @scheme[itemize]. The
 @scheme[pre-flow] list is parsed with @scheme[decode-flow].
}

@defform[(include-section module-path)]{ Requires @scheme[module-path]
 and returns its @scheme[doc] export (without making any imports
 visible to the enclosing context). Since this form expands to
 @scheme[require], it must be used in a module or top-level context.}

@section{Text Styles}

@def-elem-proc[elem]{ Parses the @scheme[pre-content] list using
@scheme[decode-content], and wraps the result as an element with
style @scheme[#f].}

@def-style-proc[italic]
@def-style-proc[bold]
@def-style-proc[tt]
@def-style-proc[subscript]
@def-style-proc[superscript]

@defproc[(hspace [n nonnegative-exact-integer?]) element?]{
Produces an element containing @scheme[n] spaces and style @scheme['hspace].
}

@defproc[(span-class [style-name string?] [pre-content any/c] ...0) 
         element?]{

Parses the @scheme[pre-content] list using @scheme[decode-content],
and produces an element with style @scheme[style-name].

}

@section{Indexing}

@defproc[(index [words (or/c string? (listof string?))]
                [pre-content any/c] ...0)
         index-element?] {

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
                 [pre-content any/c] ...0)
         index-element?] {
Like @scheme[index], except that @scheme[words] must be a list, and
the list of contents render in the index (in parallel to
@scheme[words]) is supplied as @scheme[word-contents].
}

@defproc[(as-index [pre-content any/c] ...0)
         index-element?] {

Like @scheme[index], but the word to index is determined by applying
@scheme[content->string] on the parsed @scheme[pre-content] list.

}


@section{Tables of Contents}

@defproc[(table-of-contents) delayed-flow-element?]{
 Returns a flow element that expands to a table of contents for the
 enclosing section. For LaTeX output, however, the table of contents
 currently spans the entire enclosing document.
}
