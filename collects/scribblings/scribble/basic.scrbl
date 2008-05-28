#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-syntax scheme/base)
          (for-label setup/main-collects))

@(define-syntax def-section-like
   (syntax-rules ()
     [(_ id result/c x ...)
      (defproc (id [#:tag tag (or/c false/c string?) #f]
                   [#:tag-prefix tag-prefix (or/c false/c string? module-path?) #f]
                   [#:style style any/c #f]
                   [pre-content any/c] (... ...+))
        result/c
        x ...)]))

@(define-syntax def-elem-proc
   (syntax-rules ()
     [(_ id x ...)
      (defproc (id [pre-content any/c] (... ...))
        element?
        x ...)]))
@(define-syntax def-style-proc
   (syntax-rules ()
     [(_ id)
      @def-elem-proc[id]{Like @scheme[elem], but with style @scheme['id]}]))

@title[#:tag "basic"]{Basic Document Forms}

@defmodule[scribble/basic]{The @schememodname[scribble/basic] library
provides functions and forms that can be used from code written either
in Scheme or with @elem["@"] expressions.}

For example, the @scheme[title] and @scheme[italic] functions might be
called from Scheme as

@schemeblock[
(title #:tag "how-to" 
       "How to Design " (italic "Great") " Programs")
]

or with an @elem["@"] expression as

@verbatim[#:indent 2]|{
  @title[#:tag "how-to"]{How to Design @italic{Great} Programs}
}|

Although the procedures are mostly design to be used from @elem["@"]
mode, they are easier to document in Scheme mode (partly because we
have @schememodname[scribble/manual]).

@; ------------------------------------------------------------------------

@section{Document Structure}

@defproc[(title [#:tag tag (or/c false/c string?) #f]
                [#:tag-prefix tag-prefix (or/c false/c string? module-path?) #f]
                [#:style style any/c #f]
                [#:version vers (or/c string? false/c) #f]
                [pre-content any/c] ...+)
         title-decl?]{

Generates a @scheme[title-decl] to be picked up by @scheme[decode] or
@scheme[decode-part].  The @tech{decode}d @scheme[pre-content] (i.e.,
parsed with @scheme[decode-content]) supplies the title content. If
@scheme[tag] is @scheme[#f], a tag string is generated automatically
from the content. The tag string is combined with the symbol
@scheme['part] to form the full tag.

A style of @scheme['toc] causes sub-sections to be generated as
separate pages in multi-page HTML output. A style of @scheme['index]
indicates an index section whose body is rendered in two columns for
Latex output.

The @scheme[tag-prefix] argument is propagated to the generated
structure (see @secref["tags"]). If @scheme[tag-prefix] is a module
path, it is converted to a string using
@scheme[module-path-prefix->string].

The @scheme[vers] argument is propagated to the @scheme[title-decl]
structure.

The section title is automatically indexed by
@scheme[decode-part]. For the index key, leading whitespace and a
leading ``A'', ``An'', or ``The'' (followed by more whitespace) is
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

Creates an item for use with @scheme[itemize]. The @tech{decode}d
@scheme[pre-flow] (i.e., parsed with @scheme[decode-flow]) is the item
content.}


@defproc[(item? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an item produced by
@scheme[item], @scheme[#f] otherwise.}


@defform[(include-section module-path)]{ Requires @scheme[module-path]
 and returns its @scheme[doc] export (without making any imports
 visible to the enclosing context). Since this form expands to
 @scheme[require], it must be used in a module or top-level context.}

@defproc[(module-path-prefix->string [mod-path module-path?])
         string?]{

Converts a module path to a string by resolving it to a path, and
using @scheme[path->main-collects-relative].}


@; ------------------------------------------------------------------------

@section{Text Styles}

@def-elem-proc[elem]{ Wraps the @tech{decode}d @scheme[pre-content] as
an element with style @scheme[#f].}

@def-elem-proc[aux-elem]{Like @scheme[elem], but creates an
@scheme[aux-element].}

@def-style-proc[italic]
@def-style-proc[bold]
@def-style-proc[tt]
@def-style-proc[subscript]
@def-style-proc[superscript]

@defproc[(hspace [n nonnegative-exact-integer?]) element?]{

Produces an element containing @scheme[n] spaces and style
@scheme['hspace].}


@defproc[(span-class [style-name string?] [pre-content any/c] ...)
         element?]{

Wraps the @tech{decode}d @scheme[pre-content] as an element with style
@scheme[style-name].}

@; ------------------------------------------------------------------------

@section{Indexing}

@defproc[(index [words (or/c string? (listof string?))]
                [pre-content any/c] ...)
         index-element?]{

Creates an index element given a plain-text string---or list of
strings for a hierarchy, such as @scheme['("strings" "plain")] for a
``plain'' entry below a more general ``strings'' entry. As index keys,
the strings are ``cleaned'' using @scheme[clean-up-index-strings]. The
strings (without clean-up) also serve as the text to render in the
index. The @tech{decode}d @scheme[pre-content] is the text to appear
inline as the index target.}


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
@scheme[content->string] on the @tech{decode}d @scheme[pre-content].}


@defproc[(section-index [word string?] ...)
         part-index-decl?]{

Creates a @scheme[part-index-decl] to be associated with the enclosing
section by @scheme[decode]. The @scheme[word]s serve as both the keys
and as the rendered forms of the keys.}


@defproc[(index-section [#:tag tag (or/c false/c string?) "doc-index"])
         part?]{

Produces a part that shows the index the enclosing document. The
optional @scheme[tag] argument is used as the index section's tag.}


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
