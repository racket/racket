#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-syntax scheme/base)
          (for-label setup/main-collects))

@(define-syntax def-section-like
   (syntax-rules ()
     [(_ id result/c x ...)
      (defproc (id [#:tag tag (or/c false/c string? (listof string?)) #f]
                   [#:tag-prefix tag-prefix (or/c false/c string? module-path?) #f]
                   [#:style style (or/c style? #f string? symbol? (listof symbol?)) #f]
                   [pre-content pre-content?] (... ...+))
        result/c
        x ...)]))

@(define-syntax def-elem-proc
   (syntax-rules ()
     [(_ id x ...)
      (defproc (id [pre-content pre-content?] (... ...))
        element?
        x ...)]))
@(define-syntax def-style-proc
   (syntax-rules ()
     [(_ id)
      @def-elem-proc[id]{Like @scheme[elem], but with style @scheme['id].}]))

@title[#:tag "base"]{Base Document Format}

@defmodulelang[scribble/base]{The @schememodname[scribble/base] language
provides functions and forms that can be used from code written either
in Scheme or with @elem["@"] expressions.

The @schememodname[scribble/base] name can also be used as a
library with @scheme[require], in which case it provides all of the same
bindings, but without setting the reader or setting the default
rendering format to the PLT Scheme manual format.}

Functions provided by this library, such as @scheme[title] and
@scheme[italic], might be called from Scheme as

@schemeblock[
(title #:tag "how-to" 
       "How to Design " (italic "Great") " Programs")
]

They can also be called with @elem["@"] notation as

@verbatim[#:indent 2]|{
  @title[#:tag "how-to"]{How to Design @italic{Great} Programs}
}|

Although the procedures are mostly design to be used from @elem["@"]
mode, they are easier to document in Scheme mode (partly because we
have @schememodname[scribble/manual]).

@; ------------------------------------------------------------------------

@section{Document Structure}

@defproc[(title [#:tag tag (or/c false/c string? (listof string?)) #f]
                [#:tag-prefix tag-prefix (or/c false/c string? module-path?) #f]
                [#:style style (or/c style? #f string? symbol? (listof symbol?)) #f]
                [#:version vers (or/c string? false/c) #f]
                [pre-content pre-content?] ...+)
         title-decl?]{

Generates a @scheme[title-decl] to be picked up by @scheme[decode] or
@scheme[decode-part].  The @tech{decode}d @scheme[pre-content] (i.e.,
parsed with @scheme[decode-content]) supplies the title content. If
@scheme[tag] is @scheme[#f], a tag string is generated automatically
from the content. The tag string is combined with the symbol
@scheme['part] to form the full tag.

The @scheme[style] argument can be a style structure, or it can be one
of the following: a @scheme[#f] that corresponds to a ``plain'' style,
a string that is used as a @tech{style name}, a symbol that is used as
a @tech{variant}, or a list of symbols to be used as @tech{variants}.
For information on styles, see @scheme[part]. For example, a style of
@scheme['toc] causes sub-sections to be generated as separate pages in
multi-page HTML output.

The @scheme[tag-prefix] argument is propagated to the generated
structure (see @secref["tags"]). If @scheme[tag-prefix] is a module
path, it is converted to a string using
@scheme[module-path-prefix->string].

The @scheme[vers] argument is propagated to the @scheme[title-decl]
structure. Use @scheme[""] as @scheme[vers] to suppress version
rendering in the output.

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

@defform[(include-section module-path)]{ Requires @scheme[module-path]
 and returns its @scheme[doc] export (without making any imports
 visible to the enclosing context). Since this form expands to
 @scheme[require], it must be used in a module or top-level context.}

@defproc[(author [auth content?] ...) block?]{

Generates a @scheme[paragraph] with style name @scheme['author] to
show the author(s) of a document, where each author is represented by
@tech{content}. Normally, this function is used after
@scheme[title] for the beginning of a document. See also
@scheme[author+email].}

@defproc[(author+email [author elem] [email string?]) element?]{

Combines an author name with an e-mail address, obscuring the e-mail
address slightly to avoid address-harvesting robots.}

@; ------------------------------------------------------------------------

@section{Blocks}

@defproc[(para [#:style style (or/c style? string? symbol? #f)] 
               [pre-content pre-content?] ...) paragraph?]{

 Creates a @tech{paragraph} containing the @tech{decode}d
 @scheme[pre-content] (i.e., parsed with @scheme[decode-paragraph]).

 The @scheme[style] argument can be a style, @scheme[#f] to indicate a
  ``plain'' style, a string that is used as a @tech{style name}, or a
  symbol that is used as a @tech{style name}. (Note that
  @scheme[section] and @scheme[para] treat symbols differently as
  @scheme[style] arguments.)}


@defproc[(nested [#:style style (or/c style? string? symbol? #f)] 
                 [pre-flow pre-flow?] ...) nested-flow?]{

 Creates a @tech{nested flow} containing the @tech{decode}d
 @scheme[pre-flow] (i.e., parsed with @scheme[decode-flow]).
 
 The @scheme[style] argument is handled the same as @scheme[para].
 The @scheme['inset] style causes the nested flow to be inset compared
 to surrounding text.}


@defproc[(centered [pre-flow pre-flow?] ...) nested-flow?]{

Produces a @tech{nested flow} whose content is centered.}


@defproc[(margin-note [pre-content pre-content?] ...) blockquote?]{

Produces a @tech{nested flow} that is typeset in the margin, instead
of inlined.}


@defproc[(itemlist [itm item?] ...
                   [#:style style (or/c style? string? symbol? #f) #f]) 
         itemization?]{

 Constructs an @scheme[itemization] given a sequence of items
 constructed by @scheme[item].

 The @scheme[style] argument is handled the same as @scheme[para]. The
 @scheme['ordered] style numbers items, instead of just using a
 bullet.}


@defproc[(item [pre-flow pre-flow?] ...) item?]{

Creates an item for use with @scheme[itemlist]. The @tech{decode}d
@scheme[pre-flow] (i.e., parsed with @scheme[decode-flow]) is the item
content.}


@defproc[(item? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an item produced by
@scheme[item], @scheme[#f] otherwise.}


@defproc[(tabular [cells (listof (listof (or/c block? content? 'cont)))]
                  [#:style style (or/c style? string? symbol? #f) #f]) 
         table?]{

Creates a @tech{table} with the given content, which is supplies as a
list of rows, where each row has a list of cells. The length of all
rows must match.

Use @scheme['cont] as a cell to continue the content of the preceding
cell in a row in the space that would otherwise be used for a new
cell. A @scheme['cont] must not appear as the first cell in a row.

The @scheme[style] argument is handled the same as @scheme[para].}

@defproc[(verbatim [#:indent indent exact-nonnegative-integer? 0] [str string?] ...+)
         block?]{

Typesets @scheme[str]s in typewriter font with the linebreaks
specified by newline characters in @scheme[str]. Consecutive spaces in
the @scheme[str]s are converted to @scheme[hspace] to ensure that they
are all preserved in the output. Additional space (via
@scheme[hspace]) as specified by @scheme[indent] is added to the
beginning of each line.

The @scheme[str]s are @emph{not} decoded with @scheme[decode-content],
so @scheme[(verbatim "---")] renders with three hyphens instead of an
em-dash. Beware, however, that @litchar["@"] for a @scheme[verbatim]
call performs some processing before delivering arguments to
@scheme[verbatim]. The @scheme[verbatim] form is typically used with
@litchar["|{"]...@litchar["}|"] or similar brackets to disable
@litchar["@"] notation within the @scheme[verbatim] argument, like
this:

@verbatim[#:indent 2]|{
 @verbatim|{
   Use @bold{---} like this...
 }|
}|

which renders as

@verbatim[#:indent 2]|{
   Use @bold{---} like this...
}|

Even with @litchar["|{"]...@litchar["}|"], beware that consistent
leading whitespace is removed; see @secref["alt-body-syntax"] for more
information.

See also @scheme[literal].}

@; ------------------------------------------------------------------------

@section{Text Styles and Content}

@defproc[(elem [pre-content pre-content?] ...
               [#:style style (or style? string? symbol? #f) #f])
        element?]{

Wraps the @tech{decode}d @scheme[pre-content] as an element with style
@scheme[style].}


@def-style-proc[italic]
@def-style-proc[bold]
@def-style-proc[tt]
@def-style-proc[subscript]
@def-style-proc[superscript]

@def-elem-proc[smaller]{Like @scheme[elem], but with style
@scheme['smaller].  When uses of @scheme[smaller] are nested, text
gets progressively smaller.}

@def-elem-proc[larger]{Like @scheme[elem], but with style
@scheme['larger].  When uses of @scheme[larger] are nested, text
gets progressively larger.}

@defproc[(emph [pre-content pre-content?] ...) element?]{
The same as @scheme[italic].}

@defproc[(hspace [n exact-nonnegative-integer?]) element?]{

Produces an element containing @scheme[n] spaces and style
@scheme['hspace].}

@defproc[(literal [str string?] ...+) element?]{

Produces an element containing literally @scheme[str]s with no
decoding via @scheme[decode-content].

Beware that @litchar["@"] for a @scheme[literal] call performs some
processing before delivering arguments to @scheme[literal]. The
@scheme[literal] form can be used with @litchar["|{"]...@litchar["}|"]
or similar brackets to disable @litchar["@"] notation within the
@scheme[literal] argument, like this:

@verbatim[#:indent 2]|{
 @literal|{@bold{---}}|
}|

which renders as

@verbatim[#:indent 2]|{
   @literal|{@bold{---}}|
}|

See also @scheme[verbatim].}


@defproc[(image [path (or/c path-string? (cons/c 'collects (listof bytes?)))]
                [#:scale scale real? 1.0]
                [#:suffixes suffixes (listof #rx"^[.]") null]
                [pre-content pre-content?] ...)
         element?]{

 Creates an image element from the given path. The @tech{decode}d
 @scheme[pre-content] serves as the alternate text for contexts where
 the image cannot be displayed.

 The path is relative to the current directory, which is set by
 @exec{setup-plt} and @exec{scribble} to the directory of the main
 document file. The @scheme[path] argument also can be a result of
 @scheme[path->main-collects-relative].

 The strings in @scheme[suffixes] are filtered to those supported by
 given renderer, and then the acceptable suffixes are tried in
 order. The HTML renderer supports @scheme[".png"] and
 @scheme[".gif"], while the Latex renderer supports @scheme[".png"],
 @scheme[".pdf"], and @scheme[".ps"] (but @scheme[".ps"] works only
 when converting Latex output to DVI, and @scheme[".png"] and
 @scheme[".pdf"] work only for converting Latex output to PDF).}


@; ------------------------------------------------------------------------
@section[#:tag "base-links"]{Links}

@defproc[(hyperlink [url string?] [pre-content pre-content?] ...
                    [#:underline? underline? any/c #t]
                    [#:style style (or/c style? string? symbol? #f) (if underline? #f "plainlink")]) 
         element?]{

The @tech{decode}d @scheme[pre-content] is hyperlinked to
@scheme[url].  If @scheme[style] is not supplied, then
@scheme[underline?] determines how the link is rendered.}


@defproc[(url [dest string?]) element?]{

Generates a literal hyperlinked URL.}


@defproc[(secref [tag string?]
                 [#:doc module-path (or/c module-path? false/c) #f]
                 [#:tag-prefixes prefixes (or/c (listof string?) false/c) #f]
                 [#:underline? underline? any/c #t])
         element?]{

Inserts the hyperlinked title of the section tagged @scheme[tag], but
elements in the title content with the @scheme['aux] @tech{variant}
are omitted in the hyperlink label.

If @scheme[#:doc module-path] is provided, the @scheme[tag] refers to
a tag with a prefix determined by @scheme[module-path]. When
@exec{setup-plt} renders documentation, it automatically adds a tag
prefix to the document based on the source module. Thus, for example,
to refer to a section of the PLT Scheme reference,
@scheme[module-path] would be @scheme['(lib
"scribblings/reference/reference.scrbl")].

The @scheme[#:tag-prefixes prefixes] argument similarly supports
selecting a particular section as determined by a path of tag
prefixes. When a @scheme[#:doc] argument is provided, then
@scheme[prefixes] should trace a path of tag-prefixed subsections to
reach the @scheme[tag] section. When @scheme[#:doc] is not provided,
the @scheme[prefixes] path is relative to any enclosing section (i.e.,
the youngest ancestor that produces a match).

If @scheme[underline?] is @scheme[#f], then the hyperlink is rendered
in HTML without an underline.}


@defproc[(seclink [tag string?] 
                  [#:doc module-path (or/c module-path? false/c) #f]
                  [#:tag-prefixes prefixes (or/c (listof string?) false/c) #f]
                  [#:underline? underline? any/c #t]
                  [pre-content pre-content?] ...) element?]{

Like @scheme[secref], but the link label is the @tech{decode}d
@scheme[pre-content] instead of the target section's name.}


@defproc[(other-doc [module-path module-path?]
                    [#:underline? underline? any/c #t])
         element?]{

Like @scheme[secref] for the document's implicit @scheme["top"]
tag. Use this function to refer to a whole manual instead of
@scheme[secref], in case a special style in the future is used for
manual titles.}


@defproc[(elemtag [t (or/c tag? string?)] [pre-content pre-content?] ...) element?]{

The tag @scheme[t] refers to the content form of
@scheme[pre-content].}


@defproc[(elemref [t (or/c tag? string?)] [pre-content pre-content?] ... 
                  [#:underline? underline? any/c #t]) element?]{

The @tech{decode}d @scheme[pre-content] is hyperlinked to @scheme[t],
which is normally defined using @scheme[elemtag].}

@defproc[(module-path-prefix->string [mod-path module-path?])
         string?]{

Converts a module path to a string by resolving it to a path, and
using @scheme[path->main-collects-relative].}

@; ------------------------------------------------------------------------

@section[#:tag "base-indexing"]{Indexing}

@defproc[(index [words (or/c string? (listof string?))]
                [pre-content pre-content?] ...)
         index-element?]{

Creates an index element given a plain-text string---or list of
strings for a hierarchy, such as @scheme['("strings" "plain")] for a
``plain'' entry below a more general ``strings'' entry. As index keys,
the strings are ``cleaned'' using @scheme[clean-up-index-strings]. The
strings (without clean-up) also serve as the text to render in the
index. The @tech{decode}d @scheme[pre-content] is the text to appear
inline as the index target.

Use @scheme[index] when an index entry should point to a specific word
or phrase within the typeset document (i.e., the
@scheme[pre-content]). Use @scheme[section-index], instead, to create
an index entry that leads to a section, instead of a specific word or
phrase within the section.}


@defproc[(index* [words (listof string?)]
                 [word-contents (listof list?)]
                 [pre-content pre-content?] ...)
         index-element?]{
Like @scheme[index], except that @scheme[words] must be a list, and
the list of contents render in the index (in parallel to
@scheme[words]) is supplied as @scheme[word-contents].
}

@defproc[(as-index [pre-content pre-content?] ...)
         index-element?]{

Like @scheme[index], but the word to index is determined by applying
@scheme[content->string] on the @tech{decode}d @scheme[pre-content].}


@defproc[(section-index [word string?] ...)
         part-index-decl?]{

Creates a @scheme[part-index-decl] to be associated with the enclosing
section by @scheme[decode]. The @scheme[word]s serve as both the keys
and as the rendered forms of the keys within the index.}


@defproc[(index-section [#:tag tag (or/c false/c string?) "doc-index"])
         part?]{

Produces a part that shows the index the enclosing document. The
optional @scheme[tag] argument is used as the index section's tag.}


@; ------------------------------------------------------------------------

@section{Tables of Contents}

@defproc[(table-of-contents) delayed-block?]{

Returns a delayed flow element that expands to a table of contents for
the enclosing section. For Latex output, however, the table of
contents currently spans the entire enclosing document.}


@defproc[(local-table-of-contents [#:style style (or/c symbol? #f) #f])
         delayed-block?]{

Returns a delayed flow element that may expand to a table of contents
for the enclosing section, depending on the output type. For
multi-page HTML output, the flow element is a table of contents; for
Latex output, the flow element is empty.

The meaning of the @scheme[style] argument depends on the output type,
but @scheme['immediate-only] normally creates a table of contents that
contains only immediate sub-sections of the enclosing section. See
also the @scheme['quiet] style of @scheme[part] (i.e., in a
@scheme[part] structure, not supplied as the @scheme[style] argument
to @scheme[local-table-of-contents]), which normally suppresses
sub-part entries in a table of contents.}
