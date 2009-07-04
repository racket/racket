#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label scribble/manual-struct
                     setup/main-collects))

@title[#:tag "struct"]{Structures And Processing}

@defmodule[scribble/struct]

A document is represented as a @techlink{part}, as described in
 @secref["parts"]. This representation is intended to
 independent of its eventual rendering, and it is intended to be
 immutable; rendering extensions and specific data in a document can
 collude arbitrarily, however.

A document is processed in three passes. The first pass is the
 @deftech{collect pass}, which globally collects information in the
 document, such as targets for hyperlinking. The second pass is the
 @deftech{resolve pass}, which matches hyperlink references with
 targets and expands delayed elements (where the expansion should not
 contribute new hyperlink targets). The final pass is the
 @deftech{render pass}, which generates the resulting document. None
 of the passes mutate the document, but instead collect information in
 side @scheme[collect-info] and @scheme[resolve-info] tables.

@; ------------------------------------------------------------------------

@section[#:tag "parts"]{Parts}

A @deftech{part} is an instance of @scheme[part]; among other things,
 it has a title @techlink{content}, an initial @techlink{flow}, and a
 list of subsection @techlink{parts}. An @scheme[unnumbered-part] is
 the same as a @scheme[part], but it isn't numbered. A
 @scheme[versioned-part] is add a version field to
 @scheme[part]. There's no difference between a part and a full
 document; a particular source module just as easily defines a
 subsection (incorporated via @scheme[include-section]) as a document.

A @deftech{flow} is an instance of @scheme[flow]; it has a list of
 @techlink{blocks}.

A @deftech{block} is either a @techlink{table}, an
 @techlink{itemization}, a @techlink{blockquote}, a @techlink{paragraph},
 @techlink{compound paragraph}, or a @techlink{delayed block}.

@itemize[

       @item{A @deftech{table} is an instance of @scheme[table]; it
             has a list of list of @techlink{flows} with a particular
             style. In Latex output, each table cell is typeset as a
             single line.}

       @item{A @deftech{itemization} is an instance of @scheme[itemization];
             it has a list of @techlink{flows}.}

       @item{A @deftech{blockquote} is an instance of
             @scheme[blockquote]; it has list of @tech{blocks} that
             are typeset as sub-flow, and by default the subflow is
             inset.}

       @item{A @deftech{paragraph} is an instance of
             @scheme[paragraph]; it has a @deftech{content}, which is
             a list of @techlink{elements}:

             @itemize[

             @item{An @deftech{element} can be a string, one of a few
                   symbols, an instance of @scheme[element] (possibly
                   @scheme[link-element], etc.), a
                   @techlink{part-relative element}, a
                   @techlink{delayed element}, or anything else
                   allowed by the current renderer.

                   @itemize[

                   @item{A string element is included in the result
                         document verbatim, except for space, and
                         unless the element's style is
                         @scheme['hspace]. In a style other than
                         @scheme['hspace], consecutive spaces in the
                         output may be collapsed togther or replaced
                         with a line break. In the style
                         @scheme['hspace], all text is converted to
                         uncollapsable spaces that cannot be broken
                         across lines.}

                   @item{A symbol element is either @scheme['mdash],
                         @scheme['ndash], @scheme['ldquo],
                         @scheme['lsquo], @scheme['rsquo],
                         @scheme['rarr], or @scheme['prime]; it is
                         rendered as the corresponding HTML entity
                         (even for Latex output).}

                   @item{An instance of @scheme[element] has a list of
                         @techlink{elements} plus a style. The style's
                         interpretation depends on the rendrer, but it
                         can be one of a few special symbols (such as
                         @scheme['bold]) that are recognized by all
                         renderers.}

                   @item{An instance of @scheme[link-element] has a
                         @techlink{tag} for the target of the link.}

                   @item{An instance of @scheme[target-element] has a
                         @techlink{tag} to be referenced by
                         @scheme[link-element]s. An instance of the
                         subtype @scheme[toc-target-element] is
                         treated like a kind of section label, to be
                         shown in the ``on this page'' table for HTML
                         output.}

                   @item{An instance of @scheme[index-element] has a
                         @techlink{tag} (as a target), a list of
                         strings for the keywords (for sorting and
                         search), and a list of @techlink{elements} to
                         appear in the end-of-document index.}

                   @item{An instance of @scheme[collect-element] has a
                         procedure that is called in the
                         @techlink{collect pass} of document
                         processing to record information used by
                         later passes.}

                   @item{A @deftech{part-relative element} is an
                         instance of @scheme[part-relative-element],
                         which has a procedure that is called in the
                         @techlink{collect pass} of document
                         processing to obtain @defterm{content} (i.e.,
                         a list of @defterm{elements}). When the
                         part-relative element's procedure is called,
                         collected information is not yet available,
                         but information about the enclosing parts is
                         available.}

                   @item{A @deftech{delayed element} is an instance of
                         @scheme[delayed-element], which has a
                         procedure that is called in the
                         @techlink{resolve pass} of document
                         processing to obtain @defterm{content} (i.e.,
                         a list of @defterm{elements}).}

                   @item{An instance of @scheme[aux-element] is
                         excluded in the text of a link when it
                         appears in a referenced section name.}

                   @item{An instance of @scheme[hover-element] adds
                         text to show in render HTML when the mouse
                         hovers over the elements.}

                   @item{An instance of @scheme[script-element]
                         provides script code (usually
                         @as-index{Javascript}) to run in the browser
                         to generate the element; the element's normal
                         content is used when scripting is disabled in
                         the browser, or for rendering to other
                         formats.}

                   @item{An instance of @scheme[render-element] has a
                         procedure that is called in the
                         @techlink{render pass} of document
                         processing.}

             ]}]}

       @item{A @deftech{compound paragraph} is an instance of
             @scheme[compound-paragraph]; like @scheme[blockquote], it
             has list of @tech{blocks}, but the blocks are typeset as
             a single paragraph (e.g., no indentation after the first
             block) instead of inset.}

       @item{A @deftech{delayed block} is an instance of
             @scheme[delayed-block], which has a procedure that
             is called in the @techlink{resolve pass} of document
             processing to obtain a @defterm{block}.}

]

@; ------------------------------------------------------------------------

@section[#:tag "tags"]{Tags}

A @deftech{tag} is a list containing a symbol and either a string, a
@scheme[generated-tag] instance, or an arbitrary list. The symbol
effectively identifies the type of the tag, such as @scheme['part] for
a tag that links to a part, or @scheme['def] for a Scheme function
definition. The symbol also effectively determines the interpretation
of the second half of the tag.

A part can have a @deftech{tag prefix}, which is effectively added
onto the second item within each tag whose first item is
@scheme['part] or @scheme['tech]. The prefix is added to a string
value by creating a list containing the prefix and string, and it is
added to a list value using @scheme[cons]; a prefix is not added to a
@scheme[generated-tag] instance. The prefix is used for reference
outside the part, including the use of tags in the part's
@scheme[tags] field. Typically, a document's main part has a tag
prefix that applies to the whole document; references to sections and
defined terms within the document from other documents must include the prefix,
while references within the same document omit the prefix. Part
prefixes can be used within a document as well, to help disambiguate
references within the document.

Some procedures accept a ``tag'' that is just the string part of the
full tag, where the symbol part is supplied automatically. For
example, @scheme[section] and @scheme[secref] both accept a string
``tag'', where @scheme['part] is implicit.

@; ------------------------------------------------------------------------

@section[#:tag "passes"]{Collected and Resolved Information}

The @techlink{collect pass}, @techlink{resolve pass}, and
@techlink{render pass} processing steps all produce information that
is specific to a rendering mode. Concretely, the operations are all
represented as methods on a @scheme[render%] object.

The result of the @method[render% collect] method is a
@scheme[collect-info] instance. This result is provided back as an
argument to the @method[render% resolve] method, which produces a
@scheme[resolve-info] value that encapsulates the results from both
iterations. The @scheme[resolve-info] value is provided back to the
@method[render% resolve] method for final rendering.

Optionally, before the @method[render% resolve] method is called,
serialized information from other documents can be folded into the
@scheme[collect-info] instance via the @method[render%
deserialize-info] method. Other methods provide serialized information
out of the collected and resolved records.

During the @techlink{collect pass}, the procedure associated with a
@scheme[collect-element] instance can register information with
@scheme[collect-put!].

During the @techlink{resolve pass}, collected information for a part
can be extracted with @scheme[part-collected-info], which includes a
part's number and its parent part (or @scheme[#f]). More generally,
the @scheme[resolve-get] method looks up information previously
collected. This resolve-time information is normally obtained by the
procedure associated with a @techlink{delayed block} or
@techlink{delayed element}.

The @scheme[resolve-get] information accepts both a @scheme[part] and
a @scheme[resolve-info] argument. The @scheme[part] argument enables
searching for information in each enclosing part before sibling parts.

@; ------------------------------------------------------------------------

@section{Structure Reference}

@defstruct[part ([tag-prefix (or/c false/c string?)]
                 [tags (listof tag?)]
                 [title-content (or/c false/c list?)]
                 [style any/c]
                 [to-collect list?]
                 [flow flow?]
                 [parts (listof part?)])]{

The @scheme[tag-prefix] field determines the optional @techlink{tag
prefix} for the part.

The @scheme[tags] indicates a list of @techlink{tags} that each link
to the section.

The @scheme[title-content] field holds the part's title, if any.

The @scheme[style] field is normally either a symbol or a list.  The
currently recognized style symbols (alone or in a list) or other
values (must be in a list) are as follows:

@itemize[

 @item{@scheme['toc] --- sub-parts of the part are rendered on separate
       pages for multi-page HTML mode.}

 @item{@scheme['non-toc] --- initial sub-parts of the part are
       @emph{not} rendered on separate pages for multi-page HTML
       mode; this style applies only to the main part.}

 @item{@scheme['index] --- the part represents an index.}

 @item{@scheme['reveal] --- shows sub-parts when this part is
       displayed in a table-of-contents panel in HTML output (which
       normally shows only the top-level sections).}

 @item{@scheme['hidden] --- the part title is not shown in rendered output.}

 @item{@scheme['quiet] --- in HTML output and most other output modes,
       hides entries for sub-parts of this part in a
       @scheme[table-of-contents] or @scheme[local-table-of-contents]
       listing except when those sub-parts are top-level entries in
       the listing.}

 @item{@scheme['no-toc] --- as a style for the main part of a
       document, causes the HTML output to not include a margin box
       for the main table of contents; the ``on this page'' box that
       contains @scheme[toc-element] and @scheme[toc-target-element]
       links (and that only includes an ``on this page'' label for
       multi-page documents) takes on the location and color of the
       main table of contents, instead.}

  @item{@scheme[`(css ,_path)] --- generated HTML refers to (a copy
        of) @scheme[_path] as CSS.}

  @item{@scheme[`(tex ,_path)] --- generated Latex includes
        (a copy of) @scheme[_path] in the document header.}

  @item{@scheme[`(body-id ,_string)] --- generated HTML uses
        @scheme[_string] as the @tt{id} attribute of the @tt{body}
        tag; this style can be set separately for parts that start
        different HTML pages, otherwise it is effectively inherited by
        sub-parts; the default is @scheme["scribble-plt-scheme.org"],
        but @exec{setup-plt} installs @scheme["doc-plt-scheme.org"]
        as the @tt{id} for any document that it builds.}

]

The @scheme[to-collect] field contains @techlink{content} that is
inspected during the @techlink{collect pass}, but ignored in later
passes (i.e., it doesn't directly contribute to the output).

The @scheme[flow] field contains the part's initial flow (before
sub-parts).

The @scheme[parts] field contains sub-parts.

}


@defstruct[(unnumbered-part part) ()]{

Although a section number is computed for an ``unnumbered'' section
during the @techlink{collect pass}, the number is not rendered.

}


@defstruct[(versioned-part part) ([version (or/c string? false/c)])]{

Supplies a version number for this part and its sub-parts (except as
overridden). A @scheme[#f] version is the same as not supplying a
specific version.

The version number that is not @scheme[""] may be used when rendering
a document. At a minimum, a non-@scheme[""] version is rendered when
it is attached to a part representing the whole document. The default
version for a document is @scheme[(version)].}


@defstruct[flow ([paragraphs (listof block?)])]{

A @techlink{flow} has a list of @tech{blocks}.

}

@defstruct[paragraph ([content list?])]{

A @techlink{paragraph} has a list of @tech{elements}.

}

@defstruct[(omitable-paragraph paragraph) ()]{

Equivalent to a @scheme[paragraph], except that when a table cell
contains a single @scheme[omitable-paragraph], then when rendering to
HTML, no @tt{p} tag wraps the cell content.

}


@defstruct[(styled-paragraph paragraph) ([style any/c])]{

The @scheme[style] can be

@itemize[

 @item{A string that corresponds to a CSS class for HTML output or a
       macro for Latex output (see @secref["extra-style"]).}

 @item{An instance of @scheme[with-attributes], which combines a base
       style with a set of additional HTML attributes.}

 @item{The symbol @scheme['div], which generates @tt{<div>} HTML
       output instead of @tt{<p>}. For Latex output, a string for a
       macro name is extracted from the @scheme['class] mapping of a
       @scheme[with-attributes] wrapper, if one is present.}

]}


@defstruct[table ([style any/c]
                  [flowss (listof (listof (or/c flow? (one-of/c 'cont))))])]{

A @techlink{table} has, roughly, a list of list of flows. A cell in
the table can span multiple columns by using @scheme['cont] instead of
a flow in the following columns (i.e., for all but the first in a set
of cells that contain a single flow).

When a table cell's flow has multiple paragraphs, the rendered output
starts each paragraph on its own line, but generally doesn't insert
space between the paragraphs (as it would at the top level). For Latex
output, individual paragraphs are not automatically line-wrapped; to
get a line-wrapped paragraph, use an element with a string style and
define a corresponding Latex macro in terms of @tt{parbox}. For Latex
output of blocks in the flow that are @scheme[blockquote]s,
@scheme[itemization]s, @scheme[compound-paragraph]s, or
@scheme[delayed-block]s, the block is wrapped with @tt{minipage} using
@tt{linewidth} as the width.

The @scheme[style] can be any of the following:

@itemize[

 @item{A string that corresponds to a CSS class for HTML output or an
       environment for Latex output (see @secref["extra-style"]).}

 @item{@scheme['boxed] to render as a definition.}

 @item{@scheme['centered] to render centered horizontally.}

 @item{@scheme['at-left] to render left-aligned (HTML only).}

 @item{@scheme['at-right] to render right-aligned (HTML only).}

 @item{An association list with the following optional mappings:

       @itemize[ 
 
         @item{@scheme['style] to a string for a CSS class for HTML output.}

         @item{@scheme['alignment] to a list of symbols and
               @scheme[#f]s (one for each column); each symbol can be
               @scheme['left], @scheme['right], or @scheme['center].}

         @item{@scheme['valignment] to a list of symbols and
               @scheme[#f]s (one for each column); each symbol can be
               @scheme['top], @scheme['baseline], @scheme['center], 
               or @scheme['bottom].}

         @item{@scheme['row-styles] to a list of association lists,
               one for each row in the table. Each of these nested
               association lists can map @scheme['alignment] and
               @scheme['valignment] to a list of symbols and
               @scheme[#f]s (one for each column cell) and/or
               @scheme['style] to a list of strings and @scheme[#f]s
               (one for each column cell) for a CSS class in HTML
               output. Row-specific @scheme['valignment] and
               @scheme['alignment] associations override row-independent
               associations.}

         ]}

  @item{An instance of @scheme[with-attributes], which combines a base
       style with a set of additional HTML attributes.}

]}


@defstruct[itemization ([flows (listof flow?)])]{

A @techlink{itemization} has a list of flows.

}


@defstruct[(styled-itemization itemization) ([style any/c])]{

The @scheme[style] can be

@itemize[

 @item{A string that corresponds to a CSS class for HTML output or a
       macro for Latex output (see @secref["extra-style"]).}

 @item{The symbol @scheme['ordered], which generates @tt{<ol>} HTML
       output instead of @tt{<li>} or an Latex enumeration instead of
       an itemization.}

]}


@defstruct[blockquote ([style any/c]
                       [paragraphs (listof block?)])]{

A @techlink{blockquote} has a style and a list of @tech{blocks}.  The
@scheme[style] field is normally a string that corresponds to a CSS
class for HTML output or Latex environment for Latex output where a
leading @litchar{\} in the style name is treated specially (see
@secref["extra-style"]).

}

@defstruct[compound-paragraph ([style any/c]
                               [blocks (listof block?)])]{

A @techlink{compound paragraph} has a style and a list of @tech{blocks}.  The
@scheme[style] field is normally a string that corresponds to a CSS
class for HTML output or Latex environment for Latex output where a
leading @litchar{\} in the style name is treated specially (see
@secref["extra-style"]).

}

@defstruct[delayed-block ([resolve (any/c part? resolve-info? . -> . block?)])]{

The @scheme[resolve] procedure is called during the @techlink{resolve
pass} to obtain a normal @tech{block}. The first argument to
@scheme[resolve] is the renderer.

}


@defstruct[element ([style any/c]
                    [content list?])]{

The @scheme[style] field is normally either

@itemize[

 @item{a string, which corresponds to a CSS class for HTML output and
       a macro name for Latex output (see @secref["extra-style"]);}

 @item{one of the symbols that all renderers recognize: @scheme['tt],
       @scheme['italic], @scheme['bold], @scheme['sf], @scheme['url],
       @scheme['subscript], @scheme['superscript], @scheme['hspace],
       or @scheme['newline] (which renders a line break independent of
       the @scheme[content]);}

 @item{a list of the form @scheme[(list 'color _name)] or
       @scheme[(list 'color _byte _byte _byte)] to set the text color,
       where @scheme[_name] is one of @scheme["white"],
       @scheme["black"], @scheme["red"], @scheme["green"],
       @scheme["blue"], @scheme["cyan"], @scheme["magenta"], or
       @scheme["yellow"], or three @scheme[_byte]s specify RGB
       values;}

 @item{a list of the form @scheme[(list 'bg-color _name)] or
       @scheme[(list 'bg-color _byte _byte _byte)] to set the text
       background color (with the same constraints and meanings as for
       @scheme['color]);}

 @item{an instance of @scheme[target-url] to generate a hyperlink;}

 @item{an instance of @scheme[image-file] to support an inline image; or}

 @item{an instance of @scheme[with-attributes], which combines a base
       style with a set of additional HTML attributes.}

]

The @scheme[content] field is a list of @techlink{elements}.

}


@defstruct[(target-element element) ([tag tag?])]{

Declares the content as a hyperlink target for @scheme[tag].

}


@defstruct[(toc-target-element target-element) ()]{

Like @scheme[target-element], the content is also a kind of section
label to be shown in the ``on this page'' table for HTML output.

}


@defstruct[(toc-element element) ([toc-content list?])]{

Similar to @scheme[toc-target-element], but with specific content for
the ``on this page'' table specified in the @scheme[toc-content]
field.

}


@defstruct[(link-element element) ([tag tag?])]{

Hyperlinks the content to @scheme[tag].

}


@defstruct[(index-element element) ([tag tag?]
                                    [plain-seq (and/c pair? (listof string?))]
                                    [entry-seq list?]
                                    [desc any/c])]{

The @scheme[plain-seq] specifies the keys for sorting, where the first
@tech{element} is the main key, the second is a sub-key, etc. For
example, an ``night'' portion of an index might have sub-entries for
``night, things that go bump in'' and ``night, defender of the''. The
former would be represented by @scheme[plain-seq] @scheme['("night"
"things that go bump in")], and the latter by @scheme['("night"
"defender of the")]. Naturally, single-@tech{element}
@scheme[plain-seq] lists are the common case, and at least one word is
required, but there is no limit to the word-list length. The strings in 
@scheme[plain-seq] must not contain a newline character.

The @scheme[entry-seq] list must have the same length as
@scheme[plain-seq]. It provides the form of each key to render in the
final document.

The @scheme[desc] field provides additional information about the
index entry as supplied by the entry creator. For example, a reference
to a procedure binding can be recognized when @scheme[desc] is an
instance of @scheme[procedure-index-desc]. See
@schememodname[scribble/manual-struct] for other typical types of
@scheme[desc] values.

See also @scheme[index].}


@defstruct[(aux-element element) ()]{

Instances of this structure type are intended for use in titles, where
 the auxiliary part of the title can be omitted in hyperlinks. See,
 for example, @scheme[secref].

}

@defstruct[(hover-element element) ([text string?])]{

The @scheme[text] is displayed in HTML output when the mouse hovers
over the element's content.}


@defstruct[(script-element element) ([type string?]
                                     [script (or/c path-string?
                                                   (listof string?))])]{

For HTML rendering, when scripting is enabled in the browser,
@scheme[script] is used for the element instead of its normal
content---it can be either path naming a script file to refer to, or
the contents of the script. The @scheme[type] string is normally
@scheme["text/javascript"].}


@defstruct[delayed-element ([resolve (any/c part? resolve-info? . -> . list?)]
                            [sizer (-> any/c)]
                            [plain (-> any/c)])]{

The @scheme[render] procedure's arguments are the same as for
@scheme[delayed-block], but the result is @techlink{content} (i.e.,
a list of @techlink{elements}). Unlike @scheme[delayed-block], the
result of the @scheme[render] procedure's argument is remembered on
the first call for re-use for a particular resolve pass.

The @scheme[sizer] field is a procedure that produces a substitute
@techlink{element} for the delayed element for the purposes of
determining the delayed element's width (see @scheme[element-width]).

The @scheme[plain] field is a procedure that produces a substitute
@techlink{element} when needed before the @techlink{collect pass},
such as when @scheme[element->string] is used before the @tech{collect
pass}.

}


@defstruct[part-relative-element ([resolve (collect-info? . -> . list?)]
                                  [sizer (-> any/c)]
                                  [plain (-> any/c)])]{

Similar to @scheme[delayed-block], but the replacement
@techlink{content} is obtained in the @techlink{collect pass} by
calling the function in the @scheme[resolve] field.

The @scheme[resolve] function can call @scheme[collect-info-parents]
to obtain a list of @techlink{parts} that enclose the element,
starting with the nearest enclosing section. Functions like
@scheme[part-collected-info] and @scheme[collected-info-number] can
extract information like the part number.

}


@defstruct[(collect-element element) ([collect (collect-info . -> . any)])]{

Like @scheme[element], but the @scheme[collect] procedure is called
during the @techlink{collect pass}. The @scheme[collect] procedure
normally calls @scheme[collect-put!].

Unlike @scheme[delayed-element] or @scheme[part-relative-element], the
element remains intact (i.e., it is not replaced) by either the
@tech{collect pass} or @tech{resolve pass}.

}

@defstruct[(render-element element) ([render (any/c part? resolve-info? . -> . any)])]{

Like @scheme[delayed-element], but the @scheme[render] procedure is called
during the @techlink{render pass}.

If a @scheme[render-element] instance is serialized (such as when
saving collected info), it is reduced to a @scheme[element] instance.

}

@defstruct[with-attributes ([style any/c]
                            [assoc (listof (cons/c symbol? string?))])]{

Used for an @scheme[element]'s style to combine a base style with
arbitrary HTML attributes. When the @scheme[style] field is itself an
instance of @scheme[with-attributes], its content is automatically
flattened into the enclosing @scheme[with-attributes] when it is used
(when, e.g., rendering an @tech{element} or a styled @tech{paragraph}).}


@defstruct[collected-info ([number (listof (or/c false/c integer?))]
                           [parent (or/c false/c part?)]
                           [info any/c])]{

Computed for each part by the @techlink{collect pass}.

}


@defstruct[target-url ([addr path-string?]
                       [style any/c])]{

Used as a style for an @scheme[element]. The @scheme[style] at this
layer is a style for the hyperlink.}


@defstruct[image-file ([path (or/c path-string?
                                   (cons/c 'collects (listof bytes?)))]
                       [scale real?])]{

Used as a style for an @scheme[element] to inline an image. The
@scheme[path] field can be a result of
@scheme[path->main-collects-relative].

For Latex output, a @filepath{.gif} suffix on @scheme[path] is
replaced with a @filepath{.png} suffix (because animated GIFs can be
useful in HTML output, but Latex does not support GIFs). For HTML
output, a @filepath{.pdf} suffix on @scheme[path] is replaced with a
@filepath{.png} suffix (because PDF line drawings can be more
appropriate for Latex output, but HTML output needs bitmaps).}


@defproc[(block? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @scheme[paragraph],
@scheme[table], @scheme[itemization], @scheme[blockquote], or
@scheme[delayed-block], @scheme[#f] otherwise.

}


@defproc[(tag? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is acceptable as a link
@techlink{tag}, which is a list containing a symbol and either a
string, a @scheme[generated-tag] instance, or a list (of arbitrary
values).}


@defstruct[generated-tag ()]{

A placeholder for a tag to be generated during the @techlink{collect
 pass}. Use @scheme[tag-key] to convert a tag containing a
 @scheme[generated-tag] instance to one containing a string.

}


@defproc*[([(content->string (content list?)) string?]
           [(content->string (content list?) (renderer any/c) (p part?) (info resolve-info?)) string?])]{

Converts a list of @tech{elements} to a single string (essentially
rendering the content as ``plain text'').

If @scheme[p] and @scheme[info] arguments are not supplied, then a
pre-``collect'' substitute is obtained for @tech{delayed
elements}. Otherwise, the two arguments are used to force the
@tech{delayed element} (if it has not been forced already).}


@defproc*[([(element->string (element any/c)) string?]
           [(element->string (element any/c) (renderer any/c) (p part?) (info resolve-info?)) string?])]{

Like @scheme[content->string], but for a single @tech{element}.

}

@defproc[(element-width (element any/c)) exact-nonnegative-integer?]{

Returns the width in characters of the given @tech{element}.

}


@defproc[(block-width (e block?)) exact-nonnegative-integer?]{

Returns the width in characters of the given @tech{block}.}


@defstruct[collect-info ([ht any/c] [ext-ht any/c] [parts any/c] 
                         [tags any/c] [gen-prefix any/c] 
                         [relatives any/c] 
                         [parents (listof part?)])]{

Encapsulates information accumulated (or being accumulated) from the
@techlink{collect pass}. The fields are exposed, but not currently
intended for external use, except that @scheme[collect-info-parents]
is intended for external use.

}

@defstruct[resolve-info ([ci any/c] [delays any/c] [undef any/c])]{

Encapsulates information accumulated (or being accumulated) from the
@techlink{resolve pass}. The fields are exposed, but not currently
intended for external use.

}

@defproc[(info-key? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an @deftech{info key}: a list of
at least two elements whose first element is a symbol. The result is
@scheme[#f] otherwise.

For a list that is an info tag, the interpretation of the second
element of the list is effectively determined by the leading symbol,
which classifies the key. However, a @scheme[#f] value as the second
element has an extra meaning: collected information mapped by such
info keys is not propagated out of the part where it is collected;
that is, the information is available within the part and its
sub-parts, but not in ancestor or sibling parts.

Note that every @techlink{tag} is an info key.

}

@defproc[(collect-put! [ci collect-info?] [key info-key?] [val any/c])
         void?]{

Registers information in @scheme[ci]. This procedure should be called
only during the @techlink{collect pass}.

}

@defproc[(resolve-get [p (or/c part? false/c)] [ri resolve-info?] [key info-key?])
         any/c]{

Extract information during the @techlink{resolve pass} or
@techlink{render pass} for @scheme[p] from @scheme[ri], where the
information was previously registered during the @techlink{collect
pass}. See also @secref["passes"].

The result is @scheme[#f] if the no value for the given key is found.
Furthermore, the search failure is recorded for potential consistency
reporting, such as when @exec{setup-plt} is used to build
documentation.

}


@defproc[(resolve-get/ext? [p (or/c part? false/c)] [ri resolve-info?] [key info-key?])
         (values any/c boolean?)]{

Like @scheme[render-get], but returns a second value to indicate
whether the resulting information originated from an external source
(i.e., a different document).}


@defproc[(resolve-search [dep-key any/c][p (or/c part? false/c)] [ri resolve-info?] [key info-key?])
         void?]{

Like @scheme[resolve-get], but a shared @scheme[dep-key] groups
multiple searches as a single request for the purposes of consistency
reporting and dependency tracking. That is, a single success for the
same @scheme[dep-key] means that all of the failed attempts for the
same @scheme[dep-key] have been satisfied. However, for dependency
checking, such as when using @exec{setup-plt} to re-build
documentation, all attempts are recorded (in case external changes
mean that an earlier attempt would succeed next time).

}

@defproc[(resolve-get/tentative [p (or/c part? false/c)] [ri resolve-info?] [key info-key?])
         any/c]{

Like @scheme[resolve-search], but without dependency tracking. For
multi-document settings where dependencies are normally tracked, such
as when using @exec{setup-plt} to build documentation, this function
is suitable for use only for information within a single document.

}

@defproc[(resolve-get-keys [p (or/c part? false/c)]
                           [ri resolve-info?] 
                           [pred (info-key? . -> . any/c)])
         list?]{

Applies @scheme[pred] to each key mapped for @scheme[p] in
@scheme[ri], returning a list of all keys for which @scheme[pred]
returns a true value.

}

@defproc[(part-collected-info [p part?]
                              [ri resolve-info?])
         collected-info?]{

Returns the information collected for @scheme[p] as recorded within
@scheme[ri].

}

@defproc[(tag-key [t tag?] [ri resolve-info?]) tag?]{

Converts a @scheme[generated-tag] value with @scheme[t] to a string.

}
