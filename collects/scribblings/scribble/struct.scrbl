#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require["utils.ss"]

@title[#:tag "struct"]{Document Structures And Processing}

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
 the same as a @scheme[part], but it isn't numbered. There's no
 difference between a part and a full document; a particular source
 module just as easily defines a subsection (incorporated via
 @scheme[include-section]) as a document.

A @deftech{flow} is an instance of @scheme[flow]; it has a list of
 @techlink{flow elements}.

A @deftech{flow element} is either a @techlink{table}, an
 @techlink{itemization}, @techlink{blockquote}, @techlink{paragraph},
 or a @techlink{delayed flow element}.

@itemize{

       @item{A @deftech{table} is an instance of @scheme[table]; it
             has a list of list of @techlink{flows} with a particular
             style. In Latex output, each table cell is typeset as a
             single line.}

       @item{A @deftech{itemization} is an instance of @scheme[itemization];
             it has a list of @techlink{flows}.}

       @item{A @deftech{blockquote} is an instance of
             @scheme[blockquote]; it has list of flow elements that
             are indented according to a specified style.}

       @item{A @deftech{paragraph} is an instance of
             @scheme[paragraph]; it has a @deftech{content}, which is
             a list of @techlink{elements}:

             @itemize{

             @item{An @deftech{element} can be a string, one of a few
                   symbols, an instance of @scheme[element] (possibly
                   @scheme[link-element], etc.), a @techlink{delayed
                   element}, or anything else allowed by the current
                   renderer.

                   @itemize{

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

                   @item{A @deftech{delayed element} is an instance of
                         @scheme[delayed-element], which has a
                         procedure that is called in the
                         @techlink{resolve pass} of document
                         processing to obtain @defterm{content} (i.e.,
                         a list of @defterm{elements}).}

                   @item{An instance of @scheme[aux-element] is
                         excluded in the text of a link when it
                         appears in a referenced section name.}

             }}}}

       @item{A @deftech{delayed flow element} is an instance of
             @scheme[delayed-flow-element], which has a procedure that
             is called in the @techlink{resolve pass} of document
             processing to obtain a @defterm{flow element}.}

}

@; ------------------------------------------------------------------------

@section[#:tag "tags"]{Tags}

A @deftech{tag} is a list containing a symbol and a string. The symbol
 effectively identifies the type of the tag, such as @scheme['part]
 for a tag that links to a part, or @scheme['def] for a Scheme
 function definition.

A section can have a @deftech{tag prefix}, which is effectively
 prefixed onto the string part of each @scheme['part] and
 @scheme['tech] tag within the part for reference outside the part,
 including the tags in the @scheme[tags] field. Typically, a
 document's main part has a tag prefix that applies to the whole
 document; references to sections and defined terms within the
 document from other documents must include the prefix plus a
 separating @litchar{:}, while references within the same document
 omit the prefix. Part prefixes can be used within a document as well,
 to help disambiguate references within the document.

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
procedure associated with a @techlink{delayed flow element} or
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

The @scheme[style] field is normally either a symbol or a list of
symbols.  The currently recognized style symbols (alone or in a list)
are as follows:

@itemize{

 @item{@scheme['toc] --- sub-parts of the part are rendered on separate
       pages for multi-page HTML mode.}

 @item{@scheme['index] --- the part represents an index.}

 @item{@scheme['reveal] --- shows sub-parts when this part is
       displayed in a table-of-contents panel in HTML output (which
       normally shows only the top-level sections).}

 @item{@scheme['hidden] --- the part title is not shown in rendered output.}

}

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


@defstruct[flow ([paragraphs (listof flow-element?)])]{

A @techlink{flow} has a list of flow elements.

}

@defstruct[paragraph ([content list?])]{

A @techlink{paragraph} has a list of elements.

}

@defstruct[(styled-paragraph paragraph) ([style any/c])]{

The @scheme[style] is normally a string that corresponds to a CSS
class for HTML output.

}


@defstruct[table ([style any/c]
                  [flowss (listof (listof (or/c flow? (one-of/c 'cont))))])]{

A @techlink{table} has, roughly, a list of list of flows. A cell in
the table can span multiple columns by using @scheme['cont] instead of
a flow in the following columns (i.e., for all but the first in a set
of cells that contain a single flow).

}


@defstruct[itemization ([flows (listof flow?)])]{

A @techlink{itemization} has a list of flows.

}

@defstruct[blockquote ([style any/c]
                       [paragraphs (listof flow-element?)])]{

A @techlink{blockquote} has a style and a list of flow elements.  The
@scheme[style] field is normally a string that corresponds to a CSS
class for HTML output.

}

@defstruct[delayed-flow-element ([resolve (any/c part? resolve-info? . -> . flow-element?)])]{

The @scheme[resolve] procedure is called during the @techlink{resolve
pass} to obtain a normal flow element. The first argument to
@scheme[resolve] is the renderer.

}


@defstruct[element ([style any/c]
                    [content list?])]{

The @scheme[style] field is normally either

@itemize{

 @item{a string, which corresponds to a CSS class for HTML output;}

 @item{one of the symbols that all renderers recognize: @scheme['tt],
       @scheme['italic], @scheme['bold], @scheme['sf],
       @scheme['subscript], @scheme['superscript], or
       @scheme['hspace];}

 @item{an instance of @scheme[target-url] to generate a hyperlink; or}

 @item{an instance of @scheme[image-file] to support an inline image.}

}

The @scheme[content] field is a list of @techlink{elements}.

}


@defstruct[(target-element element) ([tag tag?])]{

Declares the content as a hyperlink target for @scheme[tag].

}


@defstruct[(toc-target-element target-element) ()]{

Like @scheme[target-element], the content is also a kind of section
label to be shown in the ``on this page'' table for HTML output.

}


@defstruct[(link-element element) ([tag any/c])]{

Hyperlinks the content to @scheme[tag].

}


@defstruct[(index-element element) ([tag tag?]
                                    [plain-seq (listof string?)]
                                    [entry-seq list?])]{

The @scheme[plain-seq] specifies the keys for sorting, where the first
 element is the main key, the second is a sub-key, etc. The
 @scheme[entry-seq] list must have the same length, and it provides
 the form of each key to render in the final document. See also
 @scheme[index].

}


@defstruct[(aux-element element) ()]{

Instances of this structure type are intended for use in titles, where
 the auxiliary part of the title can be omitted in hyperlinks. See,
 for example, @scheme[secref].

}

@defstruct[delayed-element ([resolve (any/c part? resolve-info? . -> . list?)]
                            [sizer (-> any/c)]
                            [plain (-> any/c)])]{

The @scheme[render] procedure's arguments are the same as for
 @scheme[delayed-flow-element]. Unlike @scheme[delayed-flow-element],
 the result of the @scheme[render] procedure's argument is remembered
 on the first call.

The @scheme[sizer] field is a procedure that produces a substitute
 element for the delayed element for the purposes of determining the
 element's width (see @scheme[element-width]).

The @scheme[plain] field is a procedure that produces a substitute for
 the element when needed before the @techlink{collect pass}.

}


@defstruct[(collect-element element) ([collect (collect-info . -> . any)])]{

Like @scheme[element], but the @scheme[collect] procedure is called
during the @techlink{collect pass}. The @scheme[collect] procedure
normally calls @scheme[collect-put!].

}


@defstruct[collected-info ([number (listof (or/c false/c integer?))]
                           [parent (or/c false/c part?)]
                           [info any/c])]{

Computed for each part by the @techlink{collect pass}.

}


@defstruct[target-url ([addr string?])]{

Used as a style for an @scheme[element].}


@defstruct[image-file ([path path-string?])]{

Used as a style for an @scheme[element].}


@defproc[(flow-element? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @scheme[paragraph],
@scheme[table], @scheme[itemization], @scheme[blockquote], or
@scheme[delayed-flow-element], @scheme[#f] otherwise.

}


@defproc[(tag? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is acceptable as a link tag, which
is a list containing a symbol and either a string or a
@scheme[generated-tag] instance.}


@defstruct[generated-tag ()]{

A placeholder for a tag to be generated during the @scheme{collect
 pass}. Use @scheme[tag-key] to convert a tag containing a
 @scheme[generated-tag] instance to one containing a string.

}


@defproc*[([(content->string (content list?)) string?]
           [(content->string (content list?) (p part?) (info resolve-info?)) string?])]{

Converts a list of elements to a single string (essentially
rendering the content as ``plain text'').

If @scheme[p] and @scheme[info] arguments are not supplied, then a
pre-``collect'' substitute is obtained for delayed
elements. Otherwise, the two arguments are used to force the delayed
element (if it has not been forced already).}


@defproc*[([(element->string (element any/c)) string?]
           [(element->string (element any/c) (p part?) (info resolve-info?)) string?])]{

Like @scheme[content->string], but for a single element.
}

@defproc[(element-width (element any/c)) nonnegative-exact-integer?]{

Returns the width in characters of the given element.}


@defproc[(flow-element-width (e flow-element?)) nonnegative-exact-integer?]{

Returns the width in characters of the given flow element.}

@defstruct[collect-info ([ht any/c] [ext-ht any/c] [parts any/c] [tags any/c] [gen-prefix any/c])]{

Encapsulates information accumulated (or being accumulated) from the
@techlink{collect pass}. The fields are exposed, but not currently
intended for external use.

}

@defstruct[resolve-info ([ci any/c] [delays any/c] [undef any/c])]{

Encapsulates information accumulated (or being accumulated) from the
@techlink{resolve pass}. The fields are exposed, but not currently
intended for external use.

}

@defproc[(collect-put! [ci collect-info?] [key any/c] [val any/c])
         void?]{

Registers information in @scheme[ci]. This procedure should be called
only during the @techlink{collect pass}.

}

@defproc[(resolve-get [ri resolve-info?] [key any/c])
         void?]{

Extract information during the @techlink{resolve pass} or
@techlink{render pass} from @scheme[ri], where the information was
previously registered during the @techlink{collect pass}. See also
@secref["passes"].

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
