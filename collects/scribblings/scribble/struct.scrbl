#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require["utils.ss"]

@title[#:tag "struct"]{Document Structures}

A single document is represented as a @defterm{part}:

@itemize{

 @item{A @defterm{part} is an instance of @scheme[part]; it has a list
       of @defterm{tags} used as link targets, a title
       @defterm{content}, a list of @defterm{elements} that supply
       information during the ``collect'' phase but are not rendered,
       an initial @defterm{flow}, and a list of subsection
       @defterm{part}s.  After the ``collect'' phase of rendering, it
       also has @defterm{collected info}. A @scheme[styled-part]
       includes an extra style flag. An @scheme[unnumbered-part] is
       the same as a @scheme[styled-part], but it isn't numbered.}

 @item{A @defterm{flow} is an instance of @scheme[flow]; it has a list
       of @defterm{flow element}s.}

 @item{A @defterm{flow element} is either a @defterm{table}, an
       @defterm{itemization}, @defterm{blockquote}, @defterm{paragraph}, or a
       @defterm{delayed flow element}.

       @itemize{

       @item{A @defterm{table} is an instance of @scheme[table]; it has a
             list of list of @defterm{flow}s with a particular style.}

       @item{A @defterm{itemization} is an instance of @scheme[itemization];
             it has a list of flows.}

       @item{A @defterm{blockquote} is an instance of
             @scheme[blockquote]; it has list of flow elements that
             are indented according to a specified style.}

       @item{A @defterm{paragraph} is an instance of @scheme[paragraph]; it
             has a list of @defterm{element}s.

             @itemize{

             @item{An element can be a string, one of a few symbols, an instance of
                   @scheme[element] (possibly @scheme[link-element],
                   @scheme[target-element], or
                   @scheme[index-element]), a @defterm{delayed
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
                         drawn as the corresponding HTML entity.}

                   @item{An instance of @scheme[element] has a list of
                         @defterm{element}s plus a style. The style's
                         interpretation depends on the rendrer; it can
                         be one of a few special symbols that are
                         recognized by all renderers: @scheme['tt],
                         @scheme['italic], @scheme['bold],
                         @scheme['sf], @scheme['subscript],
                         @scheme['superscript], or @scheme['hspace].
                         A string corresponds to a CSS class, LaTeX
                         macro, or something else renderer-specific.
                         Instances of @scheme[target-url] and
                         @scheme[image-file] may also be supported.}

                   @item{An instance of @scheme[link-element] has a
                         @defterm{tag} for the target of the link.}

                   @item{An instance of @scheme[target-element] has a
                         @defterm{tag} to be referenced by
                         @scheme[link-element]s. An instance of the
                         subtype @scheme[toc-target-element] is
                         treated like a kind of section label, to be
                         shown in the ``on this page'' table for HTML
                         output.}

                   @item{An instance of @scheme[index-element] has a
                         @defterm{tag} (as a target), a list of
                         strings for the keywords (for sorting and
                         search), and a list of @defterm{element}s to
                         appear in the end-of-document index.}

                   @item{A @defterm{delayed element} is an instance of
                         @scheme[delayed-element], which has a
                         procedure that produces a
                         @defterm{element}. The ``collect'' phase of
                         rendering ignores delayed flow elements.}

                   @item{An instance of @scheme[aux-element] is
                         excluded in the text of a link when it
                         appears in a referenced section name.}

             }}}}

       @item{A @defterm{delayed flow element} is an instance of
             @scheme[delayed-flow-element], which has a procedure that
             produces a @defterm{flow element}. The ``collect'' phase
             of rendering ignores delayed flow elements.}

       }}

 @item{The @defterm{collected info} of a part includes its number, its
       parent part (or @scheme[#f]), and information about link
       targets and index entries within the part.}

 @item{A @defterm{tag} is eiter a string or a list containing a symbol
       and a string.}

}

Note that there's no difference between a part and a full document. A
particular source module just as easily defines a subsection
(incoprated via @scheme[include-section]) as a document.

@defstruct[part ([tags (listof tag?)]
                 [title-content (or/c false/c list?)]
                 [collected-info (or/c false/c collected-info?)]
                 [to-collect list?]
                 [flow flow?]
                 [parts (listof part?)])]{

Each element of @scheme[tags] is actually wrapped as @scheme[`(part
,_tag)] as a target for links; functions like @scheme[seclink]
similarly insert the @scheme[`(part ,_tag)] wrapper.

}


@defstruct[(styled-part part) ([style any/c])]{

The currently recognized values for @scheme[style] are as follows:

@itemize{

 @item{@scheme['toc] --- sub-parts of the part are rendered on separate
       pages for multi-page HTML mode.}

 @item{@scheme['index] --- the part represents an index.}

}

}

@defstruct[(unnumbered-part styled-part) ()]{

Although a section number is computed for an ``unnumbered'' section
during the ``collect'' phase, the number is not rendered.

}

@defstruct[flow ([paragraphs (listof flow-element?)])]{

}

@defstruct[paragraph ([content list?])]{

}

@defstruct[(styled-paragraph paragraph) ([style any/c])]{
}


@defstruct[table ([style any/c]
                  [flowss (listof (listof flow?))])]{

}


@defstruct[delayed-flow-element ([render (any/c part? any/c . -> . flow-element?)])]{

For the @scheme[render] procedure, the first argument corresponds to
the rendering context, the second to the immediately enclosing
section, and the last argument correspond to global information
(possibly psanning multiple documents).

}


@defstruct[itemization ([flows (listof flow?)])]{

}

@defstruct[blockquote ([style any/c]
                       [flows (listof flow-element?)])]{

}

@defstruct[element ([style any/c]
                    [content list?])]{

}

@defstruct[(target-element element) ([tag tag?])]{

}

@defstruct[(toc-target-element target-element) ()]{

}

@defstruct[(link-element element) ([tag any/c]
                                   [complain-if-fail? boolean?])]{

}


@defstruct[(index-element element) ([tag tag?]
                                    [plain-seq (listof string?)]
                                    [entry-seq list?])]{

The @scheme[plain-seq] specifies the keys for sorting, where the first
element is the main key, the second is a sub-key, etc. The
@scheme[entry-seq] list must have the same length, and it provides the
form of each key to render in the final document. See also
@scheme[index].

}

@defstruct[(aux-element element) ()]{

Instances of this structure type are intended for use in titles, where
the auxiliary part of the title can be omitted in hyperlinks. See, for
example, @scheme[secref].

}

@defstruct[delayed-element ([render (any/c part? any/c . -> . list?)]
                            [sizer (-> any/c)]
                            [plain (-> any/c)])]{

The @scheme[render] procedure's arguments are the same as for
@scheme[delayed-flow-element]. Unlike @scheme[delayed-flow-element],
the result of the @scheme[render] procedure's argument is remembered
on the first call. Furthemore, the element can be marshelled (e.g.,
for an index entry or a section-title entry) only if it has been
rendered first.

The @scheme[sizer] field is a procedure that produces a substitute
element for the delayed element for the purposes of determine the
element's width (see @scheme[element-width]).

The @scheme[plain] field is a procedure that produces a substitute for
the element when needed before the ``collect'' phase.

}

@defstruct[collected-info ([number (listof (or/c false/c integer?))]
                           [parent (or/c false/c part?)]
                           [info any/c])]{

Computed for each part by the ``collect'' phase.

}

@defproc[(flow-element? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @scheme[paragraph],
@scheme[table], @scheme[itemization], @scheme[blockquote], or
@scheme[delayed-flow-element], @scheme[#f] otherwise.

}


@defproc[(tag? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is acceptable as a link tag,
@scheme[#f], otherwise. Currently, an acceptable tag is either a
string or a list containing a symbol and a string.}


@defproc*[([(content->string (content list?)) string?]
           [(content->string (content list?) (p part?) (info any/c)) string?])]{

Converts a list of elements to a single string (essentially
rendering the content as ``plain text'').

If @scheme[p] and @scheme[info] arguments are not supplied, then a
pre-``collect'' substitute is obtained for delayed
elements. Otherwise, the two arguments are used to force the delayed
element (if it has not been forced already).}


@defproc*[([(element->string (element any/c)) string?]
           [(element->string (element any/c) (p part?) (info any/c)) string?])]{

Like @scheme[content->string], but for a single element.
}
