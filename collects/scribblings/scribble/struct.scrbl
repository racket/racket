#reader"../docreader.ss"
@require["../manual.ss"]
@require["utils.ss"]

@title[#:tag "struct"]{Document Structures}

A single document is reprsented as a @defterm{part}:

@itemize{

 @item{A @defterm{part} is an instance of @scheme[part]; it has a
       title @defterm{content}, an initial @defterm{flow}, and a list
       of subsection @defterm{part}s.  After the ``collect'' phase of
       rendering, it also has @defterm{collected info}. An
       @scheme[unnumbered-part] is the same as a @scheme[part], but it
       isn't numbered.}

 @item{A @defterm{flow} is an instance of @scheme[flow]; it has a list
       of @defterm{flow element}s.}

 @item{A @defterm{flow element} is either a @defterm{table}, an
       @defterm{itemization}, @defterm{paragraph}, or a
       @defterm{delayed flow element}.

       @itemize{

       @item{A @defterm{table} is an instance of @scheme[table]; it has a
             list of list of @defterm{flow}s with a particular style.}

       @item{A @defterm{itemization} is an instance of @scheme[itemization];
             it has a list of flows.}

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
                         document verbatim.}

                   @item{A symbol element is either @scheme['mdash],
                         @scheme['ndash], @scheme['ldquo],
                         @scheme['lsquo], @scheme['rsquo], or
                         @scheme['rarr]; it is drawn as the
                         corresponding HTML entity.}

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
                         @scheme[link-element]s.}

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

@defstruct[part ([tag (or/c false/c tag?)]
                 [title-content (or/c false/c list?)]
                 [collected-info (or/c false/c collected-info?)]
                 [flow flow?]
                 [parts (listof part?)])]{

}


@defstruct[(unnumbered-part part) ()]{

}

@defstruct[flow ([paragraphs (listof flow-element?)])]{

}

@defstruct[paragraph ([content list?])]{

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

@defstruct[element ([style any/c]
                    [content list?])]{

}

@defstruct[(target-element element) ([tag tag?])]{

}

@defstruct[(link-element element) ([tag any/c]
                                   [complain-if-fail? boolean?])]{

}


@defstruct[(index-element element) ([tag tag?]
                                    [plain-seq (listof string?)]
                                    [entry-seq list?])]{

}

@defstruct[delayed-element ([render (any/c part? any/c . -> . list?)])]{

The @scheme[render] procedure's arguments are the same as for
@scheme[delayed-flow-element]. Unlike @scheme[delayed-flow-element],
the result of the @scheme[render] procedure's argument is remembered
on the first call. Furthemore, the element can be marshelled (e.g.,
for an index entry or a section-title entry) only if it has been
rendered first.

}

@defstruct[collected-info ([number (listof (or/c false/c integer?))]
                           [parent (or/c false/c part?)]
                           [info any/c])]{

}

@defproc[(flow-element? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @scheme[paragraph],
@scheme[table], @scheme[itemization], or
@scheme[delayed-flow-element], @scheme[#f] otherwise.

}

@defproc[(tag? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is acceptable as a link tag,
@scheme[#f], otherwise. Currently, an acceptable tag is either a
string or a list containing a symbol and a string.

}

@defproc[(content->string (content list?)) string?]{

Converts a list of elements to a single string (essentially
rendering the content as ``plain text'').

}

