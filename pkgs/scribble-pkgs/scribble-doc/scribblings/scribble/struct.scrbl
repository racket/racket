#lang scribble/manual
@(require (except-in "utils.rkt"
                     make-part make-paragraph make-table make-itemization make-compound-paragraph
                     make-element make-toc-element make-target-element make-toc-target-element
                     make-page-target-element make-redirect-target-element make-link-element
                     make-index-element
                     make-target-url target-url struct:target-url target-url? target-url-addr
                     deserialize-info:target-url-v0
                     toc-element-toc-content part-title-content paragraph-content 
                     element? element-content element-style)
          (for-label scribble/manual-struct
                     scribble/struct
                     setup/main-collects))

@(define (compat) @italic{For backward compatibility.})
@(define-syntax-rule (compat/comp id)
   @elem{@compat[] Compared to the normal constructor for @racket[id]})

@title[#:tag "struct"]{Compatibility Structures And Processing}

@defmodule[scribble/struct]{
The @racket[scribble/struct] compatibility library mostly re-exports
@racket[scribble/core], but using some different names (e.g.,
@racket[blockquote] instead of @racket[nested-flow]).}

The following structure types and functions are re-exported directly:

@racketblock[collect-info resolve-info tag? block?
             delayed-block collected-info delayed-element ; delayed-element-content delayed-block-blocks current-serialize-resolve-info
             part-relative-element collect-info-parents ; part-relative-element-content delayed-index-desc 
             collect-element render-element generated-tag ; generate-tag current-tag-prefixes add-current-tag-prefix
             tag-key content->string element->string ; strip-aux
             block-width element-width
             info-key? part-collected-info collect-put!
             resolve-get resolve-get/tentative resolve-get/ext?
             resolve-search resolve-get-keys]

The following structure types are re-exported, but the constructors and some selectors
are replaced as documented further below:

@racketblock[part paragraph table itemization compound-paragraph
             element toc-element target-element toc-target-element
             page-target-element redirect-target-element link-element
             index-element]

Several additional compatibility functions and structure types are
also exported.

@defproc[(make-part [tag-prefix (or/c false/c string?)]
                    [tags (listof tag?)]
                    [title-content (or/c false/c list?)]
                    [style any/c]
                    [to-collect list?]
                    [blocks (listof block?)]
                    [parts (listof part?)])
         part?]{

@compat/comp[part], parses @racket[style] to convert old formats to
the current one. Also, if @racket[title-content] is a list with a
single item, the item by itself is stored in the resulting
@racket[part].}

@defproc[(part-flow [p part?]) (listof block?)]{

@compat[] An alias for @racket[part-blocks].}

@defproc[(part-title-content [p part?]) list?]{

@compat[] Like the normal selector, but if the result would not be a list, it is 
coerced to one.}

@deftogether[(
@defproc[(make-versioned-part [tag-prefix (or/c false/c string?)]
                               [tags (listof tag?)]
                               [title-content (or/c false/c list?)]
                               [style any/c]
                               [to-collect list?]
                               [blocks (listof block?)]
                               [parts (listof part?)]
                               [version string?])
         part?]
@defproc[(versioned-part? [v any/c]) boolean?]
)]{

@compat[] Like @racket[make-part], but adds a the
@racket[document-version] @tech{style property} using the given
@racket[version]. The @racket[versioned-part?] predicate recognizes a
@racket[part] with a @racket[document-version] property.}

@deftogether[(
@defproc[(make-unnumbered-part [tag-prefix (or/c false/c string?)]
                               [tags (listof tag?)]
                               [title-content (or/c false/c list?)]
                               [style any/c]
                               [to-collect list?]
                               [blocks (listof block?)]
                               [parts (listof part?)])
         part?]
@defproc[(unnumbered-part? [v any/c]) boolean?]
)]{

@compat[] Like @racket[make-part], but adds the @racket['unnumbered]
@tech{style property}. The @racket[unnumbered-part?] predicate
recognizes a @racket[part] with the @racket['unnumbered] property.}


@defproc[(make-paragraph [content list?]) paragraph?]{

@compat/comp[paragraph], omits a style argument. Also, if
@racket[content] is a list containing a single item, the item by
itself is stored in the resulting @racket[paragraph].}

@defproc[(paragraph-content [p paragraph?]) list?]{

@compat[] Like the normal selector, but if the result would not be a list, it is 
coerced to one.
}


@deftogether[(
@defproc[(make-styled-paragraph [content list?] [style any/c]) paragraph?]
@defproc[(styled-paragraph? [v any/c]) boolean?]
@defproc[(styled-paragraph-style [p paragraph?]) style?]
)]{

@compat/comp[paragraph], parses @racket[style] to convert old formats
to the current one. The @racket[styled-paragraph?] predicate and
@racket[styled-paragraph-style] accessor are aliases for
@racket[paragraph?] and @racket[paragraph-style].}


@deftogether[(
@defproc[(make-omitable-paragraph [content list?]) paragraph?]
@defproc[(omitable-paragraph? [v any/c]) boolean?]
)]{

@compat[] Like @racket[make-paragraph], but adds the
@racket['omitable] @tech{style property}. The
@racket[omitable-paragraph?] predicate checks for a paragraph with the
property.}


@defproc[(make-table [style any/c]
                     [blocksss (listof (listof (or/c (listof block?) (one-of/c 'cont))))])
         table?]{

@compat/comp[table], the style is converted, and each cell has a list
of blocks instead of a single block. If any such list has multiple
blocks, they are combined into a @racket[nested-flow].}

@defproc[(table-flowss [table table?]) 
         (listof (listof (or/c (listof block?) (one-of/c 'cont))))]{

@compat[] Like @racket[table-blockss], but adds a list wrapper to be
consistent with @racket[make-table].}

@defproc[(make-itemization [blockss (listof (listof block?))]) itemization?]{

@compat/comp[itemization], omits a style argument.}

@deftogether[(
@defproc[(make-styled-itemization [style any/c]
                                  [blockss (listof (listof block?))]) itemization?]
@defproc[(styled-itemization? [v any/c]) boolean?]
@defproc[(styled-itemization-style [i itemization?]) style?]
)]{

@compat/comp[itemization], parses @racket[style] to convert old
formats to the current one. The @racket[styled-itemization?] predicate
is an alias for @racket[itemization?], and
@racket[styled-itemization-style] is an alias for
@racket[itemization-style].}

@defproc[(make-blockquote [style any/c] [blocks (listof block?)])
         nested-flow?]{

@compat[] Like @racket[make-nested-flow], but @racket[style] is
parsed to the current format.}


@deftogether[(
@defproc[(make-auxiliary-table [style any/c]
                               [blocksss (listof (listof (or/c (listof block?) (one-of/c 'cont))))])
         table?]
@defproc[(auxiliary-table? [v any/c]) boolean?]
)]{

@compat[] Like @racket[make-table], but adds the @racket['aux]
@tech{style property}.  The @racket[auxiliary-table?] predicate recognizes
tables with the @racket['aux] property.}


@defproc[(make-compound-paragraph [style any/c]
                                  [blocks (listof block?)])
         compound-paragraph?]{

@compat/comp[compound-paragraph], parses @racket[style] to convert old
formats to the current one.}

@deftogether[(
@defproc[(make-element [style any/c] [content list?]) element?]
@defproc[(make-toc-element [style any/c] [content list?] [toc-content list?]) toc-element?]
@defproc[(make-target-element [style any/c] [content list?] [tag tag?]) target-element?]
@defproc[(make-toc-target-element [style any/c] [content list?] [tag tag?]) toc-target-element?]
@defproc[(make-page-target-element [style any/c] [content list?] [tag tag?]) page-target-element?]
@defproc[(make-redirect-target-element [style any/c] [content list?] [tag tag?]
                                       [alt-path path-string?] [alt-anchor string?]) redirect-target-element?]
@defproc[(make-link-element [style any/c] [content list?] [tag tag?]) link-element?]
@defproc[(make-index-element [style any/c] [content list?] [tag tag?]
                             [plain-seq (and/c pair? (listof string?))]
                             [entry-seq list?] [desc any/c]) index-element?]
)]{

@compat[] Compared to the normal constructors, parses @racket[style] to convert old
formats to the current one.}


@deftogether[(
@defproc[(element? [v any/c]) boolean?]
@defproc[(element-content [e element?]) list?]
@defproc[(element-style [e element?]) element-style?]
)]{

@compat[] A @tech{content} list is treated as an element by these functions,
and the result of @racket[element-content] is always a list.}


@defproc[(make-aux-element [style any/c] [content list?]) element?]{

@compat[] Like @racket[make-element], but adds the @racket['aux]
@tech{style property}.}


@defproc[(make-hover-element [style any/c] [content list?] [text string?]) element?]{

@compat[] Like @racket[make-element], but adds @racket[hover-property]
containing @racket[text] to the element's style.}


@defproc[(make-script-element [style any/c] [content list?] [type string?]
                              [script (or/c path-string? (listof string?))]) element?]{

@compat[] Like @racket[make-element], but adds @racket[script-property]
containing @racket[type] and @racket[script] to the element's style.}


@defstruct[with-attributes ([style any/c]
                            [assoc (listof (cons/c symbol? string?))])]{

@compat[] Used for an @racket[element]'s style to combine a base style with
arbitrary HTML attributes. When the @racket[style] field is itself an
instance of @racket[with-attributes], its content is automatically
flattened into the enclosing @racket[with-attributes] when it is used
(when, e.g., rendering an @racket[element] or @racket[paragraph]).}


@defstruct[target-url ([addr path-string?]
                       [style any/c])]{

@compat[] Used as a style for an @racket[element]. The @racket[style] at this
layer is a style for the hyperlink.}


@defstruct[image-file ([path (or/c path-string?
                                   (cons/c 'collects (listof bytes?)))]
                       [scale real?])]{

@compat[] Used as a style for an @racket[element] to inline an image. The
@racket[path] field can be a result of
@racket[path->main-collects-relative].}




@defproc*[([(element->string (element content?)) string?]
           [(element->string (element content?) (renderer any/c) (p part?) (info resolve-info?)) string?])]{

@compat[] An alias for @racket[content->string].

}
