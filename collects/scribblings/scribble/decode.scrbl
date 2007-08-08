#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require["utils.ss"]

@title[#:tag "decode"]{Text Decoder}

The @file{decode.ss} library helps you write document content in a
natural way---more like plain text, except for @elem["@"] escapes.
Roughly, it processes a stream of strings to produces instances of the
@file{struct.ss} datatypes (see @secref["struct"]).

At the flow level, decoding recognizes a blank line as a paragraph
separator. At the paragraph-content level, decoding makes just a few
special text conversions:

@itemize{

 @item{@litchar{---}: converted to @scheme['mdash], which the HTML render
       outputs as an en-dash surrounded by space (so don't put spaces around
       @litchar{---} in a document)}

 @item{@litchar{--}: converted to @scheme['ndash]}

 @item{@litchar{``}: converted to @scheme['ldquo], which is fancy open quotes: ``}

 @item{@litchar{''}: converted to @scheme['rdquo], which is fancy closing quotes: ''}

 @item{@litchar{'}: converted to @scheme['rsquo], which is a fancy apostrophe: '}

}

@defproc[(decode [lst list?]) part?]{

Decodes a document, producing a part. In @scheme[lst], instances of
@scheme[splice] are inlined into the list. An instance of
@scheme[title-decl] supplies the title for the part. Instances of
@scheme[index-section-decl] (that preceed any sub-part) add index
entries that point to the section. Instances of @scheme[part-start] at
level 0 trigger sub-part parsing. Instances of @scheme[section]
trigger are used as-is as subsections, and instances of
@scheme[paragraph] and other flow-element datatypes are used as-is in
the enclosing flow.

}

@defproc[(decode-part [lst list?]
                      [tag string?]
                      [title (or/c false/c list?)]
                      [depth excat-nonnegative-integer?])
         part?]{

Like @scheme[decode], but given a tag for the section, a title (if
@scheme[#f], then a @scheme[title-decl] instance is used if found),
and a depth for @scheme[part-start]s to trigger sub-part parsing.

}

@defproc[(decode-flow [lst list?]) (listof flow-element?)]{

Decodes a flow. A sequence of two or more newlines separated only by
whitespace counts is parsed as a paragraph separator. In @scheme[lst],
instances of @scheme[splice] are inlined into the list. Instances of
@scheme[paragraph] and other flow-element datatypes are used as-is in
the enclosing flow.

}

@defproc[(decode-paragraph [lst list?]) paragraph?]{

Decodes a paragraph.

}

@defproc[(decode-content [lst list?]) list?]{

Decodes a sequence of elements.

}

@defproc[(decode-string [s string?]) list?]{

Decodes a single string to produce a list of elements.

}

@defproc[(whitespace? [s string?]) boolean?]{

Returns @scheme[#t] if @scheme[s] contains only whitespace, @scheme[#f]
otherwise.

}

@defstruct[title-decl ([tag any/c]
                       [content list?])]{

See @scheme[decode] and @scheme[decode-part].

}

@defstruct[part-start ([depth integer?]
                       [tag (or/c false/c string?)]
                       [title list?])]{

See @scheme[decode] and @scheme[decode-part].

}

@defstruct[part-index-decl ([plain-seq (listof string?)]
                            [content-seq list?])]{

See @scheme[decode]. The two fields are as for @scheme[index-element].

}

@defstruct[splice ([run list?])]{

See @scheme[decode], @scheme[decode-part], and @scheme[decode-flow].

}

