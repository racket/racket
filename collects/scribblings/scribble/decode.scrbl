#lang scribble/doc
@(require scribble/manual
          "utils.ss")

@title[#:tag "decode"]{Decoding Text}

@defmodule[scribble/decode]{The @schememodname[scribble/decode]
library helps you write document content in a natural way---more like
plain text, except for @litchar["@"] escapes.  Roughly, it processes a
stream of strings to produces instances of the
@schememodname[scribble/struct] datatypes (see @secref["struct"]).}

At the @tech{flow} level, decoding recognizes a blank line as a
@tech{paragraph} separator. Blocks and paragraphs without blank lines
in between are collected into a @tech{compound paragraph}.

At the @tech{content} level, decoding makes just a few
special text conversions:

@itemize[

 @item{@litchar{---}: converted to @scheme['mdash], which the HTML render
       outputs as an en-dash surrounded by space (so don't put spaces around
       @litchar{---} in a document)}

 @item{@litchar{--}: converted to @scheme['ndash]}

 @item{@litchar{``}: converted to @scheme['ldquo], which is fancy open quotes: ``}

 @item{@litchar{''}: converted to @scheme['rdquo], which is fancy closing quotes: ''}

 @item{@litchar{'}: converted to @scheme['rsquo], which is a fancy apostrophe: '}

]

Some functions @deftech{decode} a sequence of @scheme[_pre-flow] or
@scheme[_pre-content] arguments using @scheme[decode-flow] or
@scheme[decode-content], respectively. For example, the @scheme[bold]
function accepts any number of @scheme[_pre-content] arguments, so
that in

@verbatim[#:indent 2]|{@bold{``apple''}}|

the @litchar{``apple''} argument is decoded to use fancy quotes, and
then it is bolded.


@defproc[(pre-content? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @deftech{pre-content} value: a
string or other non-list @scheme[content], or a @scheme[splice]
containing a list of @tech{pre-content} values; otherwise returns
@scheme[#f].

Pre-content is decoded into @tech{content} by functions like
@scheme[decode-content] and @scheme[decode-paragraph].}


@defproc[(pre-flow? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @deftech{pre-flow} value: a
string or other non-list @scheme[content], a @scheme[block],
@|void-const|, or a @scheme[splice] containing a list of
@tech{pre-flow} values; otherwise returns @scheme[#f].

Pre-flow is decoded into a @tech{flow} (i.e., a list of @tech{blocks})
by functions like @scheme[decode-flow].}


@defproc[(pre-part? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @deftech{pre-part} value: a
string or other non-list @tech{content}, a @tech{block}, a
@scheme[part], a @scheme[title-decl], a @scheme[part-start], a
@scheme[part-index-decl], a @scheme[part-collect-decl], a
@scheme[part-tag-decl], @|void-const|, or a @scheme[splice] containing
a list of @tech{pre-part} values; otherwise returns @scheme[#f].

A pre-part sequence is decoded into a @scheme[part] by functions like
@scheme[decode] and @scheme[decode-part].}


@defproc[(decode [lst (listof pre-part?)]) part?]{

Decodes a document, producing a part. In @scheme[lst], instances of
@scheme[splice] are inlined into the list. An instance of
@scheme[title-decl] supplies the title for the part, plus tag, style
and version information. Instances of @scheme[part-index-decl] (that
precede any sub-part) add index entries that point to the
section. Instances of @scheme[part-collect-decl] add elements to the
part that are used only during the @techlink{collect pass}. Instances
of @scheme[part-tag-decl] add hyperlink tags to the section
title. Instances of @scheme[part-start] at level 0 trigger sub-part
parsing. Instances of @scheme[section] trigger are used as-is as
subsections, and instances of @scheme[paragraph] and other
flow-element datatypes are used as-is in the enclosing flow.}


@defproc[(decode-part [lst (listof pre-part?)]
                      [tags (listof string?)]
                      [title (or/c #f list?)]
                      [depth exact-nonnegative-integer?])
         part?]{

Like @scheme[decode], but given a list of tag string for the part, a
title (if @scheme[#f], then a @scheme[title-decl] instance is used if
found), and a depth for @scheme[part-start]s to trigger sub-part
parsing.

}

@defproc[(decode-flow [lst (listof pre-flow?)]) flow?]{

Decodes a flow. A sequence of two or more newlines separated only by
whitespace counts is parsed as a paragraph separator. In @scheme[lst],
instances of @scheme[splice] are inlined into the list. Instances of
@scheme[paragraph] and other flow-element datatypes are used as-is in
the enclosing flow.

}

@defproc[(decode-compound-paragraph [lst (listof pre-flow?)]) block?]{

Decodes a compound paragraph. If the compound paragraph contains a
single block, the block is returned without a
@scheme[compound-paragraph] wrapper.

}

@defproc[(decode-paragraph [lst (listof pre-content?)]) paragraph?]{

Decodes a paragraph.

}

@defproc[(decode-content [lst (listof pre-content?)]) list?]{

Decodes @tech{content}.

}

@defproc[(decode-elements [lst (listof pre-content?)]) list?]{

An alias for @scheme[decode-content].
}

@defproc[(decode-string [s string?]) (listof content?)]{

Decodes a single string to produce @tech{content}.

}


@defproc[(whitespace? [s string?]) boolean?]{

Returns @scheme[#t] if @scheme[s] contains only whitespace, @scheme[#f]
otherwise.

}

@defstruct[title-decl ([tag-prefix (or/c #f string?)]
                       [tags (listof string?)]
                       [version (or/c string? #f)]
                       [style any/c]
                       [content content?])]{

See @scheme[decode] and @scheme[decode-part]. The @scheme[tag-prefix]
and @scheme[style] fields are propagated to the resulting
@scheme[part].

}

@defstruct[part-start ([depth integer?]
                       [tag-prefix (or/c #f string?)]
                       [tags (listof string?)]
                       [style any/c]
                       [title content?])]{

Like @scheme[title-decl], but for a sub-part.  See @scheme[decode] and
@scheme[decode-part].

}

@defstruct[part-index-decl ([plain-seq (listof string?)]
                            [entry-seq list?])]{

See @scheme[decode]. The two fields are as for @scheme[index-element].

}

@defstruct[part-collect-decl ([element element?])]{

See @scheme[decode].

}

@defstruct[part-tag-decl ([tag tag?])]{

See @scheme[decode].

}

@defstruct[splice ([run list?])]{

See @scheme[decode], @scheme[decode-part], and @scheme[decode-flow].

}

@defproc[(clean-up-index-string [str string?]) string?]{

Trims leading and trailing whitespace, and converts non-empty
sequences of whitespace to a single space character.}

